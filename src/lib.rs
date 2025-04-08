use {
    std::{
        borrow::Cow,
        env,
        fmt,
        iter,
        num::NonZeroU8,
        path::{
            Path,
            PathBuf,
        },
        str::FromStr,
    },
    async_proto::Protocol,
    dir_lock::DirLock,
    directories::UserDirs,
    itertools::{
        Itertools as _,
        Position,
    },
    lazy_regex::{
        regex_is_match,
        regex_captures,
    },
    serde::{
        Deserialize,
        Serialize,
        de::DeserializeOwned,
    },
    serde_plain::derive_deserialize_from_fromstr,
    tokio::process::Command,
    wheel::{
        fs,
        traits::{
            AsyncCommandOutputExt as _,
            SyncCommandOutputExt as _,
        },
    },
    which::which,
};
#[cfg(unix)] use xdg::BaseDirectories;
#[cfg(windows)] use directories::BaseDirs;

pub mod camc;
pub mod spoiler;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum Branch {
    Dev,
    DevBlitz,
    DevFenhl,
    DevR,
    DevRob,
    Enemizer,
    Sgl2023,
    Sgl2024,
}

impl Branch {
    pub fn from_id(branch_identifier: u8) -> Option<Self> {
        match branch_identifier {
            0x00 | 0x01 => Some(Self::Dev),
            0x45 => Some(Self::DevRob),
            0x46 => Some(Self::Enemizer),
            0x52 => Some(Self::DevR),
            0x69 => Some(Self::DevBlitz),
            0xfe => Some(Self::DevFenhl),
            _ => None,
        }
    }

    pub fn github_username(&self) -> &'static str {
        match self {
            Self::Dev => "OoTRandomizer",
            Self::DevBlitz | Self::Sgl2023 | Self::Sgl2024 => "Elagatua",
            Self::DevR => "Roman971",
            Self::DevFenhl => "fenhl",
            Self::DevRob | Self::Enemizer => "rrealmuto",
        }
    }

    fn github_branch_name(&self, allow_riir: bool) -> Option<&'static str> {
        match self {
            Self::DevFenhl => if allow_riir { Some("riir") } else { None },
            Self::Enemizer => Some("enemy_shuffle"),
            Self::Sgl2023 => Some("feature/sgl-2023"),
            Self::Sgl2024 => Some("feature/sgl-2024"),
            Self::Dev | Self::DevBlitz | Self::DevR | Self::DevRob => None,
        }
    }

    pub fn latest_web_name_known_settings(&self) -> &'static str {
        match self {
            Self::Dev => "dev",
            Self::DevBlitz => "devTFBlitz",
            Self::DevFenhl => "devFenhl",
            Self::DevR => "devR",
            Self::DevRob => "devrreal",
            Self::Enemizer => "devEnemyShuffle",
            Self::Sgl2023 | Self::Sgl2024 => "devSGLive22",
        }
    }

    pub fn latest_web_name_random_settings(&self) -> Option<&'static str> {
        match self {
            Self::Dev => None,
            Self::DevBlitz => None,
            Self::DevFenhl => Some("devFenhlRSL"),
            Self::DevR => None,
            Self::DevRob => Some("devRSL"),
            Self::Enemizer => None,
            Self::Sgl2023 | Self::Sgl2024 => None,
        }
    }

    pub fn latest_web_name(&self, random_settings: bool) -> Option<&'static str> {
        if random_settings {
            self.latest_web_name_random_settings()
        } else {
            Some(self.latest_web_name_known_settings())
        }
    }

    fn dir_parent(&self, allow_riir: bool) -> Result<PathBuf, DirError> {
        #[cfg(unix)] {
            let base_path = Path::new("/opt/git/github.com").join(self.github_username()).join("OoT-Randomizer");
            Ok(if self.github_branch_name(allow_riir).is_some() {
                base_path.join("branch")
            } else {
                base_path
            })
        }
        #[cfg(windows)] {
            let base_path = UserDirs::new().ok_or(DirError::UserDirs)?.home_dir().join("git").join("github.com").join(self.github_username()).join("OoT-Randomizer");
            Ok(if self.github_branch_name(allow_riir).is_some() {
                base_path.join("branch")
            } else {
                base_path
            })
        }
    }

    fn dir_name(&self, allow_riir: bool) -> &'static str {
        if let Some(branch_name) = self.github_branch_name(allow_riir) {
            branch_name
        } else {
            #[cfg(unix)] { "master" }
            #[cfg(windows)] { "main" }
        }
    }

    pub fn dir(&self, allow_riir: bool) -> Result<PathBuf, DirError> {
        Ok(self.dir_parent(allow_riir)?.join(self.dir_name(allow_riir)))
    }

    pub async fn clone_repo(&self, allow_riir: bool) -> Result<(), CloneError> {
        let dir = self.dir(allow_riir)?;
        let cargo_manifest_path = dir.join("Cargo.toml");
        let build_rust = if fs::exists(&dir).await? {
            let old_commit_hash = gix::open(&dir)?.head_id()?.detach();
            //TODO hard reset to remote instead?
            //TODO use gix instead?
            Command::new("git").arg("pull").current_dir(&dir).check("git pull").await?;
            let new_commit_hash = gix::open(&dir)?.head_id()?.detach();
            old_commit_hash != new_commit_hash && fs::exists(&cargo_manifest_path).await?
        } else {
            let parent = self.dir_parent(allow_riir)?;
            fs::create_dir_all(&parent).await?;
            //TODO use gix instead? see:
            // https://docs.rs/gix/latest/gix/fn.prepare_clone.html
            // https://github.com/Byron/gitoxide/blob/072ee32f693a31161cd6a843da6582d13efbb20b/gitoxide-core/src/repository/clone.rs#L75-L84
            let mut command = Command::new("git");
            command.arg("clone");
            command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.github_username()));
            if let Some(branch_name) = self.github_branch_name(allow_riir) {
                command.arg(format!("--branch={branch_name}"));
            }
            command.arg(self.dir_name(allow_riir));
            command.current_dir(parent);
            command.check("git clone").await?;
            fs::exists(dir.join("Cargo.toml")).await?
        };
        if build_rust {
            let rust_lock_dir: Cow<'static, Path> = {
                #[cfg(unix)] { Cow::Borrowed(Path::new("/tmp/syncbin-startup-rust.lock")) }
                #[cfg(windows)] { Cow::Owned(BaseDirs::new().ok_or(CloneError::HomeDir)?.data_local_dir().join("Temp").join("syncbin-startup-rust.lock")) }
            };
            if !which("rustup").is_ok_and(|rustup_path| rustup_path.starts_with("/nix/store")) { // skip self-update if rustup is managed //TODO update rustup via nix
                let lock = DirLock::new(&rust_lock_dir).await?;
                let mut rustup_cmd = Command::new("rustup");
                if let Some(user_dirs) = UserDirs::new() {
                    rustup_cmd.env("PATH", env::join_paths(iter::once(user_dirs.home_dir().join(".cargo").join("bin")).chain(env::var_os("PATH").map(|path| env::split_paths(&path).collect::<Vec<_>>()).into_iter().flatten()))?);
                }
                rustup_cmd.arg("self");
                rustup_cmd.arg("update");
                rustup_cmd.check("rustup").await?;
                lock.drop_async().await?;
            }
            let lock = DirLock::new(rust_lock_dir).await?;
            let mut rustup_cmd = Command::new("rustup");
            if let Some(user_dirs) = UserDirs::new() {
                rustup_cmd.env("PATH", env::join_paths(iter::once(user_dirs.home_dir().join(".cargo").join("bin")).chain(env::var_os("PATH").map(|path| env::split_paths(&path).collect::<Vec<_>>()).into_iter().flatten()))?);
            }
            rustup_cmd.arg("update");
            rustup_cmd.arg("stable");
            rustup_cmd.check("rustup").await?;
            lock.drop_async().await?;
            let mut cargo = Command::new("cargo");
            if let Some(user_dirs) = UserDirs::new() {
                cargo.env("PATH", env::join_paths(iter::once(user_dirs.home_dir().join(".cargo").join("bin")).chain(env::var_os("PATH").map(|path| env::split_paths(&path).collect::<Vec<_>>()).into_iter().flatten()))?);
            }
            cargo.arg("build");
            cargo.arg("--lib"); // old versions of the riir branch were organized as a single crate with multiple targets
            cargo.arg("--release");
            cargo.arg("--package=ootr-python");
            cargo.current_dir(&dir);
            cargo.check("cargo build").await?;
            #[cfg(target_os = "windows")] fs::copy(dir.join("target").join("release").join("rs.dll"), dir.join("rs.pyd")).await?;
            #[cfg(target_os = "linux")] fs::copy(dir.join("target").join("release").join("librs.so"), dir.join("rs.so")).await?;
            #[cfg(target_os = "macos")] fs::copy(dir.join("target").join("release").join("librs.dylib"), dir.join("rs.so")).await?;
            let use_rust_cli = if let Some(package) = cargo_metadata::MetadataCommand::new()
                .manifest_path(cargo_manifest_path)
                .exec()?
                .packages
                .into_iter()
                .find(|package| package.name == "ootr-cli")
            {
                package.version >= semver::Version { major: 8, minor: 2, patch: 49, pre: "fenhl.1.riir.2".parse()?, build: semver::BuildMetadata::default() }
            } else {
                false
            };
            if use_rust_cli {
                let mut cargo = Command::new("cargo");
                if let Some(user_dirs) = UserDirs::new() {
                    cargo.env("PATH", env::join_paths(iter::once(user_dirs.home_dir().join(".cargo").join("bin")).chain(env::var_os("PATH").map(|path| env::split_paths(&path).collect::<Vec<_>>()).into_iter().flatten()))?);
                }
                cargo.arg("build");
                cargo.arg("--release");
                cargo.arg("--package=ootr-cli"); // old versions of the riir branch had ootr-python as the default crate
                cargo.current_dir(&dir);
                cargo.check("cargo build").await?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Protocol)]
#[async_proto(as_string)]
pub struct Version {
    branch: Branch,
    base: semver::Version,
    /// Invariant: `supplementary.is_some() == branch != Branch::Dev`
    supplementary: Option<u8>,
}

#[derive(Debug, thiserror::Error)]
pub enum DirError {
    #[cfg(unix)] #[error(transparent)] Xdg(#[from] xdg::BaseDirectoriesError),
    #[cfg(unix)]
    #[error("midos-house data directory not found")]
    DataPath,
    #[cfg(windows)]
    #[error("failed to access user directories")]
    UserDirs,
}

#[derive(Debug, thiserror::Error)]
pub enum CloneError {
    #[error(transparent)] CargoMetadata(#[from] cargo_metadata::Error),
    #[error(transparent)] Dir(#[from] DirError),
    #[error(transparent)] DirLock(#[from] dir_lock::Error),
    #[error(transparent)] EnvJoinPaths(#[from] env::JoinPathsError),
    #[error(transparent)] GitCommit(#[from] gix::object::commit::Error),
    #[error(transparent)] GitFindObject(#[from] gix::object::find::existing::Error),
    #[error(transparent)] GitHeadCommit(#[from] gix::reference::head_commit::Error),
    #[error(transparent)] GitHeadId(#[from] gix::reference::head_id::Error),
    #[error(transparent)] GitTryInto(#[from] gix::object::try_into::Error),
    #[error(transparent)] GitOpen(#[from] gix::open::Error),
    #[error(transparent)] Semver(#[from] semver::Error),
    #[error(transparent)] Utf8(#[from] std::str::Utf8Error),
    #[error(transparent)] Wheel(#[from] wheel::Error),
    #[error("failed to convert git object")]
    GitObject,
    #[cfg(windows)]
    #[error("could not determine home dir")]
    HomeDir,
    #[error("encountered a revision of the randomizer repo without a version.py")]
    MissingVersionFile,
    #[error("the given version was not found on its branch")]
    VersionNotFound,
}

impl Version {
    pub const fn from_dev(major: u8, minor: u8, patch: u8) -> Self {
        Self {
            branch: Branch::Dev,
            base: semver::Version::new(major as u64, minor as u64, patch as u64),
            supplementary: None,
        }
    }

    pub const fn from_branch(branch: Branch, major: u8, minor: u8, patch: u8, supplementary: u8) -> Self {
        Self {
            base: semver::Version::new(major as u64, minor as u64, patch as u64),
            supplementary: Some(supplementary),
            branch,
        }
    }

    pub fn from_bytes([major, minor, patch, branch, supplementary]: [u8; 5]) -> Option<Self> {
        let branch = Branch::from_id(branch)?;
        Some(if let Branch::Dev = branch {
            Self::from_dev(major, minor, patch)
        } else {
            Self::from_branch(branch, major, minor, patch, supplementary)
        })
    }

    pub fn is_release(&self) -> bool {
        self.branch == Branch::Dev && self.base.patch == 0
    }

    pub fn branch(&self) -> Branch {
        self.branch
    }

    pub fn base(&self) -> &semver::Version {
        &self.base
    }

    pub fn supplementary(&self) -> Option<u8> {
        self.supplementary
    }

    pub fn web_branch_name_known_settings(&self) -> &'static str {
        self.branch.latest_web_name_known_settings() // known-settings web branch names have been stable so far
    }

    pub fn web_branch_name_random_settings(&self) -> Option<&'static str> {
        match self.branch {
            Branch::Dev => None,
            Branch::DevBlitz => None,
            Branch::DevFenhl => Some("devFenhlRSL"),
            Branch::DevR => if self.base >= semver::Version::new(8, 1, 29) { None } else { Some("devRSL") },
            Branch::DevRob => if self.base >= semver::Version::new(8, 1, 29) { Some("devRSL") } else { None },
            Branch::Enemizer => None,
            Branch::Sgl2023 | Branch::Sgl2024 => None,
        }
    }

    pub fn web_branch_name(&self, random_settings: bool) -> Option<&'static str> {
        if random_settings {
            self.web_branch_name_random_settings()
        } else {
            Some(self.web_branch_name_known_settings())
        }
    }

    pub fn to_string_web(&self, random_settings: bool) -> Option<String> {
        Some(if self.is_release() {
            if random_settings { return None }
            self.base.to_string()
        } else if let Some(supplementary) = self.supplementary.filter(|&supplementary| supplementary != 0) {
            format!("{}_{}-{supplementary}", self.web_branch_name(random_settings)?, self.base)
        } else if self.base >= semver::Version::new(8, 1, 14) {
            format!("{}_{}-0", self.web_branch_name(random_settings)?, self.base)
        } else {
            format!("{}_{}", self.web_branch_name(random_settings)?, self.base)
        })
    }

    fn dir_parent(&self) -> Result<PathBuf, DirError> {
        #[cfg(unix)] {
            BaseDirectories::new()?.find_data_file("midos-house").ok_or(DirError::DataPath)
        }
        #[cfg(windows)] {
            Ok(UserDirs::new().ok_or(DirError::UserDirs)?.home_dir().join("git").join("github.com").join(self.branch.github_username()).join("OoT-Randomizer").join("tag"))
        }
    }

    pub fn dir_name(&self) -> String {
        format!(
            "rando-{}-{}{}",
            self.web_branch_name_known_settings(),
            self.base,
            if let Some(supplementary) = self.supplementary { format!("-{supplementary}") } else { String::default() },
        )
    }

    fn repo_dir_name(&self) -> String {
        #[cfg(unix)] {
            self.dir_name()
        }
        #[cfg(windows)] {
            self.base.to_string() //TODO adjust for tag systems on branches other than Dev
        }
    }

    pub fn dir(&self) -> Result<PathBuf, DirError> {
        Ok(self.dir_parent()?.join(self.repo_dir_name()))
    }

    pub async fn clone_repo(&self) -> Result<(), CloneError> {
        let dir = self.dir()?;
        if !fs::exists(&dir).await? {
            let parent = self.dir_parent()?;
            fs::create_dir_all(&parent).await?;
            let to_try = match self.branch {
                Branch::Dev => if self.base.patch == 0 {
                    if self.base.minor == 0 {
                        vec![(vec![format!("--branch=v{}.{}", self.base.major, self.base.minor)], false)]
                    } else {
                        // try tag formats `vX.Y` and `vX.Y.0`
                        vec![
                            (vec![format!("--branch=v{}.{}", self.base.major, self.base.minor)], false),
                            (vec![format!("--branch=v{}", self.base)], false),
                        ]
                    }
                } else {
                    vec![(vec![format!("--branch={}", self.base)], false)]
                },
                Branch::DevFenhl => vec![(vec![format!("--branch={}-fenhl.{}", self.base, self.supplementary.unwrap())], false)],
                Branch::DevRob => vec![
                    // Rob sometimes tags new versions but doesn't merge them into Dev-Rob.
                    (vec![format!("--branch={}.Rob-{}", self.base, self.supplementary.unwrap())], false),
                    // Other times, versions are merged into Dev-Rob without being tagged.
                    (Vec::default(), true),
                ],
                Branch::Enemizer => vec![
                    // Handled like Dev-Rob to be safe
                    (vec![format!("--branch={}.Rob-E{}", self.base, self.supplementary.unwrap())], false),
                    (vec![format!("--branch=enemy_shuffle")], true),
                ],
                Branch::Sgl2023 => vec![(
                    vec![format!("--branch=feature/sgl-2023")],
                    false, // this branch is not versioned correctly
                )],
                Branch::Sgl2024 => vec![(
                    vec![format!("--branch=feature/sgl-2024")],
                    false, // this branch is not versioned correctly
                )],
                Branch::DevBlitz | Branch::DevR => vec![(Vec::default(), true)], // no tags on these forks
            };
            for (pos, (args, bisect)) in to_try.into_iter().with_position() {
                //TODO use gix instead? see:
                // https://docs.rs/gix/latest/gix/fn.prepare_clone.html
                // https://github.com/Byron/gitoxide/blob/072ee32f693a31161cd6a843da6582d13efbb20b/gitoxide-core/src/repository/clone.rs#L75-L84
                let mut command = Command::new("git");
                command.arg("clone");
                command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.branch.github_username()));
                command.arg(self.repo_dir_name());
                command.args(args);
                if !bisect {
                    command.arg("--depth=1");
                }
                command.current_dir(&parent);
                if let Err(e) = command.check("git clone").await {
                    match pos {
                        Position::First | Position::Middle => continue,
                        Position::Last | Position::Only => return Err(e.into()),
                    }
                }
                if bisect {
                    let repo = gix::open(&dir)?;
                    let mut commit = repo.head_commit()?;
                    loop {
                        let blob = commit.tree()?.find_entry("version.py").ok_or(CloneError::MissingVersionFile)?.object()?.try_into_blob()?;
                        let version_py = std::str::from_utf8(&blob.data)?;
                        if version_py.lines()
                            .filter_map(|line| regex_captures!("^__version__ = '([0-9.]+)'$", line))
                            .filter_map(|(_, base_version)| base_version.parse::<semver::Version>().ok())
                            .any(|base_version| base_version == self.base)
                        && self.supplementary.map_or(true, |supplementary| version_py.lines()
                            .filter_map(|line| regex_captures!("^supplementary_version = ([0-9]+)$", line))
                            .filter_map(|(_, supplementary_version)| supplementary_version.parse::<u8>().ok())
                            .any(|supplementary_version| supplementary_version == supplementary))
                        { break }
                        let parent_id = commit.parent_ids().next().ok_or(CloneError::VersionNotFound)?;
                        commit = parent_id.object()?.try_into_commit()?;
                    }
                    std::process::Command::new("git").arg("reset").arg("--hard").arg(commit.id.to_string()).check("git reset")?; //TODO use gix instead?
                }
            }
        }
        Ok(())
    }

    pub fn py_modules(&self, python: impl AsRef<Path>) -> Result<PyModules, DirError> {
        Ok(PyModules {
            python: python.as_ref().to_owned(),
            version: self.clone(),
            path: self.dir()?,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VersionParseError {
    #[error(transparent)] ParseInt(#[from] std::num::ParseIntError),
    #[error("incorrect randomizer base version format")]
    Base,
    #[error("This branch is currently not supported. Please contact Fenhl to request support.")]
    Branch,
    #[error("empty randomizer version or multiple spaces")]
    Words,
}

impl From<VersionParseError> for async_proto::ReadErrorKind {
    fn from(e: VersionParseError) -> Self {
        Self::Custom(e.to_string())
    }
}

impl FromStr for Version {
    type Err = VersionParseError;

    fn from_str(s: &str) -> Result<Self, VersionParseError> {
        match &*s.split_ascii_whitespace().collect_vec() {
            [base] => {
                let (_, major, minor, patch) = regex_captures!(r"^([0-9]+)\.([0-9]+)\.([0-9]+)$", base).ok_or(VersionParseError::Base)?;
                Ok(Self::from_dev(major.parse()?, minor.parse()?, patch.parse()?))
            }
            [base, extra] => {
                let (_, major, minor, patch) = regex_captures!(r"^([0-9]+)\.([0-9]+)\.([0-9]+)$", base).ok_or(VersionParseError::Base)?;
                if let "f.LUM" | "Release" | "pic-2" = *extra {
                    Ok(Self::from_dev(major.parse()?, minor.parse()?, patch.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^blitz-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevBlitz, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^Fenhl-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevFenhl, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^R-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevR, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^Rob-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevRob, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^Rob-E([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::Enemizer, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else {
                    Err(VersionParseError::Branch)
                }
            }
            [base, extra, riir] => {
                let (_, major, minor, patch) = regex_captures!(r"^([0-9]+)\.([0-9]+)\.([0-9]+)$", base).ok_or(VersionParseError::Base)?;
                if let (Some((_, supplementary)), true) = (regex_captures!("^Fenhl-([0-9]+)$", extra), regex_is_match!("^riir-[0-9]+$", riir)) {
                    Ok(Self::from_branch(Branch::DevFenhl, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else {
                    Err(VersionParseError::Branch)
                }
            }
            _ => Err(VersionParseError::Words),
        }
    }
}

derive_deserialize_from_fromstr!(Version, "valid randomizer version number");

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.branch {
            Branch::Dev if self.base.patch == 0 => write!(f, "{} Release", self.base),
            Branch::Dev | Branch::Sgl2023 | Branch::Sgl2024 => write!(f, "{} f.LUM", self.base),
            Branch::DevBlitz => write!(f, "{} blitz-{}", self.base, self.supplementary.unwrap()),
            Branch::DevFenhl => write!(f, "{} Fenhl-{}", self.base, self.supplementary.unwrap()),
            Branch::DevR => write!(f, "{} R-{}", self.base, self.supplementary.unwrap()),
            Branch::DevRob => write!(f, "{} Rob-{}", self.base, self.supplementary.unwrap()),
            Branch::Enemizer => write!(f, "{} Rob-E{}", self.base, self.supplementary.unwrap()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PyJsonErrorKind {
    #[error(transparent)] Json(#[from] serde_json::Error),
    #[error(transparent)] Wheel(#[from] wheel::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("error from randomizer version {version}: {source}")]
pub struct PyJsonError {
    pub version: Version,
    pub source: PyJsonErrorKind,
}

#[derive(Clone)]
pub struct PyModules {
    python: PathBuf,
    version: Version,
    path: PathBuf,
}

impl PyModules {
    pub fn version(&self) -> &Version { &self.version }

    pub async fn py_json<T: DeserializeOwned>(&self, code: &str) -> Result<T, PyJsonError> {
        let output = Command::new(&self.python)
            .arg("-c")
            .arg(code)
            .current_dir(&self.path)
            .check(self.python.display().to_string()).await.map_err(|e| PyJsonError {
                version: self.version.clone(),
                source: e.into(),
            })?;
        Ok(serde_json::from_slice(&output.stdout).map_err(|e| PyJsonError {
            version: self.version.clone(),
            source: e.into(),
        })?)
    }

    pub async fn override_entry(&self, source_world: NonZeroU8, location: &str, target_world: NonZeroU8, item: &str, keyring_give_bk: bool) -> Result<Option<(u64, u16)>, PyJsonError> {
        Ok(self.py_json::<Option<[u8; 16]>>(&format!("
import json, Item, Location, Patches

class Settings:
    def __init__(self):
        self.shuffle_child_trade = [ # assume all trade items shuffled so a MaskShop location listed in a spoiler log will be considered shuffled
            'Weird Egg',
            'Chicken',
            'Zeldas Letter',
            'Keaton Mask',
            'Skull Mask',
            'Spooky Mask',
            'Bunny Hood',
            'Goron Mask',
            'Zora Mask',
            'Gerudo Mask',
            'Mask of Truth',
        ]

        self.keyring_give_bk = {} # checked on dev-fenhl to determine the type of keyring to send, which affects behavior

        # these settings are only checked on dev-fenhl to determine chest appearance, so the actual value doesn't matter
        self.bridge = 'vanilla'
        self.shuffle_ganon_bosskey = 'dungeon'
        self.shuffle_enemy_drops = True
        self.free_bombchu_drops = False
        self.lacs_condition = 'vanilla'
        self.minor_items_as_major_chest = []
        self.tokensanity = 'off'

class World:
    def __init__(self, id):
        self.id = id
        self.settings = Settings()

    @property
    def shuffle_ganon_bosskey(self):
        return self.settings.shuffle_ganon_bosskey

loc = Location.LocationFactory({location:?})
loc.world = World({source_world})
loc.item = Item.ItemFactory({item:?}, World({target_world}))
entry = Patches.get_override_entry(loc)
if entry is None:
    print(json.dumps(None))
else:
    print(json.dumps(list(Patches.override_struct.pack(*entry))))
        ", if keyring_give_bk { "True" } else { "False" })).await?.map(|[k0, k1, k2, k3, k4, k5, k6, k7, v0, v1, _, _, _, _, _, _]| (u64::from_be_bytes([k0, k1, k2, k3, k4, k5, k6, k7]), u16::from_be_bytes([v0, v1]))))
    }
}
