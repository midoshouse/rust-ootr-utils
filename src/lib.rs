#![deny(rust_2018_idioms, unused, unused_crate_dependencies, unused_import_braces, unused_lifetimes, unused_qualifications, warnings)]
#![forbid(unsafe_code)]

use {
    std::{
        fmt,
        num::NonZeroU8,
        path::{
            Path,
            PathBuf,
        },
        str::FromStr,
    },
    async_proto::Protocol,
    git2::{
        Repository,
        ResetType,
    },
    itertools::{
        Itertools as _,
        Position,
    },
    lazy_regex::regex_captures,
    serde::de::DeserializeOwned,
    serde_plain::derive_deserialize_from_fromstr,
    tokio::process::Command,
    wheel::{
        fs,
        traits::AsyncCommandOutputExt as _,
    },
};
#[cfg(unix)] use xdg::BaseDirectories;
#[cfg(windows)] use directories::UserDirs;

pub mod camc;
pub mod spoiler;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Branch {
    Dev,
    DevBlitz,
    DevFenhl,
    DevR,
    DevRob,
    Sgl,
}

impl Branch {
    pub fn from_id(branch_identifier: u8) -> Option<Self> {
        match branch_identifier {
            0x00 | 0x01 => Some(Self::Dev),
            0x45 => Some(Self::DevRob),
            0x52 => Some(Self::DevR),
            0x69 => Some(Self::DevBlitz),
            0xfe => Some(Self::DevFenhl),
            _ => None,
        }
    }

    pub fn github_username(&self) -> &'static str {
        match self {
            Self::Dev => "OoTRandomizer",
            Self::DevBlitz | Self::Sgl => "Elagatua",
            Self::DevR => "Roman971",
            Self::DevFenhl => "fenhl",
            Self::DevRob => "rrealmuto",
        }
    }

    fn github_branch_name(&self) -> Option<&'static str> {
        match self {
            Self::Sgl => Some("feature/sgl-2023"),
            Self::Dev | Self::DevBlitz | Self::DevR | Self::DevRob | Self::DevFenhl => None,
        }
    }

    pub fn latest_web_name_known_settings(&self) -> &'static str {
        match self {
            Self::Dev => "dev",
            Self::DevBlitz => "devTFBlitz",
            Self::DevFenhl => "devFenhl",
            Self::DevR => "devR",
            Self::DevRob => "devrreal",
            Self::Sgl => "devSGLive22",
        }
    }

    pub fn latest_web_name_random_settings(&self) -> Option<&'static str> {
        match self {
            Self::Dev => None,
            Self::DevBlitz => None,
            Self::DevFenhl => Some("devFenhlRSL"),
            Self::DevR => None,
            Self::DevRob => Some("devRSL"),
            Self::Sgl => None,
        }
    }

    pub fn latest_web_name(&self, random_settings: bool) -> Option<&'static str> {
        if random_settings {
            self.latest_web_name_random_settings()
        } else {
            Some(self.latest_web_name_known_settings())
        }
    }

    fn dir_parent(&self) -> Result<PathBuf, DirError> {
        #[cfg(unix)] {
            let base_path = Path::new("/opt/git/github.com").join(self.github_username()).join("OoT-Randomizer");
            Ok(if self.github_branch_name().is_some() {
                base_path.join("branch")
            } else {
                base_path
            })
        }
        #[cfg(windows)] {
            let base_path = UserDirs::new().ok_or(DirError::UserDirs)?.home_dir().join("git").join("github.com").join(self.github_username()).join("OoT-Randomizer");
            Ok(if self.github_branch_name().is_some() {
                base_path.join("branch")
            } else {
                base_path
            })
        }
    }

    fn dir_name(&self) -> &'static str {
        if let Some(branch_name) = self.github_branch_name() {
            branch_name
        } else {
            #[cfg(unix)] { "master" }
            #[cfg(windows)] { "main" }
        }
    }

    pub fn dir(&self) -> Result<PathBuf, DirError> {
        Ok(self.dir_parent()?.join(self.dir_name()))
    }

    pub async fn clone_repo(&self) -> Result<(), CloneError> {
        let dir = self.dir()?;
        if dir.exists() {
            //TODO hard reset to remote instead?
            //TODO use git2 or gix instead?
            Command::new("git").arg("pull").current_dir(dir).check("git").await?;
        } else {
            let parent = self.dir_parent()?;
            fs::create_dir_all(&parent).await?;
            let mut command = Command::new("git"); //TODO use git2 or gix instead? (git2 doesn't support shallow clones, gix is very low level)
            command.arg("clone");
            command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.github_username()));
            if let Some(branch_name) = self.github_branch_name() {
                command.arg(format!("--branch={branch_name}"));
            }
            command.arg(self.dir_name());
            command.current_dir(parent);
            command.check("git").await?;
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
    #[error(transparent)] Dir(#[from] DirError),
    #[error(transparent)] Git(#[from] git2::Error),
    #[error(transparent)] Utf8(#[from] std::str::Utf8Error),
    #[error(transparent)] Wheel(#[from] wheel::Error),
    #[error("failed to convert git object")]
    GitObject,
    #[error("the given version was not found on its branch")]
    VersionNotFound,
}

impl<'a> From<git2::Object<'a>> for CloneError {
    fn from(_: git2::Object<'a>) -> Self {
        Self::GitObject
    }
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
            Branch::Sgl => None,
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
        if !dir.exists() {
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
                Branch::Sgl => vec![(
                    vec![format!("--branch=feature/sgl-2023")],
                    false, // this branch is not versioned correctly
                )],
                Branch::DevBlitz | Branch::DevR => vec![(Vec::default(), true)], // no tags on these forks
            };
            for (pos, (args, bisect)) in to_try.into_iter().with_position() {
                let mut command = Command::new("git"); //TODO use git2 or gix instead? (git2 doesn't support shallow clones, gix is very low level)
                command.arg("clone");
                command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.branch.github_username()));
                command.arg(self.repo_dir_name());
                command.args(args);
                if !bisect {
                    command.arg("--depth=1");
                }
                command.current_dir(&parent);
                if let Err(e) = command.check("git").await {
                    match pos {
                        Position::First | Position::Middle => continue,
                        Position::Last | Position::Only => return Err(e.into()),
                    }
                }
                if bisect {
                    let repo = Repository::open(&dir)?;
                    let mut commit = repo.head()?.peel_to_commit()?;
                    loop {
                        let blob = commit.tree()?.get_path(Path::new("version.py"))?.to_object(&repo)?.into_blob()?;
                        let version_py = std::str::from_utf8(blob.content())?;
                        if version_py.lines()
                            .filter_map(|line| regex_captures!("^__version__ = '([0-9.]+)'$", line))
                            .filter_map(|(_, base_version)| base_version.parse::<semver::Version>().ok())
                            .any(|base_version| base_version == self.base)
                        && self.supplementary.map_or(true, |supplementary| version_py.lines()
                            .filter_map(|line| regex_captures!("^supplementary_version = ([0-9]+)$", line))
                            .filter_map(|(_, supplementary_version)| supplementary_version.parse::<u8>().ok())
                            .any(|supplementary_version| supplementary_version == supplementary))
                        { break }
                        commit = commit.parents().next().ok_or(CloneError::VersionNotFound)?;
                    }
                    repo.reset(&commit.into_object(), ResetType::Hard, None)?;
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
                } else if let "SGL" = *extra {
                    Ok(Self::from_branch(Branch::Sgl, major.parse()?, minor.parse()?, patch.parse()?, 1))
                } else if let Some((_, supplementary)) = regex_captures!("^blitz-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevBlitz, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^Fenhl-([0-9]+)(?: riir-[0-9]+)?$", extra) {
                    Ok(Self::from_branch(Branch::DevFenhl, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^R-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevR, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^Rob-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevRob, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
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
            Branch::Dev => write!(f, "{} f.LUM", self.base),
            Branch::DevBlitz => write!(f, "{} blitz-{}", self.base, self.supplementary.unwrap()),
            Branch::DevFenhl => write!(f, "{} Fenhl-{}", self.base, self.supplementary.unwrap()),
            Branch::DevR => write!(f, "{} R-{}", self.base, self.supplementary.unwrap()),
            Branch::DevRob => write!(f, "{} Rob-{}", self.base, self.supplementary.unwrap()),
            Branch::Sgl => write!(f, "{} SGL", self.base),
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
