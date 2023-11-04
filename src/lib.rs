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
    itertools::Itertools as _,
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
    Sgl,
}

impl Branch {
    pub fn from_id(branch_identifier: u8) -> Option<Self> {
        match branch_identifier {
            0x00 | 0x01 => Some(Self::Dev),
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
        }
    }

    fn github_branch_name(&self) -> Option<&'static str> {
        match self {
            Self::Sgl => Some("feature/sgl-2023"),
            Self::Dev | Self::DevBlitz | Self::DevR | Self::DevFenhl => None,
        }
    }

    pub fn web_name_known_settings(&self) -> &'static str {
        match self {
            Self::Dev => "dev",
            Self::DevBlitz => "devTFBlitz",
            Self::DevFenhl => "devFenhl",
            Self::DevR => "devR",
            Self::Sgl => "devSGLive22",
        }
    }

    pub fn web_name_random_settings(&self) -> Option<&'static str> {
        match self {
            Self::Dev => None,
            Self::DevBlitz => None,
            Self::DevFenhl => Some("devFenhlRSL"),
            Self::DevR => Some("devRSL"),
            Self::Sgl => None,
        }
    }

    pub fn web_name(&self, random_settings: bool) -> Option<&'static str> {
        if random_settings {
            self.web_name_random_settings()
        } else {
            Some(self.web_name_known_settings())
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

    pub fn branch(&self) -> Branch {
        self.branch
    }

    pub fn base(&self) -> &semver::Version {
        &self.base
    }

    pub fn supplementary(&self) -> Option<u8> {
        self.supplementary
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
            self.branch.web_name_known_settings(),
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
            let mut command = Command::new("git"); //TODO use git2 or gix instead? (git2 doesn't support shallow clones, gix is very low level)
            command.arg("clone");
            command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.branch.github_username()));
            command.arg(self.repo_dir_name());
            command.current_dir(parent);
            let bisect = match self.branch {
                Branch::Dev => {
                    if self.base.patch == 0 {
                        if self.base.minor == 0 {
                            command.arg(format!("--branch=v{}.{}", self.base.major, self.base.minor));
                        } else {
                            command.arg(format!("--branch=v{}", self.base));
                        }
                    } else {
                        command.arg(format!("--branch={}", self.base));
                    }
                    false
                }
                Branch::DevFenhl => {
                    command.arg(format!("--branch={}-fenhl.{}", self.base, self.supplementary.unwrap()));
                    false
                }
                Branch::DevBlitz | Branch::DevR => true,
                Branch::Sgl => {
                    command.arg("--branch=feature/sgl-2023");
                    false // this branch is not versioned correctly
                }
            };
            if !bisect {
                command.arg("--depth=1");
            }
            command.check("git").await?;
            if bisect {
                let repo = Repository::open(dir)?;
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
        Ok(())
    }

    pub fn py_modules(&self) -> Result<PyModules, DirError> {
        Ok(PyModules { version: self.clone(), path: self.dir()? })
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

impl From<VersionParseError> for async_proto::ReadError {
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
                } else if let Some((_, supplementary)) = regex_captures!("^Fenhl-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevFenhl, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
                } else if let Some((_, supplementary)) = regex_captures!("^R-([0-9]+)$", extra) {
                    Ok(Self::from_branch(Branch::DevR, major.parse()?, minor.parse()?, patch.parse()?, supplementary.parse()?))
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
            Branch::Sgl => write!(f, "{} SGL", self.base),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PyJsonError {
    #[error(transparent)] Json(#[from] serde_json::Error),
    #[error(transparent)] Wheel(#[from] wheel::Error),
}

#[derive(Clone)]
pub struct PyModules {
    version: Version,
    path: PathBuf,
}

impl PyModules {
    pub fn version(&self) -> &Version { &self.version }

    pub async fn py_json<T: DeserializeOwned>(&self, code: &str) -> Result<T, PyJsonError> {
        let output = Command::new("python3")
            .arg("-c")
            .arg(code)
            .current_dir(&self.path)
            .check("python3").await?;
        Ok(serde_json::from_slice(&output.stdout)?)
    }

    pub async fn override_entry(&self, world: NonZeroU8, location: &str, item: &str) -> Result<(u64, u16), PyJsonError> {
        let [k0, k1, k2, k3, k4, k5, k6, k7, v0, v1, _, _, _, _, _, _] = self.py_json(&format!("
import Item, Location, Patches

class World:
    def __init__(self):
        self.id = {world}

loc = Location.LocationFactory({location:?})
loc.item = Item.ItemFactory({item:?}, World())
print(list(Patches.override_struct.pack(*Patches.get_override_entry(loc))))
        ")).await?;
        Ok((u64::from_be_bytes([k0, k1, k2, k3, k4, k5, k6, k7]), u16::from_be_bytes([v0, v1])))
    }
}
