#![deny(rust_2018_idioms, unused, unused_crate_dependencies, unused_import_braces, unused_lifetimes, unused_qualifications, warnings)]
#![forbid(unsafe_code)]

use {
    std::{
        fmt,
        path::PathBuf,
        str::FromStr,
    },
    async_proto::Protocol,
    itertools::Itertools as _,
    lazy_regex::regex_captures,
    serde_plain::derive_deserialize_from_fromstr,
    tokio::process::Command,
    wheel::traits::AsyncCommandOutputExt as _,
};
#[cfg(unix)] use xdg::BaseDirectories;
#[cfg(windows)] use directories::UserDirs;
#[cfg(feature = "pyo3")] use {
    pyo3::{
        exceptions::*,
        prelude::*,
        types::PyList,
    },
    crate::lazy::PyLazy,
};

#[cfg(feature = "pyo3")] mod lazy;
pub mod spoiler;

#[cfg(feature = "pyo3")] static MODULE_SEARCH_PATH: PyLazy<PyResult<Vec<String>>> = PyLazy::new(|py| py.import("sys")?.getattr("path")?.extract());

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Branch {
    Dev,
    DevFenhl,
    DevR,
}

impl Branch {
    pub fn github_username(&self) -> &'static str {
        match self {
            Self::Dev => "TestRunnerSRL",
            Self::DevFenhl => "fenhl",
            Self::DevR => "Roman971",
        }
    }

    pub fn web_name_known_settings(&self) -> &'static str {
        match self {
            Self::Dev => "dev",
            Self::DevFenhl => "devFenhl",
            Self::DevR => "devR",
        }
    }

    pub fn web_name_random_settings(&self) -> Option<&'static str> {
        match self {
            Self::Dev => None,
            Self::DevFenhl => Some("devFenhlRSL"),
            Self::DevR => Some("devRSL"),
        }
    }

    pub fn web_name(&self, random_settings: bool) -> Option<&'static str> {
        if random_settings {
            self.web_name_random_settings()
        } else {
            Some(self.web_name_known_settings())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Protocol)]
#[async_proto(as_string)]
pub struct Version {
    branch: Branch,
    base: semver::Version,
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

#[cfg(feature = "pyo3")] impl From<DirError> for PyErr {
    fn from(e: DirError) -> Self {
        match e {
            #[cfg(unix)] DirError::Xdg(e) => tokio::io::Error::from(e).into(),
            #[cfg(unix)] DirError::DataPath => PyFileNotFoundError::new_err(e.to_string()),
            #[cfg(windows)] DirError::UserDirs => PyFileNotFoundError::new_err(e.to_string()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CloneError {
    #[error(transparent)] Dir(#[from] DirError),
    #[error(transparent)] Wheel(#[from] wheel::Error),
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

    fn dir_name(&self) -> String {
        #[cfg(unix)] {
            format!(
                "rando-{}-{}{}",
                self.branch.web_name_known_settings(),
                self.base,
                if let Some(supplementary) = self.supplementary { format!("-{supplementary}") } else { String::default() },
            )
        }
        #[cfg(windows)] {
            self.base.to_string() //TODO adjust for tag systems on branches other than Dev
        }
    }

    pub fn dir(&self) -> Result<PathBuf, DirError> {
        Ok(self.dir_parent()?.join(self.dir_name()))
    }

    pub async fn clone_repo(&self) -> Result<(), CloneError> {
        if !self.dir()?.exists() {
            let mut command = Command::new("git");
            command.arg("clone");
            command.arg("--depth=1"); //TODO don't use for branches that have to be bisected
            command.arg(format!("https://github.com/{}/OoT-Randomizer.git", self.branch.github_username()));
            command.arg(self.dir_name());
            command.current_dir(self.dir_parent()?);
            match self.branch {
                Branch::Dev => {
                    command.arg(format!("--branch={}", self.base));
                }
                Branch::DevFenhl => {
                    command.arg(format!("--branch={}-fenhl.{}", self.base, self.supplementary.unwrap()));
                }
                Branch::DevR => unimplemented!(), //TODO bisect Dev-R to find the requested version
            }
            command.check("git").await?;
        }
        Ok(())
    }

    #[cfg(feature = "pyo3")]
    pub fn py_modules<'p>(&self, py: Python<'p>) -> PyResult<PyModules<'p>> {
        let mut new_path = match MODULE_SEARCH_PATH.get(py) {
            Ok(path) => path.clone(),
            Err(e) => return Err(e.clone_ref(py)),
        };
        new_path.push(self.dir()?.into_os_string().into_string().expect("non-UTF-8 randomizer path"));
        py.import("sys")?.getattr("path")?.downcast::<PyList>()?.set_slice(0, py.import("sys")?.getattr("path")?.downcast::<PyList>()?.len(), new_path.into_py(py).as_ref(py))?;
        Ok(PyModules { version: self.clone(), py })
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
                if *extra == "f.LUM" {
                    Ok(Self::from_dev(major.parse()?, minor.parse()?, patch.parse()?))
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
            Branch::DevFenhl => write!(f, "{} Fenhl-{}", self.base, self.supplementary.unwrap()),
            Branch::DevR => write!(f, "{} R-{}", self.base, self.supplementary.unwrap()),
        }
    }
}

#[cfg(feature = "pyo3")]
#[derive(Clone)]
pub struct PyModules<'p> {
    py: Python<'p>,
    version: Version,
}

#[cfg(feature = "pyo3")]
impl PyModules<'_> {
    pub fn py(&self) -> Python<'_> { self.py }
    pub fn version(&self) -> &Version { &self.version }

    pub fn override_key(&self, location: &str, item: &str) -> PyResult<Option<u32>> {
        let mod_location = self.py.import("Location")?;
        let location = mod_location.getattr("LocationFactory")?.call1((location,))?;
        let default = location.getattr("default")?;
        let mod_item = self.py.import("Item")?;
        let item = mod_item.getattr("ItemFactory")?.call1((item,))?;
        Ok(if let (Some(scene), false) = (location.getattr("scene")?.extract()?, default.is_none()) {
            let (kind, default) = match location.getattr("type")?.extract()? {
                "NPC" | "Scrub" | "BossHeart" => (0, default.extract::<u16>()?),
                "Chest" => (1, default.extract::<u16>()? & 0x1f),
                "Freestanding" | "Pot" | "Crate" | "FlyingPot" | "SmallCrate" | "RupeeTower" | "Beehive" | "SilverRupee" => {
                    let (room, scene_setup, flag) = if default.is_instance_of::<PyList>()? { default.get_item(0)? } else { default }.extract::<(u16, u16, u16)>()?;
                    (6, room << 8 + scene_setup << 14 + flag)
                }
                "Collectable" | "ActorOverride" => (2, default.extract()?),
                "GS Token" => (3, default.extract()?),
                "Shop" if item.getattr("type")?.extract::<&str>()? != "Shop" => (0, default.extract()?),
                "GrottoScrub" if item.getattr("type")?.extract::<&str>()? != "Shop" => (4, default.extract()?),
                "Song" | "Cutscene" => (5, default.extract()?),
                _ => return Ok(None),
            };
            Some(if self.version.base < semver::Version::new(6, 9, 1) {
                u32::from_be_bytes([0, scene, kind, default as u8])
            } else {
                let [default_hi, default_lo] = default.to_be_bytes();
                u32::from_be_bytes([scene, kind, default_hi, default_lo])
            })
        } else {
            None
        })
    }

    pub fn item_kind(&self, item: &str) -> PyResult<Option<u16>> {
        let item_list = self.py.import("ItemList")?;
        Ok(item_list.getattr("item_table")?.call_method1("get", (item,))?.extract::<Option<(&PyAny, &PyAny, _, &PyAny)>>()?.map(|(_, _, kind, _)| kind))
    }
}
