use {
    std::{
        collections::BTreeMap,
        fmt,
        iter,
        marker::PhantomData,
        num::NonZeroU8,
    },
    async_proto::Protocol,
    enum_iterator::Sequence,
    itertools::Itertools as _,
    lazy_regex::regex_captures,
    serde::{
        Deserialize,
        Deserializer,
        Serialize,
        de::{
            Error as _,
            value::MapDeserializer,
        },
    },
    serde_plain::{
        derive_display_from_serialize,
        derive_fromstr_from_deserialize,
    },
    crate::camc::{
        ChestAppearance,
        ChestTexture,
    },
};

pub fn deserialize_multiworld<'de, D: Deserializer<'de>, T: Deserialize<'de>>(deserializer: D) -> Result<Vec<T>, D::Error> {
    struct MultiworldVisitor<'de, T: Deserialize<'de>> {
        _marker: PhantomData<(&'de (), T)>,
    }

    impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for MultiworldVisitor<'de, T> {
        type Value = Vec<T>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a multiworld map")
        }

        fn visit_map<A: serde::de::MapAccess<'de>>(self, mut map: A) -> Result<Vec<T>, A::Error> {
            Ok(if let Some(first_key) = map.next_key::<String>()? {
                if let Some((_, world_number)) = regex_captures!("^World ([0-9]+)$", &first_key) {
                    let world_number = world_number.parse::<usize>().expect("failed to parse world number");
                    let mut worlds = iter::repeat_with(|| None).take(world_number - 1).collect_vec();
                    worlds.push(map.next_value()?);
                    while let Some((key, value)) = map.next_entry::<String, _>()? {
                        let world_number = regex_captures!("^World ([0-9]+)$", &key).expect("found mixed-format multiworld spoiler log").1.parse::<usize>().expect("failed to parse world number");
                        if world_number > worlds.len() {
                            if world_number > worlds.len() + 1 {
                                worlds.resize_with(world_number - 1, || None);
                            }
                            worlds.push(Some(value));
                        } else {
                            worlds[world_number - 1] = Some(value);
                        }
                    }
                    worlds.into_iter().map(|world| world.expect("missing entry for world")).collect()
                } else {
                    let mut new_map = iter::once((first_key.to_owned(), map.next_value()?)).collect::<serde_json::Map<_, _>>();
                    while let Some((key, value)) = map.next_entry()? {
                        new_map.insert(key, value);
                    }
                    vec![T::deserialize(MapDeserializer::new(new_map.into_iter())).map_err(A::Error::custom)?]
                }
            } else {
                Vec::default()
            })
        }
    }

    deserializer.deserialize_map(MultiworldVisitor { _marker: PhantomData })
}

#[derive(Debug, Deserialize, Protocol)]
pub struct SpoilerLog {
    pub file_hash: [HashIcon; 5],
    pub password: Option<[OcarinaNote; 6]>,
    #[serde(rename = ":version")]
    pub version: crate::Version,
    #[serde(deserialize_with = "deserialize_multiworld")]
    pub settings: Vec<Settings>,
    #[serde(deserialize_with = "deserialize_multiworld")]
    pub randomized_settings: Vec<RandomizedSettings>,
    #[serde(deserialize_with = "deserialize_multiworld")]
    pub locations: Vec<BTreeMap<String, Item>>,
}

impl SpoilerLog {
    pub fn midos_house_chests(&self) -> impl Iterator<Item = [ChestAppearance; 4]> + '_ {
        self.locations.iter().enumerate().map(|(world_idx, locations)| {
            let player = NonZeroU8::new(world_idx as u8 + 1).unwrap();
            [
                ChestAppearance::from_item(self, player, ChestAppearance { texture: ChestTexture::Normal, big: false }, ["KF Midos House Top Left Chest", "KF Midos Top Left Chest", "Mido Chest Top Left"].into_iter().find_map(|name| locations.get(name)).cloned().unwrap_or_else(|| Item { item: format!("Rupees (5)"), model: None, player })),
                ChestAppearance::from_item(self, player, ChestAppearance { texture: ChestTexture::Normal, big: false }, ["KF Midos House Top Right Chest", "KF Midos Top Right Chest", "Mido Chest Top Right"].into_iter().find_map(|name| locations.get(name)).cloned().unwrap_or_else(|| Item { item: format!("Rupees (5)"), model: None, player })),
                ChestAppearance::from_item(self, player, ChestAppearance { texture: ChestTexture::Normal, big: false }, ["KF Midos House Bottom Left Chest", "KF Midos Bottom Left Chest", "Mido Chest Bottom Left"].into_iter().find_map(|name| locations.get(name)).cloned().unwrap_or_else(|| Item { item: format!("Rupee (1)"), model: None, player })),
                ChestAppearance::from_item(self, player, ChestAppearance { texture: ChestTexture::Normal, big: false }, ["KF Midos House Bottom Right Chest", "KF Midos Bottom Right Chest", "Mido Chest Bottom Right"].into_iter().find_map(|name| locations.get(name)).cloned().unwrap_or_else(|| Item { item: format!("Recovery Heart"), model: None, player })),
            ]
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence, Deserialize, Serialize, Protocol)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type), sqlx(type_name = "hash_icon"))]
pub enum HashIcon {
    #[serde(rename = "Deku Stick")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Deku Stick"))]
    DekuStick,
    #[serde(rename = "Deku Nut")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Deku Nut"))]
    DekuNut,
    Bow,
    Slingshot,
    #[serde(rename = "Fairy Ocarina")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Fairy Ocarina"))]
    FairyOcarina,
    Bombchu,
    Longshot,
    Boomerang,
    #[serde(rename = "Lens of Truth")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Lens of Truth"))]
    LensOfTruth,
    Beans,
    #[serde(rename = "Megaton Hammer")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Megaton Hammer"))]
    MegatonHammer,
    #[serde(rename = "Bottled Fish")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Bottled Fish"))]
    BottledFish,
    #[serde(rename = "Bottled Milk")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Bottled Milk"))]
    BottledMilk,
    #[serde(rename = "Mask of Truth")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Mask of Truth"))]
    MaskOfTruth,
    #[serde(rename = "SOLD OUT")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "SOLD OUT"))]
    SoldOut,
    Cucco,
    Mushroom,
    Saw,
    Frog,
    #[serde(rename = "Master Sword")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Master Sword"))]
    MasterSword,
    #[serde(rename = "Mirror Shield")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Mirror Shield"))]
    MirrorShield,
    #[serde(rename = "Kokiri Tunic")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Kokiri Tunic"))]
    KokiriTunic,
    #[serde(rename = "Hover Boots")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Hover Boots"))]
    HoverBoots,
    #[serde(rename = "Silver Gauntlets")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Silver Gauntlets"))]
    SilverGauntlets,
    #[serde(rename = "Gold Scale")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Gold Scale"))]
    GoldScale,
    #[serde(rename = "Stone of Agony")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Stone of Agony"))]
    StoneOfAgony,
    #[serde(rename = "Skull Token")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Skull Token"))]
    SkullToken,
    #[serde(rename = "Heart Container")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Heart Container"))]
    HeartContainer,
    #[serde(rename = "Boss Key")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Boss Key"))]
    BossKey,
    Compass,
    Map,
    #[serde(rename = "Big Magic")]
    #[cfg_attr(feature = "sqlx", sqlx(rename = "Big Magic"))]
    BigMagic,
}

impl HashIcon {
    pub fn from_racetime_emoji(emoji: &str) -> Option<Self> {
        match emoji {
            "HashBeans" => Some(Self::Beans),
            "HashBigMagic" => Some(Self::BigMagic),
            "HashBombchu" => Some(Self::Bombchu),
            "HashBoomerang" => Some(Self::Boomerang),
            "HashBossKey" => Some(Self::BossKey),
            "HashBottledFish" => Some(Self::BottledFish),
            "HashBottledMilk" => Some(Self::BottledMilk),
            "HashBow" => Some(Self::Bow),
            "HashCompass" => Some(Self::Compass),
            "HashCucco" => Some(Self::Cucco),
            "HashDekuNut" => Some(Self::DekuNut),
            "HashDekuStick" => Some(Self::DekuStick),
            "HashFairyOcarina" => Some(Self::FairyOcarina),
            "HashFrog" => Some(Self::Frog),
            "HashGoldScale" => Some(Self::GoldScale),
            "HashHeart" => Some(Self::HeartContainer),
            "HashHoverBoots" => Some(Self::HoverBoots),
            "HashKokiriTunic" => Some(Self::KokiriTunic),
            "HashLensOfTruth" => Some(Self::LensOfTruth),
            "HashLongshot" => Some(Self::Longshot),
            "HashMap" => Some(Self::Map),
            "HashMaskOfTruth" => Some(Self::MaskOfTruth),
            "HashMasterSword" => Some(Self::MasterSword),
            "HashHammer" => Some(Self::MegatonHammer),
            "HashMirrorShield" => Some(Self::MirrorShield),
            "HashMushroom" => Some(Self::Mushroom),
            "HashSaw" => Some(Self::Saw),
            "HashSilvers" => Some(Self::SilverGauntlets),
            "HashSkullToken" => Some(Self::SkullToken),
            "HashSlingshot" => Some(Self::Slingshot),
            "HashSoldOut" => Some(Self::SoldOut),
            "HashStoneOfAgony" => Some(Self::StoneOfAgony),
            _ => None,
        }
    }

    pub fn to_racetime_emoji(&self) -> &'static str {
        match self {
            Self::Beans => "HashBeans",
            Self::BigMagic => "HashBigMagic",
            Self::Bombchu => "HashBombchu",
            Self::Boomerang => "HashBoomerang",
            Self::BossKey => "HashBossKey",
            Self::BottledFish => "HashBottledFish",
            Self::BottledMilk => "HashBottledMilk",
            Self::Bow => "HashBow",
            Self::Compass => "HashCompass",
            Self::Cucco => "HashCucco",
            Self::DekuNut => "HashDekuNut",
            Self::DekuStick => "HashDekuStick",
            Self::FairyOcarina => "HashFairyOcarina",
            Self::Frog => "HashFrog",
            Self::GoldScale => "HashGoldScale",
            Self::HeartContainer => "HashHeart",
            Self::HoverBoots => "HashHoverBoots",
            Self::KokiriTunic => "HashKokiriTunic",
            Self::LensOfTruth => "HashLensOfTruth",
            Self::Longshot => "HashLongshot",
            Self::Map => "HashMap",
            Self::MaskOfTruth => "HashMaskOfTruth",
            Self::MasterSword => "HashMasterSword",
            Self::MegatonHammer => "HashHammer",
            Self::MirrorShield => "HashMirrorShield",
            Self::Mushroom => "HashMushroom",
            Self::Saw => "HashSaw",
            Self::SilverGauntlets => "HashSilvers",
            Self::SkullToken => "HashSkullToken",
            Self::Slingshot => "HashSlingshot",
            Self::SoldOut => "HashSoldOut",
            Self::StoneOfAgony => "HashStoneOfAgony",
        }
    }
}

derive_fromstr_from_deserialize!(HashIcon);
derive_display_from_serialize!(HashIcon);

#[derive(Debug, thiserror::Error)]
#[error("invalid hash icon")]
pub struct HashIconDecodeError;

impl TryFrom<u8> for HashIcon {
    type Error = HashIconDecodeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::Beans),
            0x01 => Ok(Self::BigMagic),
            0x02 => Ok(Self::Bombchu),
            0x03 => Ok(Self::Boomerang),
            0x04 => Ok(Self::BossKey),
            0x05 => Ok(Self::BottledFish),
            0x06 => Ok(Self::BottledMilk),
            0x07 => Ok(Self::Bow),
            0x08 => Ok(Self::Compass),
            0x09 => Ok(Self::Cucco),
            0x0a => Ok(Self::DekuNut),
            0x0b => Ok(Self::DekuStick),
            0x0c => Ok(Self::FairyOcarina),
            0x0d => Ok(Self::Frog),
            0x0e => Ok(Self::GoldScale),
            0x0f => Ok(Self::HeartContainer),
            0x10 => Ok(Self::HoverBoots),
            0x11 => Ok(Self::KokiriTunic),
            0x12 => Ok(Self::LensOfTruth),
            0x13 => Ok(Self::Longshot),
            0x14 => Ok(Self::Map),
            0x15 => Ok(Self::MaskOfTruth),
            0x16 => Ok(Self::MasterSword),
            0x17 => Ok(Self::MegatonHammer),
            0x18 => Ok(Self::MirrorShield),
            0x19 => Ok(Self::Mushroom),
            0x1a => Ok(Self::Saw),
            0x1b => Ok(Self::SilverGauntlets),
            0x1c => Ok(Self::SkullToken),
            0x1d => Ok(Self::Slingshot),
            0x1e => Ok(Self::SoldOut),
            0x1f => Ok(Self::StoneOfAgony),
            _ => Err(HashIconDecodeError),
        }
    }
}

impl From<HashIcon> for u8 {
    fn from(icon: HashIcon) -> Self {
        match icon {
            HashIcon::Beans => 0x00,
            HashIcon::BigMagic => 0x01,
            HashIcon::Bombchu => 0x02,
            HashIcon::Boomerang => 0x03,
            HashIcon::BossKey => 0x04,
            HashIcon::BottledFish => 0x05,
            HashIcon::BottledMilk => 0x06,
            HashIcon::Bow => 0x07,
            HashIcon::Compass => 0x08,
            HashIcon::Cucco => 0x09,
            HashIcon::DekuNut => 0x0a,
            HashIcon::DekuStick => 0x0b,
            HashIcon::FairyOcarina => 0x0c,
            HashIcon::Frog => 0x0d,
            HashIcon::GoldScale => 0x0e,
            HashIcon::HeartContainer => 0x0f,
            HashIcon::HoverBoots => 0x10,
            HashIcon::KokiriTunic => 0x11,
            HashIcon::LensOfTruth => 0x12,
            HashIcon::Longshot => 0x13,
            HashIcon::Map => 0x14,
            HashIcon::MaskOfTruth => 0x15,
            HashIcon::MasterSword => 0x16,
            HashIcon::MegatonHammer => 0x17,
            HashIcon::MirrorShield => 0x18,
            HashIcon::Mushroom => 0x19,
            HashIcon::Saw => 0x1a,
            HashIcon::SilverGauntlets => 0x1b,
            HashIcon::SkullToken => 0x1c,
            HashIcon::Slingshot => 0x1d,
            HashIcon::SoldOut => 0x1e,
            HashIcon::StoneOfAgony => 0x1f,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence, Deserialize, Serialize, Protocol)]
pub enum OcarinaNote {
    A,
    #[serde(rename = "C down")]
    CDown,
    #[serde(rename = "C right")]
    CRight,
    #[serde(rename = "C left")]
    CLeft,
    #[serde(rename = "C up")]
    CUp,
}

impl OcarinaNote {
    pub fn from_racetime_emoji(emoji: &str) -> Option<Self> {
        match emoji {
            "NoteA" => Some(Self::A),
            "NoteCdown" => Some(Self::CDown),
            "NoteCright" => Some(Self::CRight),
            "NoteCleft" => Some(Self::CLeft),
            "NoteCup" => Some(Self::CUp),
            _ => None,
        }
    }

    pub fn to_racetime_emoji(&self) -> &'static str {
        match self {
            Self::A => "NoteA",
            Self::CDown => "NoteCdown",
            Self::CRight => "NoteCright",
            Self::CLeft => "NoteCleft",
            Self::CUp => "NoteCup",
        }
    }
}

impl TryFrom<char> for OcarinaNote {
    type Error = char;

    fn try_from(c: char) -> Result<Self, char> {
        match c {
            'A' | 'a' => Ok(Self::A),
            'D' | 'd' | 'V' | 'v' => Ok(Self::CDown),
            'R' | 'r' | '>' => Ok(Self::CRight),
            'L' | 'l' | '<' => Ok(Self::CLeft),
            'U' | 'u' | '^' => Ok(Self::CUp),
            _ => Err(c),
        }
    }
}

impl From<OcarinaNote> for char {
    fn from(note: OcarinaNote) -> Self {
        match note {
            OcarinaNote::A => 'A',
            OcarinaNote::CDown => 'v',
            OcarinaNote::CRight => '>',
            OcarinaNote::CLeft => '<',
            OcarinaNote::CUp => '^',
        }
    }
}

fn make_one() -> NonZeroU8 { NonZeroU8::new(1).unwrap() }

#[derive(Debug, Deserialize, Protocol)]
pub struct Settings {
    #[serde(default = "make_one")]
    pub world_count: NonZeroU8,
    #[serde(default)]
    pub lacs_condition: LacsCondition,
    #[serde(default)]
    pub bridge: Bridge,
    #[serde(default)]
    pub shuffle_ganon_bosskey: ShuffleGanonBosskey,
    #[serde(default)]
    pub keyring_give_bk: bool,
    #[serde(default, alias = "bombchus_in_logic")]
    pub free_bombchu_drops: bool,
    #[serde(default)]
    pub correct_chest_sizes: bool,
    pub correct_chest_appearances: Option<CorrectChestAppearances>,
    #[serde(default)]
    pub minor_items_as_major_chest: MinorItemsAsMajorChest,
    #[serde(default)]
    pub invisible_chests: bool,
}

#[derive(Debug, Deserialize, Protocol)]
pub struct RandomizedSettings {
    #[serde(default)]
    pub bridge: Bridge,
}

#[derive(Debug, Default, Deserialize, Protocol)]
#[serde(rename_all = "snake_case")]
pub enum Bridge {
    Open,
    Vanilla,
    Stones,
    #[default]
    Medallions,
    Dungeons,
    Tokens,
    Hearts,
    Random,
}

#[derive(Debug, Default, Deserialize, Protocol)]
#[serde(rename_all = "snake_case")]
pub enum LacsCondition {
    #[default]
    #[serde(alias = "lacs_vanilla")] // this was never actually the name of this option, but it was specified this way for a while by the RSL script
    Vanilla,
    Stones,
    Medallions,
    Dungeons,
    Tokens,
    Hearts,
}

#[derive(Debug, Default, Deserialize, Protocol)]
#[serde(rename_all = "snake_case")]
pub enum ShuffleGanonBosskey {
    Remove,
    Vanilla,
    #[default]
    Dungeon,
    Regional,
    Overworld,
    AnyDungeon,
    Keysanity,
    #[serde(alias = "lacs_vanilla")]
    OnLacs,
    #[serde(alias = "lacs_stones")] // close enough for CAMC purposes
    Stones,
    #[serde(alias = "lacs_medallions")] // close enough for CAMC purposes
    Medallions,
    #[serde(alias = "lacs_dungeons")] // close enough for CAMC purposes
    Dungeons,
    #[serde(alias = "lacs_tokens")] // close enough for CAMC purposes
    Tokens,
    Hearts,
}

#[derive(Debug, Default, Clone, Copy, Deserialize, Protocol)]
#[serde(rename_all = "snake_case")]
pub enum CorrectChestAppearances {
    #[default]
    Off,
    Classic,
    Textures,
    #[serde(alias = "sizes")]
    Both,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum MinorItemsAsMajorChestCombobox {
    Off,
    Shields,
    Bombchus,
    Both,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum MinorItemsAsMajorChestMultiselect {
    Bombchus,
    Shields,
    Capacity,
}

/// The `minor_items_as_major_chest` setting is a checkbox on main Dev but a multiselect on dev-fenhl.
#[derive(Deserialize)]
#[serde(untagged)]
enum JsonMinorItemsAsMajorChest {
    Checkbox(bool),
    Combobox(MinorItemsAsMajorChestCombobox),
    Multiselect(Vec<MinorItemsAsMajorChestMultiselect>),
}

impl From<JsonMinorItemsAsMajorChest> for MinorItemsAsMajorChest {
    fn from(value: JsonMinorItemsAsMajorChest) -> Self {
        match value {
            JsonMinorItemsAsMajorChest::Checkbox(value) => Self { bombchus: value, shields: value, capacity: false },
            JsonMinorItemsAsMajorChest::Combobox(value) => match value {
                MinorItemsAsMajorChestCombobox::Off => Self { bombchus: false, shields: false, capacity: false },
                MinorItemsAsMajorChestCombobox::Shields => Self { bombchus: false, shields: true, capacity: false },
                MinorItemsAsMajorChestCombobox::Bombchus => Self { bombchus: true, shields: false, capacity: false },
                MinorItemsAsMajorChestCombobox::Both => Self { bombchus: true, shields: true, capacity: false },
            },
            JsonMinorItemsAsMajorChest::Multiselect(items) => {
                let mut value = Self { bombchus: false, shields: false, capacity: false, };
                for item in items {
                    match item {
                        MinorItemsAsMajorChestMultiselect::Bombchus => value.bombchus = true,
                        MinorItemsAsMajorChestMultiselect::Shields => value.shields = true,
                        MinorItemsAsMajorChestMultiselect::Capacity => value.capacity = true,
                    }
                }
                value
            }
        }
    }
}

#[derive(Debug, Default, Deserialize, Protocol)]
#[serde(from = "JsonMinorItemsAsMajorChest")]
pub struct MinorItemsAsMajorChest {
    pub bombchus: bool,
    pub shields: bool,
    pub capacity: bool,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum JsonItem {
    Simple(String),
    Complex {
        #[serde(default = "make_one")]
        player: NonZeroU8,
        item: String,
        model: Option<String>,
    },
}

#[derive(Debug, Clone, Deserialize, Protocol)]
#[serde(from = "JsonItem")]
pub struct Item {
    pub player: NonZeroU8,
    pub item: String,
    pub model: Option<String>,
}

impl From<JsonItem> for Item {
    fn from(item: JsonItem) -> Self {
        match item {
            JsonItem::Simple(item) => Self { item, player: make_one(), model: None },
            JsonItem::Complex { player, item, model } => Self { player, item, model },
        }
    }
}
