use {
    std::{
        fmt,
        iter,
        marker::PhantomData,
        num::NonZeroU8,
    },
    async_proto::Protocol,
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
    serde_plain::derive_display_from_serialize,
};

fn deserialize_multiworld<'de, D: Deserializer<'de>, T: Deserialize<'de>>(deserializer: D) -> Result<Vec<T>, D::Error> {
    struct MultiworldVisitor<'de, T: Deserialize<'de>> {
        _marker: PhantomData<(&'de (), T)>,

    }

    impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for MultiworldVisitor<'de, T> {
        type Value = Vec<T>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a multiworld map")
        }

        fn visit_map<A: serde::de::MapAccess<'de>>(self, mut map: A) -> Result<Vec<T>, A::Error> {
            Ok(if let Some(first_key) = map.next_key()? {
                if let Some((_, world_number)) = regex_captures!("^World ([0-9]+)$", first_key) {
                    let world_number = world_number.parse::<usize>().expect("failed to parse world number");
                    let mut worlds = iter::repeat_with(|| None).take(world_number - 1).collect_vec();
                    worlds.push(map.next_value()?);
                    while let Some((key, value)) = map.next_entry()? {
                        let world_number = regex_captures!("^World ([0-9]+)$", key).expect("found mixed-format multiworld spoiler log").1.parse::<usize>().expect("failed to parse world number");
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

#[derive(Deserialize)]
pub struct SpoilerLog {
    pub file_hash: [HashIcon; 5],
    #[serde(rename = ":version")]
    pub version: crate::Version,
    pub settings: Settings,
    #[serde(deserialize_with = "deserialize_multiworld")]
    pub locations: Vec<Locations>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, Protocol)]
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

derive_display_from_serialize!(HashIcon);

fn make_one() -> NonZeroU8 { NonZeroU8::new(1).unwrap() }

#[derive(Deserialize)]
pub struct Settings {
    #[serde(default = "make_one")]
    pub world_count: NonZeroU8,
    #[serde(default)]
    pub bridge: Bridge,
    #[serde(default)]
    pub bombchus_in_logic: bool,
    #[serde(default)]
    pub shuffle_ganon_bosskey: ShuffleGanonBosskey,
    #[serde(default)]
    pub lacs_condition: LacsCondition,
    #[serde(default)]
    pub correct_chest_sizes: bool,
    pub correct_chest_appearances: Option<CorrectChestAppearances>,
    #[serde(default)]
    pub minor_items_as_major_chest: MinorItemsAsMajorChest,
    #[serde(default)]
    pub invisible_chests: bool,
}

#[derive(Default, Deserialize)]
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
}

#[derive(Default, Deserialize)]
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

#[derive(Default, Deserialize)]
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

#[derive(Default, Clone, Copy, Deserialize)]
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
enum MinorItemAsMajorChest {
    Bombchus,
    Shields,
}

/// The `minor_items_as_major_chest` setting is a checkbox on main Dev but a multiselect on dev-fenhl.
#[derive(Deserialize)]
#[serde(untagged)]
enum JsonMinorItemsAsMajorChest {
    Checkbox(bool),
    Multiselect(Vec<MinorItemAsMajorChest>),
}

impl From<JsonMinorItemsAsMajorChest> for MinorItemsAsMajorChest {
    fn from(value: JsonMinorItemsAsMajorChest) -> Self {
        match value {
            JsonMinorItemsAsMajorChest::Checkbox(value) => Self { bombchus: value, shields: value },
            JsonMinorItemsAsMajorChest::Multiselect(items) => {
                let mut value = Self { bombchus: false, shields: false };
                for item in items {
                    match item {
                        MinorItemAsMajorChest::Bombchus => value.bombchus = true,
                        MinorItemAsMajorChest::Shields => value.shields = true,
                    }
                }
                value
            }
        }
    }
}

#[derive(Default, Deserialize)]
#[serde(from = "JsonMinorItemsAsMajorChest")]
pub struct MinorItemsAsMajorChest {
    pub bombchus: bool,
    pub shields: bool,
}

fn make_blue_rupee() -> Item { Item { item: format!("Rupees (5)"), model: None } }
fn make_green_rupee() -> Item { Item { item: format!("Rupee (1)"), model: None } }
fn make_recovery_heart() -> Item { Item { item: format!("Recovery Heart"), model: None } }

#[derive(Deserialize)]
pub struct Locations {
    #[serde(rename = "KF Midos Top Left Chest", alias = "Mido Chest Top Left", default = "make_blue_rupee")] pub kf_midos_top_left_chest: Item,
    #[serde(rename = "KF Midos Top Right Chest", alias = "Mido Chest Top Right", default = "make_blue_rupee")] pub kf_midos_top_right_chest: Item,
    #[serde(rename = "KF Midos Bottom Left Chest", alias = "Mido Chest Bottom Left", default = "make_green_rupee")] pub kf_midos_bottom_left_chest: Item,
    #[serde(rename = "KF Midos Bottom Right Chest", alias = "Mido Chest Bottom Right", default = "make_recovery_heart")] pub kf_midos_bottom_right_chest: Item,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum JsonItem {
    Simple(String),
    Complex {
        item: String,
        model: Option<String>,
    },
}

#[derive(Deserialize)]
#[serde(from = "JsonItem")]
pub struct Item {
    pub item: String,
    pub model: Option<String>,
}

impl From<JsonItem> for Item {
    fn from(item: JsonItem) -> Self {
        match item {
            JsonItem::Simple(item) => Self { item, model: None },
            JsonItem::Complex { item, model } => Self { item, model },
        }
    }
}
