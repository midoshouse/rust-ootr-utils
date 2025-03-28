use {
    std::num::NonZeroU8,
    serde::{
        Deserialize,
        Serialize,
    },
    crate::{
        Branch,
        spoiler::{
            Bridge,
            CorrectChestAppearances,
            Item,
            LacsCondition,
            ShuffleGanonBosskey,
            SpoilerLog,
        },
    },
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    /// The original “Chest Size Matches Contents” setting, added in [commit 9866777](https://github.com/OoTRandomizer/OoT-Randomizer/tree/9866777f66083dfc8dde90fba5a71302b34459fb)
    Classic,
    /// The initial iteration of “Chest Appearance Matches Contents”, added in [PR #1429](https://github.com/OoTRandomizer/OoT-Randomizer/pull/1429), [version 6.2.4](https://github.com/OoTRandomizer/OoT-Randomizer/tree/0e8c66a6a3b3a35df0920b220eb5188b1479cfa1)
    Initial,
    /// The second iteration of “Chest Appearance Matches Contents” which updated the textures for major items and small keys to make them more distinctive, and reintroduced the classic behavior as an option.
    /// Added in [PR #1500](https://github.com/OoTRandomizer/OoT-Randomizer/pull/1500), [version 6.2.54](https://github.com/OoTRandomizer/OoT-Randomizer/tree/1e39a95e8a4629e962634bd7e02f71d7d3602353)
    Pr1500,
    /// The adjusted small key texture from [PR #1751](https://github.com/OoTRandomizer/OoT-Randomizer/pull/1751), [version 6.2.233](https://github.com/OoTRandomizer/OoT-Randomizer/tree/38334774503cd9a2c7389e222abe5884617830b7)
    Pr1751,
    /// The addition of heart chest textures from [PR #1908](https://github.com/OoTRandomizer/OoT-Randomizer/pull/1908), [version 7.1.76](https://github.com/OoTRandomizer/OoT-Randomizer/tree/9e823509c41cbcd3f53081cf9d99ceedc96036e7)
    Pr1908,
    /// The addition of bombchu chest textures on [the Triforce Blitz branch](crate::Branch::DevBlitz) starting with [version 8.0.22 blitz-48](https://github.com/Elagatua/OoT-Randomizer/tree/18848dfc94a596bf58d88d542b6561b7104c5138)
    TfbS3,
}

impl Version {
    pub fn from_rando_version(rando_version: &crate::Version) -> Self {
        if rando_version.branch() == Branch::DevBlitz && *rando_version.base() >= semver::Version::new(8, 0, 22) && rando_version.supplementary().is_some_and(|supplementary| supplementary >= 48) {
            Self::TfbS3
        } else if *rando_version.base() >= semver::Version::new(7, 1, 76) {
            Self::Pr1908
        } else if *rando_version.base() >= semver::Version::new(6, 2, 233) {
            Self::Pr1751
        } else if *rando_version.base() >= semver::Version::new(6, 2, 54) {
            Self::Pr1500
        } else if *rando_version.base() >= semver::Version::new(6, 2, 4) {
            Self::Initial
        } else {
            // CSMC seems to have been introduced before the current versioning scheme
            Self::Classic
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ChestTexture {
    Normal,
    OldMajor,
    Major,
    SmallKeyOld,
    SmallKey1500,
    SmallKey1751,
    BossKey,
    Token,
    Invisible,
    Heart,
    Bombchu,
}

impl TryFrom<char> for ChestTexture {
    type Error = char;

    fn try_from(c: char) -> Result<Self, char> {
        match c {
            'n' => Ok(Self::Normal),
            'm' => Ok(Self::OldMajor),
            'i' => Ok(Self::Major),
            'k' => Ok(Self::SmallKeyOld),
            'y' => Ok(Self::SmallKey1500),
            'a' => Ok(Self::SmallKey1751),
            'b' => Ok(Self::BossKey),
            's' => Ok(Self::Token),
            'd' => Ok(Self::Invisible),
            'h' => Ok(Self::Heart),
            'c' => Ok(Self::Bombchu),
            _ => Err(c),
        }
    }
}

impl From<ChestTexture> for char {
    fn from(texture: ChestTexture) -> Self {
        match texture {
            ChestTexture::Normal => 'n',
            ChestTexture::OldMajor => 'm',
            ChestTexture::Major => 'i',
            ChestTexture::SmallKeyOld => 'k',
            ChestTexture::SmallKey1500 => 'y',
            ChestTexture::SmallKey1751 => 'a',
            ChestTexture::BossKey => 'b',
            ChestTexture::Token => 's',
            ChestTexture::Invisible => 'd',
            ChestTexture::Heart => 'h',
            ChestTexture::Bombchu => 'c',
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize)]
pub struct ChestAppearance {
    pub texture: ChestTexture,
    pub big: bool,
}

impl ChestAppearance {
    pub fn from_item(SpoilerLog { version, settings, randomized_settings, .. }: &SpoilerLog, source_world: NonZeroU8, mut vanilla_appearance: ChestAppearance, item: Item) -> Self {
        let camc_version = Version::from_rando_version(&version);
        let camc_kind = match camc_version {
            Version::Classic => if settings.get(usize::from(source_world.get() - 1)).unwrap_or_else(|| &settings[0]).correct_chest_sizes { CorrectChestAppearances::Classic } else { CorrectChestAppearances::Off },
            Version::Initial | Version::Pr1500 | Version::Pr1751 | Version::Pr1908 | Version::TfbS3 => settings.get(usize::from(source_world.get() - 1)).unwrap_or_else(|| &settings[0]).correct_chest_appearances.unwrap_or_default(),
        };
        //TODO support for the new setting(s?) affecting chest appearance on DevBlitz, e.g. “Minor Items use Chext Texture” (no source code available yet?)
        let chus_in_major_chests = settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).free_bombchu_drops || settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).minor_items_as_major_chest.bombchus;
        let shields_in_major_chests = settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).minor_items_as_major_chest.shields;
        let capacity_in_major_chests = settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).minor_items_as_major_chest.capacity;
        let token_wincon = matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).bridge, Bridge::Tokens) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).bridge, Bridge::Random) && matches!(randomized_settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &randomized_settings[0]).bridge, Bridge::Tokens) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).lacs_condition, LacsCondition::Tokens) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).shuffle_ganon_bosskey, ShuffleGanonBosskey::Tokens);
        let heart_wincon = matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).bridge, Bridge::Hearts) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).bridge, Bridge::Random) && matches!(randomized_settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &randomized_settings[0]).bridge, Bridge::Hearts) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).lacs_condition, LacsCondition::Hearts) || matches!(settings.get(usize::from(item.player.get() - 1)).unwrap_or_else(|| &settings[0]).shuffle_ganon_bosskey, ShuffleGanonBosskey::Hearts);
        if let CorrectChestAppearances::Off = camc_kind {
            if settings.get(usize::from(source_world.get() - 1)).unwrap_or_else(|| &settings[0]).invisible_chests {
                vanilla_appearance.texture = ChestTexture::Invisible;
            }
            return vanilla_appearance
        }
        let item_name = if item.item == "Ice Trap" {
            item.model.as_deref().expect("ice trap without model in CSMC")
        } else {
            &item.item
        };
        let mut appearance = match item_name {
            "Bow" |
            "Slingshot" |
            "Deku Seed Bag" |
            "Boomerang" |
            "Progressive Hookshot" |
            "Lens of Truth" |
            "Zeldas Letter" |
            "Ocarina" |
            "Hammer" |
            "Megaton Hammer" |
            "Cojiro" |
            "Bottle" |
            "Bottle with Red Potion" |
            "Bottle with Green Potion" |
            "Bottle with Blue Potion" |
            "Bottle with Fairy" |
            "Bottle with Milk" |
            "Bottle with Letter" |
            "Rutos Letter" |
            "Skull Mask" |
            "Spooky Mask" |
            "Chicken" |
            "Keaton Mask" |
            "Bunny Hood" |
            "Mask of Truth" |
            "Pocket Egg" |
            "Pocket Cucco" |
            "Odd Mushroom" |
            "Odd Potion" |
            "Poachers Saw" |
            "Broken Sword" |
            "Prescription" |
            "Eyeball Frog" |
            "Eyedrops" |
            "Claim Check" |
            "Kokiri Sword" |
            "Giants Knife" |
            "Mirror Shield" |
            "Goron Tunic" |
            "Zora Tunic" |
            "Iron Boots" |
            "Hover Boots" |
            "Bomb Bag" |
            "Progressive Strength Upgrade" |
            "Progressive Scale" |
            "Stone of Agony" |
            "Gerudo Membership Card" |
            "Progressive Wallet" |
            "Weird Egg" |
            "Goron Mask" |
            "Zora Mask" |
            "Gerudo Mask" |
            "Biggoron Sword" |
            "Fire Arrows" |
            "Ice Arrows" |
            "Blue Fire Arrows" |
            "Light Arrows" |
            "Dins Fire" |
            "Farores Wind" |
            "Nayrus Love" |
            "Bombchus" |
            "Magic Meter" |
            "Bottle with Fish" |
            "Bottle with Blue Fire" |
            "Bottle with Bugs" |
            "Bottle with Big Poe" |
            "Bottle with Poe" |
            "Double Defense" |
            "Minuet of Forest" |
            "Bolero of Fire" |
            "Serenade of Water" |
            "Requiem of Spirit" |
            "Nocturne of Shadow" |
            "Prelude of Light" |
            "Zeldas Lullaby" |
            "Eponas Song" |
            "Sarias Song" |
            "Suns Song" |
            "Song of Time" |
            "Song of Storms" |
            "Magic Bean Pack" |
            "Triforce Piece" |
            "Easter Egg" |
            "Easter Egg (Pink)" |
            "Easter Egg (Orange)" |
            "Easter Egg (Green)" |
            "Easter Egg (Blue)" |
            "Triforce of Power" |
            "Triforce of Wisdom" |
            "Triforce of Courage" |
            "Kokiri Emerald" |
            "Goron Ruby" |
            "Zora Sapphire" |
            "Light Medallion" |
            "Forest Medallion" |
            "Fire Medallion" |
            "Water Medallion" |
            "Shadow Medallion" |
            "Spirit Medallion" |
            "Ocarina A Button" |
            "Ocarina C down Button" |
            "Ocarina C right Button" |
            "Ocarina C left Button" |
            "Ocarina C up Button" |
            "Stalfos Soul" |
            "Octorok Soul" |
            "Wallmaster Soul" |
            "Dodongo Soul" |
            "Keese Soul" |
            "Tektite Soul" |
            "Peahat Soul" |
            "Lizalfos and Dinalfos Soul" |
            "Gohma Larvae Soul" |
            "Shabom Soul" |
            "Baby Dodongo Soul" |
            "Biri and Bari Soul" |
            "Tailpasaran Soul" |
            "Skulltula Soul" |
            "Torch Slug Soul" |
            "Moblin Soul" |
            "Armos Soul" |
            "Deku Baba Soul" |
            "Deku Scrub Soul" |
            "Bubble Soul" |
            "Beamos Soul" |
            "Floormaster Soul" |
            "Redead and Gibdo Soul" |
            "Skullwalltula Soul" |
            "Flare Dancer Soul" |
            "Dead hand Soul" |
            "Shell Blade Soul" |
            "Like-like Soul" |
            "Like Like Soul" |
            "Spike Enemy Soul" |
            "Anubis Soul" |
            "Iron Knuckle Soul" |
            "Skull Kid Soul" |
            "Flying Pot Soul" |
            "Freezard Soul" |
            "Stinger Soul" |
            "Wolfos Soul" |
            "Guay Soul" |
            "Queen Gohma Soul" |
            "King Dodongo Soul" |
            "Barinade Soul" |
            "Phantom Ganon Soul" |
            "Volvagia Soul" |
            "Morpha Soul" |
            "Bongo Bongo Soul" |
            "Twinrova Soul" |
            "Jabu Jabu Tentacle Soul" |
            "Dark Link Soul" |
            "Deku Tree Souls" |
            "Dodongos Cavern Souls" |
            "Jabu Jabus Belly Souls" |
            "Forest Temple Souls" |
            "Fire Temple Souls" |
            "Water Temple Souls" |
            "Shadow Temple Souls" |
            "Spirit Temple Souls" |
            "Bottom of the Well Souls" |
            "Ice Cavern Souls" |
            "Gerudo Training Ground Souls" |
            "Ganons Castle Souls" |
            "Forest Area Souls" |
            "Hyrule Field Souls" |
            "Lake Hylia Souls" |
            "Gerudo Area Souls" |
            "Market Area Souls" |
            "Kakariko Area Souls" |
            "Goron Area Souls" |
            "Zora Area Souls" |
            "Lon Lon Ranch Souls" |
            "Grottos Souls" |
            "Fishing Rod" |
            "Fish (Child 10 lb)" |
            "Fish (Adult 16 lb)" |
            "Hylian Loach" => match camc_kind {
                CorrectChestAppearances::Off => unreachable!(),
                CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: true },
                CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::Major, big: vanilla_appearance.big },
                CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::Major, big: true },
            },
            "Boss Key (Forest Temple)" |
            "Boss Key (Fire Temple)" |
            "Boss Key (Water Temple)" |
            "Boss Key (Spirit Temple)" |
            "Boss Key (Shadow Temple)" |
            "Boss Key (Ganons Castle)" => match camc_kind {
                CorrectChestAppearances::Off => unreachable!(),
                CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::BossKey, big: true },
                CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::BossKey, big: vanilla_appearance.big },
                CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::BossKey, big: true },
            },
            "Small Key (Forest Temple)" |
            "Small Key (Fire Temple)" |
            "Small Key (Water Temple)" |
            "Small Key (Spirit Temple)" |
            "Small Key (Shadow Temple)" |
            "Small Key (Bottom of the Well)" |
            "Small Key (Gerudo Training Grounds)" |
            "Small Key (Gerudo Training Ground)" |
            "Small Key (Gerudo Fortress)" |
            "Small Key (Thieves Hideout)" |
            "Small Key (Ganons Castle)" |
            "Small Key (Treasure Chest Game)" |
            "Small Key Ring (Forest Temple)" |
            "Small Key Ring (Fire Temple)" |
            "Small Key Ring (Water Temple)" |
            "Small Key Ring (Spirit Temple)" |
            "Small Key Ring (Shadow Temple)" |
            "Small Key Ring (Bottom of the Well)" |
            "Small Key Ring (Gerudo Training Ground)" |
            "Small Key Ring (Thieves Hideout)" |
            "Small Key Ring (Ganons Castle)" |
            "Small Key Ring (Treasure Chest Game)" |
            "Silver Rupee (Dodongos Cavern Staircase)" |
            "Silver Rupee (Ice Cavern Spinning Scythe)" |
            "Silver Rupee (Ice Cavern Push Block)" |
            "Silver Rupee (Bottom of the Well Basement)" |
            "Silver Rupee (Shadow Temple Scythe Shortcut)" |
            "Silver Rupee (Shadow Temple Invisible Blades)" |
            "Silver Rupee (Shadow Temple Huge Pit)" |
            "Silver Rupee (Shadow Temple Invisible Spikes)" |
            "Silver Rupee (Gerudo Training Ground Slopes)" |
            "Silver Rupee (Gerudo Training Ground Lava)" |
            "Silver Rupee (Gerudo Training Ground Water)" |
            "Silver Rupee (Spirit Temple Child Early Torches)" |
            "Silver Rupee (Spirit Temple Adult Boulders)" |
            "Silver Rupee (Spirit Temple Lobby and Lower Adult)" |
            "Silver Rupee (Spirit Temple Sun Block)" |
            "Silver Rupee (Spirit Temple Adult Climb)" |
            "Silver Rupee (Ganons Castle Spirit Trial)" |
            "Silver Rupee (Ganons Castle Light Trial)" |
            "Silver Rupee (Ganons Castle Fire Trial)" |
            "Silver Rupee (Ganons Castle Shadow Trial)" |
            "Silver Rupee (Ganons Castle Water Trial)" |
            "Silver Rupee (Ganons Castle Forest Trial)" |
            "Silver Rupee Pouch (Dodongos Cavern Staircase)" |
            "Silver Rupee Pouch (Ice Cavern Spinning Scythe)" |
            "Silver Rupee Pouch (Ice Cavern Push Block)" |
            "Silver Rupee Pouch (Bottom of the Well Basement)" |
            "Silver Rupee Pouch (Shadow Temple Scythe Shortcut)" |
            "Silver Rupee Pouch (Shadow Temple Invisible Blades)" |
            "Silver Rupee Pouch (Shadow Temple Huge Pit)" |
            "Silver Rupee Pouch (Shadow Temple Invisible Spikes)" |
            "Silver Rupee Pouch (Gerudo Training Ground Slopes)" |
            "Silver Rupee Pouch (Gerudo Training Ground Lava)" |
            "Silver Rupee Pouch (Gerudo Training Ground Water)" |
            "Silver Rupee Pouch (Spirit Temple Child Early Torches)" |
            "Silver Rupee Pouch (Spirit Temple Adult Boulders)" |
            "Silver Rupee Pouch (Spirit Temple Lobby and Lower Adult)" |
            "Silver Rupee Pouch (Spirit Temple Sun Block)" |
            "Silver Rupee Pouch (Spirit Temple Adult Climb)" |
            "Silver Rupee Pouch (Ganons Castle Spirit Trial)" |
            "Silver Rupee Pouch (Ganons Castle Light Trial)" |
            "Silver Rupee Pouch (Ganons Castle Fire Trial)" |
            "Silver Rupee Pouch (Ganons Castle Shadow Trial)" |
            "Silver Rupee Pouch (Ganons Castle Water Trial)" |
            "Silver Rupee Pouch (Ganons Castle Forest Trial)" => match camc_kind {
                CorrectChestAppearances::Off => unreachable!(),
                CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::BossKey, big: false },
                CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::SmallKeyOld, big: vanilla_appearance.big },
                CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::SmallKeyOld, big: false },
            },
            "Ice Trap" => unreachable!(),
            "Bombchus (5)" |
            "Bombchus (10)" |
            "Bombchus (20)" => if let Version::TfbS3 = camc_version {
                //TODO is this controlled by a setting?
                ChestAppearance { texture: ChestTexture::Bombchu, big: false }
            } else {
                match camc_kind {
                    CorrectChestAppearances::Off => unreachable!(),
                    CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: chus_in_major_chests },
                    CorrectChestAppearances::Textures => ChestAppearance { texture: if chus_in_major_chests { ChestTexture::Major } else { ChestTexture::Normal }, big: vanilla_appearance.big },
                    CorrectChestAppearances::Both => ChestAppearance { texture: if chus_in_major_chests { ChestTexture::Major } else { ChestTexture::Normal }, big: chus_in_major_chests },
                }
            },
            "Deku Shield" |
            "Hylian Shield" => ChestAppearance { texture: if shields_in_major_chests { ChestTexture::Major } else { ChestTexture::Normal }, big: shields_in_major_chests },
            "Deku Stick Capacity" |
            "Deku Nut Capacity" => ChestAppearance { texture: if capacity_in_major_chests { ChestTexture::Major } else { ChestTexture::Normal }, big: capacity_in_major_chests },
            "Gold Skulltula Token" => match camc_kind {
                CorrectChestAppearances::Off => unreachable!(),
                CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: token_wincon },
                CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::Token, big: vanilla_appearance.big },
                CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::Token, big: token_wincon },
            },
            "Heart Container" |
            "Piece of Heart" |
            "Piece of Heart (Treasure Chest Game)" => if let Version::TfbS3 = camc_version {
                //TODO is this controlled by a setting?
                ChestAppearance { texture: ChestTexture::Normal, big: false }
            } else if camc_version >= Version::Pr1908 {
                match camc_kind {
                    CorrectChestAppearances::Off => unreachable!(),
                    CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: heart_wincon },
                    CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::Heart, big: vanilla_appearance.big },
                    CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::Heart, big: heart_wincon },
                }
            } else {
                match camc_kind {
                    CorrectChestAppearances::Off => unreachable!(),
                    CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: heart_wincon },
                    CorrectChestAppearances::Textures => ChestAppearance { texture: if heart_wincon { ChestTexture::Major } else { ChestTexture::Normal }, big: vanilla_appearance.big },
                    CorrectChestAppearances::Both => ChestAppearance { texture: if heart_wincon { ChestTexture::Major } else { ChestTexture::Normal }, big: heart_wincon },
                }
            },
            "Bombs (5)" |
            "Deku Nuts (5)" |
            "Deku Stick (1)" |
            "Magic Bean" |
            "Deku Seeds (5)" |
            "Compass (Deku Tree)" |
            "Compass (Dodongos Cavern)" |
            "Compass (Jabu Jabus Belly)" |
            "Compass (Forest Temple)" |
            "Compass (Fire Temple)" |
            "Compass (Water Temple)" |
            "Compass (Spirit Temple)" |
            "Compass (Shadow Temple)" |
            "Compass (Bottom of the Well)" |
            "Compass (Ice Cavern)" |
            "Map (Deku Tree)" |
            "Map (Dodongos Cavern)" |
            "Map (Jabu Jabus Belly)" |
            "Map (Forest Temple)" |
            "Map (Fire Temple)" |
            "Map (Water Temple)" |
            "Map (Spirit Temple)" |
            "Map (Shadow Temple)" |
            "Map (Bottom of the Well)" |
            "Map (Ice Cavern)" |
            "Recovery Heart" |
            "Arrows (5)" |
            "Arrows (10)" |
            "Arrows (30)" |
            "Rupee (1)" |
            "Rupees (5)" |
            "Rupees (20)" |
            "Heart Container (Boss)" |
            "Rupees (50)" |
            "Rupees (200)" |
            "Deku Sticks (5)" |
            "Deku Sticks (10)" |
            "Deku Nuts (10)" |
            "Bomb (1)" |
            "Bombs (10)" |
            "Bombs (20)" |
            "Deku Seeds (30)" |
            "Rupee (Treasure Chest Game)" |
            "Rupee (Treasure Chest Game) (1)" |
            "Rupees (Treasure Chest Game) (5)" |
            "Rupees (Treasure Chest Game) (20)" |
            "Rupees (Treasure Chest Game) (50)" |
            "Fairy Drop" |
            "Nothing" |
            "Fish (Child 6 lb)" |
            "Fish (Child 8 lb)" |
            "Fish (Adult 10 lb)" |
            "Fish (Adult 12 lb)" => match camc_kind {
                CorrectChestAppearances::Off => unreachable!(),
                CorrectChestAppearances::Classic => ChestAppearance { texture: ChestTexture::Normal, big: false },
                CorrectChestAppearances::Textures => ChestAppearance { texture: ChestTexture::Normal, big: vanilla_appearance.big },
                CorrectChestAppearances::Both => ChestAppearance { texture: ChestTexture::Normal, big: false },
            },
            _ => unimplemented!("unknown item: {item_name}"),
        };
        match camc_version {
            Version::Classic => {}
            Version::Initial => if let ChestTexture::Major = appearance.texture { appearance.texture = ChestTexture::OldMajor },
            Version::Pr1500 => if let ChestTexture::SmallKeyOld = appearance.texture { appearance.texture = ChestTexture::SmallKey1500 },
            Version::Pr1751 | Version::Pr1908 | Version::TfbS3 => if let ChestTexture::SmallKeyOld = appearance.texture { appearance.texture = ChestTexture::SmallKey1751 },
        }
        //TODO support for incorrect_chest_appearances setting
        if settings.get(usize::from(source_world.get() - 1)).unwrap_or_else(|| &settings[0]).invisible_chests {
            appearance.texture = ChestTexture::Invisible;
        }
        appearance
    }
}
