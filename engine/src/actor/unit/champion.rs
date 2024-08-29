pub mod champ_1;

use bevy::prelude::*;

use super::{ServerUnitBundle, UnitBundle};

#[derive(Debug, Clone, Copy, Component)]
pub struct Champion;

#[derive(Default, Bundle)]
pub struct ChampionBundle {
    unit: UnitBundle,
}
#[derive(Default, Bundle)]
pub struct ServerChampionBundle {
    unit: ServerUnitBundle,
}

pub struct ChampionInfo {
    pub name: String,
}
