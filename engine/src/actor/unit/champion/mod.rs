pub mod champ_1;

use bevy::prelude::*;

use super::UnitBundle;

#[derive(Debug, Clone, Copy, Component)]
pub struct Champion;

#[derive(Bundle)]
pub struct ChampionBundle {
    unit: UnitBundle,
}

pub struct ChampionInfo {
    pub name: String,
}
