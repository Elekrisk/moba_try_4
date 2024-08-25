use bevy::{prelude::*, utils::default};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
    actor::{
        unit::{UnitBundle, UnitController, UnitControllerInfo},
        ActorBundle, ActorId,
    },
    ActorBuilder, ActorKindId, ActorSpawned, ActorType,
};

use super::ChampionBundle;

pub struct Champ1Builder;

#[derive(Debug, Serialize, Deserialize)]
pub struct Champ1Data {
    pub controller: UnitControllerInfo,
}

#[derive(Bundle)]
pub struct Champ1Bundle {
    unit: ChampionBundle,
}

impl Champ1Data {
    pub const ID: ActorKindId = ActorKindId(
        match Uuid::try_parse("e47178b3-8ebd-4652-bc0a-55c656a0f8d6") {
            Ok(v) => v,
            Err(e) => panic!("Invalid UUID"),
        },
    );
}

impl ActorBuilder for Champ1Builder {
    fn actor_kind_id(&self) -> ActorKindId {
        Champ1Data::ID
    }

    fn build(
        &self,
        data: crate::ActorSpawned,
        asset_server: &AssetServer,
        commands: &mut bevy::prelude::Commands,
    ) {
        let mesh = Cuboid::new(1.0, 1.0, 2.0).mesh().build();
        let mesh = asset_server.add(mesh);
        let mat = StandardMaterial::from_color(Color::WHITE);
        let mat = asset_server.add(mat);

        let champ_1_data: Champ1Data = serde_json::from_slice(&data.data).unwrap();
        commands.spawn(Champ1Bundle {
            unit: ChampionBundle {
                unit: UnitBundle {
                    actor: ActorBundle {
                        mesh_bundle: MaterialMeshBundle {
                            mesh,
                            material: mat,
                            ..default()
                        },
                        actor: data.actor_id,
                    },
                    unit: crate::actor::unit::Unit,
                    controller: match champ_1_data.controller {
                        UnitControllerInfo::Player(id) => UnitController::Player(id),
                    },
                },
            },
        });
    }
}
