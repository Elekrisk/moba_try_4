use std::time::Duration;

use bevy::{prelude::*, scene::SceneInstance, utils::default};
use lobby_server::Team;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
    actor::{
        unit::{ServerUnitBundle, UnitBundle, UnitController, UnitControllerInfo},
        ActorBundle, ActorId, ServerActorBundle,
    },
    actor_id,
    anim::{AnimationController, AnimationInfo, AnimationKind},
    terrain::M,
    ActorBuilder, ActorKindId, ActorSpawned, ActorSpawner, ActorType, Stats, UnitData,
};

use super::{ChampionBundle, ServerChampionBundle};

pub struct Champ1Spawner;

impl ActorSpawner for Champ1Spawner {
    type Data = Champ1Data;

    fn spawn(&self, pos: Vec2, data: Self::Data, commands: &mut Commands) -> ActorSpawned {
        let actor_id = ActorId(Uuid::new_v4());
        commands.spawn(ServerChamp1Bundle {
            champ: ServerChampionBundle {
                unit: ServerUnitBundle {
                    actor: ServerActorBundle {
                        transform: TransformBundle::from_transform(Transform::from_translation(
                            pos.extend(0.0),
                        )),
                        actor: actor_id,
                        ..default()
                    },
                    unit: crate::actor::unit::Unit,
                    controller: match data.controller {
                        UnitControllerInfo::Player(id) => UnitController::Player(id),
                    },
                },
            },
        });

        ActorSpawned {
            actor_id,
            pos,
            actor_type: ActorType::Unit(UnitData {
                team: Team::Red,
                stats: Stats {
                    max_health: 1.0,
                    max_mana: 1.0,
                    movement_speed: 1.0,
                    attack_speed: 1.0,
                    attr_a: 0.0,
                    attr_b: 0.0,
                    attr_c: 0.0,
                    resist_a: 0.0,
                    resist_b: 0.0,
                    resist_c: 0.0,
                },
            }),
            kind_id: Champ1Data::ID,
            data: serde_json::to_vec(&data).unwrap(),
        }
    }
}

pub struct Champ1Builder;

#[derive(Debug, Serialize, Deserialize)]
pub struct Champ1Data {
    pub controller: UnitControllerInfo,
}

#[derive(Default, Bundle)]
pub struct Champ1Bundle {
    champ: ChampionBundle,
}

#[derive(Bundle)]
pub struct ServerChamp1Bundle {
    champ: ServerChampionBundle,
}

impl Champ1Data {
    // pub const ID: ActorKindId = ActorKindId(
    //     match Uuid::try_parse("e47178b3-8ebd-4652-bc0a-55c656a0f8d6") {
    //         Ok(v) => v,
    //         Err(e) => panic!("Invalid UUID"),
    //     },
    // );
    actor_id!(ID = "e47178b3-8ebd-4652-bc0a-55c656a0f8d6");
}

impl ActorBuilder for Champ1Builder {
    fn actor_kind_id(&self) -> ActorKindId {
        Champ1Data::ID
    }

    fn build(
        &self,
        data: crate::ActorSpawned,
        asset_server: &AssetServer,
        mut commands: &mut bevy::prelude::Commands,
    ) {
        let base_path = "champ1.glb";
        let mut graph = AnimationGraph::new();
        let mut anim = |id| {
            graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(id).from_asset(base_path)),
                1.0,
                graph.root,
            )
        };
        let idle = anim(0);
        let walking = anim(1);
        let graph = asset_server.add(graph);

        let champ_1_data: Champ1Data = serde_json::from_slice(&data.data).unwrap();
        commands
            .spawn((
                Champ1Bundle {
                    champ: ChampionBundle {
                        unit: UnitBundle {
                            actor: ActorBundle {
                                actor: data.actor_id,
                                ..default()
                            },
                            unit: crate::actor::unit::Unit,
                            controller: match champ_1_data.controller {
                                UnitControllerInfo::Player(id) => UnitController::Player(id),
                            },
                        },
                    },
                },
                SceneBundle {
                    scene: asset_server.load(GltfAssetLabel::Scene(0).from_asset("champ1.glb")),
                    transform: Transform::from_translation(data.pos.extend(0.0))
                        .looking_to(Vec3::Y, Vec3::Z),
                    ..default()
                },
                AnimationInfo {
                    animations: [
                        (AnimationKind::Idle, idle),
                        (AnimationKind::Moving, walking),
                    ]
                    .into(),
                },
                AnimationController::new(idle, Entity::PLACEHOLDER),
            ))
            .observe(
                |trigger: Trigger<OnAdd, Children>,
                 q: Query<&Children>,
                 mut qc: Query<&mut Transform>| {
                    let e = trigger.entity();
                    for child in q.get(e).unwrap() {
                        let mut trans = qc.get_mut(*child).unwrap();
                        trans.rotate_axis(Dir3::Y, std::f32::consts::PI);
                    }
                },
            );
        commands.observe(
            move |trigger: Trigger<OnAdd, AnimationPlayer>, mut commands: Commands| {
                commands
                    .entity(trigger.entity())
                    .insert(graph.clone())
                    .insert(AnimationTransitions::new());
            },
        );
    }
}
