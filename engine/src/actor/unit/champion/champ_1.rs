use std::time::Duration;

use bevy::{prelude::*, scene::SceneInstance, utils::default};
use lobby_server::{PlayerId, Team};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
    actor::{
        actor_bundle, health::Health, projectile::{Projectile, ProjectileHit, ProjectileSpeed, ProjectileTarget}, unit::{attack::{AttackEvent, AttackType}, unit_bundle, ServerUnitBundle, Unit, UnitBundle, UnitController, UnitControllerInfo}, ActorBundle, ActorId, ActorMap, MovementPath, MovementTarget, ServerActorBundle
    }, actor_id, anim::{AnimationController, AnimationInfo, AnimationKind}, terrain::M, ActorBuilder, ActorKindId, ActorSpawned, ActorSpawner, ActorType, GameClientConns, GameEvent, Stats, UnitData
};

use super::{ChampionBundle, ServerChampionBundle};

pub struct Champ1Spawner;

fn bundle(id: ActorId, transform: Transform, player_id: PlayerId, team: Team) -> impl Bundle {
    (
        unit_bundle(id, transform, UnitController::Player(player_id), team),
        AttackType::Ranged,
        Health { health: 100.0 }
    )
}

impl ActorSpawner for Champ1Spawner {
    type Data = Champ1Data;
    type SendData = Champ1Data;

    fn spawn(&self, pos: Vec2, data: Self::Data, commands: &mut Commands) -> ActorSpawned {
        let actor_id = ActorId(Uuid::new_v4());
        commands.spawn((bundle(
            actor_id,
            Transform::from_translation(pos.extend(0.0)),
            match data.controller {
                UnitControllerInfo::Player(id) => id,
            },
            data.team,
        ),)).observe(|trigger: Trigger<AttackEvent>, actor: Query<&ActorId>, trans: Query<&Transform>, mut commands: Commands, mut connections: ResMut<GameClientConns>| {
            let ev = trigger.event();
            let target = actor.get(ev.0).unwrap();
            let trans = trans.get(trigger.entity()).unwrap();
            let spawn = AutoSpawner.spawn(trans.translation.xy(), *target, &mut commands);
            connections.broadcast(&GameEvent::ActorSpawned(spawn));
        });

        ActorSpawned {
            actor_id,
            pos,
            actor_type: ActorType::Unit(UnitData {
                team: Team(0),
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
    pub team: Team,
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
        commands: &mut bevy::prelude::Commands,
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
                bundle(
                    data.actor_id,
                    Transform::from_translation(data.pos.extend(0.0)).looking_at(Vec3::Y, Vec3::Z),
                    match champ_1_data.controller {
                        UnitControllerInfo::Player(id) => id,
                    },
                    champ_1_data.team,
                ),
                VisibilityBundle::default(),
                asset_server.load::<Scene>(GltfAssetLabel::Scene(0).from_asset("champ1.glb")),
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





struct AutoSpawner;

impl AutoSpawner {
    actor_id!(ID = "e7abf683-1ff6-495a-93c2-9fd524f7d494");
}

impl ActorSpawner for AutoSpawner {
    type Data = ActorId;

    type SendData = ActorId;

    fn spawn(&self, pos: Vec2, data: Self::Data, commands: &mut Commands) -> ActorSpawned {
        let id = ActorId(Uuid::new_v4());

        commands.spawn((
            actor_bundle(id, Transform::from_translation(pos.extend(0.0))),
            Projectile,
            ProjectileTarget::Actor(data),
            ProjectileSpeed(3.0),
        ));

        ActorSpawned { actor_id: id, pos, actor_type: ActorType::Projectile(crate::ProjectileData {  }), kind_id: Self::ID, data: serde_json::to_vec(&data).unwrap() }
    }
}

pub struct AutoBuilder;

impl ActorBuilder for AutoBuilder {
    fn actor_kind_id(&self) -> ActorKindId {
        AutoSpawner::ID
    }

    fn build(&self, data: ActorSpawned, asset_server: &AssetServer, commands: &mut Commands) {
        let target: ActorId = serde_json::from_slice(&data.data).unwrap();
        commands.spawn((
            actor_bundle(data.actor_id, Transform::from_translation(data.pos.extend(1.0))),
            Projectile,
            ProjectileTarget::Actor(target),
            ProjectileSpeed(3.0),
            VisibilityBundle::default(),
            asset_server.add(Sphere::new(0.5).mesh().build()),
            asset_server.add(StandardMaterial::from_color(Color::srgb(1.0, 0.5, 0.5))),
        )).observe(|trigger: Trigger<ProjectileHit>, actor_map: Res<ActorMap>, mut hp: Query<&mut Health>, mut commands: Commands| {
            let ev = trigger.event();
            let hit_entity = actor_map.0.get(&ev.0).unwrap();
            if let Ok(mut hp) = hp.get_mut(*hit_entity) {
                hp.health -= 10.0;
            }
            commands.entity(trigger.entity()).despawn_recursive();
        });
    }
}
