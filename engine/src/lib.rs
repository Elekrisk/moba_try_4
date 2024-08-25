#![feature(never_type)]

mod actor;

use std::{any::Any, sync::mpsc};

use actor::{
    unit::{
        champion::champ_1::{Champ1Builder, Champ1Data},
        UnitControllerInfo,
    },
    ActorId,
};
use bevy::{prelude::*, utils::HashMap};
use lobby_server::{Connection, PlayerId, Team};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub enum Mode {
    Client,
    Server,
}

#[derive(Resource)]
pub struct GameServerConn(pub Connection);

pub struct GameEventReader(mpsc::Receiver<GameEvent>);

#[derive(Resource)]
pub struct GameClientConns(pub HashMap<PlayerId, Connection>);

pub struct GamePlugin<T: States + Copy> {
    pub mode: Mode,
    pub run_in_state: T,
}

impl<T: States + Copy> Plugin for GamePlugin<T> {
    fn build(&self, app: &mut App) {
        match self.mode {
            Mode::Client => {
                app.add_systems(OnEnter(self.run_in_state), client_setup);
                app.add_systems(FixedUpdate, read_game_events.run_if(in_state(self.run_in_state)));
            }
            Mode::Server => {
                app.add_systems(OnEnter(self.run_in_state), server_setup);
            }
        }

        app.insert_resource(ActorKindRegistry::setup());
    }
}

fn client_setup(asset_server: Res<AssetServer>, mut commands: Commands) {
    commands.spawn(Camera3dBundle {
        projection: Projection::Perspective(PerspectiveProjection {
            fov: std::f32::consts::FRAC_PI_2,
            ..default()
        }),
        transform: Transform::from_xyz(0.0, -10.0, 10.0).looking_at(Vec3::ZERO, Vec3::Z),
        ..default()
    });
    
    commands.add(|world: &mut World| {
        let conn = world.get_resource::<GameServerConn>().unwrap();
        let mut conn = Connection { conn: conn.0.conn.try_clone().unwrap() };
        let (s, r) = mpsc::channel();
        std::thread::spawn(move || loop {
            let msg = conn.read::<GameEvent>().unwrap();
            s.send(msg).unwrap();
        });
        world.insert_non_send_resource(GameEventReader(r));
    });
}

fn read_game_events(reader: NonSend<GameEventReader>, actor_registry: Res<ActorKindRegistry>, asset_server: Res<AssetServer>, mut commands: Commands) {
    while let Ok(event) = reader.0.try_recv() {
        match event {
            GameEvent::ActorSpawned(spawn) => {
                if let Some(builder) = actor_registry.map.get(&spawn.kind_id) {
                    builder.build(spawn, &asset_server, &mut commands)
                }
            },
        }
    }
}

fn server_setup(mut connections: ResMut<GameClientConns>, mut commands: Commands) {
    // spawn champ1 controlled by no player
    for (player, conn) in &mut connections.0 {
        let data = Champ1Data {
            controller: UnitControllerInfo::Player(*player),
        };
        let spawn = ActorSpawned {
            actor_id: ActorId(Uuid::new_v4()),
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
        };

        conn.write(&GameEvent::ActorSpawned(spawn)).unwrap();
    }
}

#[derive(Component)]
pub struct Actor;

#[derive(Debug, Serialize, Deserialize)]
pub enum GameEvent {
    ActorSpawned(ActorSpawned),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ActorSpawned {
    actor_id: ActorId,
    actor_type: ActorType,
    kind_id: ActorKindId,
    data: Vec<u8>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OtherData {
    Bytes(Vec<u8>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorKindId(pub Uuid);

#[derive(Debug, Serialize, Deserialize)]
pub enum ActorType {
    Unit(UnitData),
    Projectile(ProjectileData),
    Other(OtherData),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UnitData {
    team: Team,
    stats: Stats,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProjectileData {}

#[derive(Resource)]
pub struct ActorKindRegistry {
    map: HashMap<ActorKindId, Box<dyn ActorBuilder>>,
}

impl ActorKindRegistry {
    fn setup() -> Self {
        let mut registry = Self {
            map: HashMap::new()
        };

        registry.add(Champ1Builder);

        registry
    }

    fn add(&mut self, builder: impl ActorBuilder + 'static) {
        self.map.insert(builder.actor_kind_id(), Box::new(builder));
    }
}

pub trait ActorBuilder: Send + Sync {
    fn actor_kind_id(&self) -> ActorKindId;
    fn build(&self, data: ActorSpawned, asset_server: &AssetServer, commands: &mut Commands);
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Stats {
    max_health: f32,
    max_mana: f32,
    movement_speed: f32,
    attack_speed: f32,
    attr_a: f32,
    attr_b: f32,
    attr_c: f32,
    resist_a: f32,
    resist_b: f32,
    resist_c: f32,
}




fn client_control() {
    
}
