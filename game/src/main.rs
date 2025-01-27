#![feature(array_windows)]
#![feature(trivial_bounds)]
#![feature(closure_lifetime_binder)]
#![feature(let_chains)]

mod button;
mod game;
mod lobby;
mod terrain_editor;

use std::{
    net::{SocketAddr, TcpStream},
    sync::mpsc,
};

use bevy::{prelude::*, utils::hashbrown::HashMap};
use button::{ButtonAction, ButtonPlugin, UiMyButtonExt};
use clap::Parser;
use engine::{
    terrain::{create_path_graph, Terrain},
    GamePlugin, GameServerConn,
};
use game::ClientGamePlugin;
use game_server::GameToGameServer;
use leafwing_input_manager::plugin::InputManagerPlugin;
use lobby::{LobbyMenuPlugin, MyPlayerId};
use lobby_server::{
    ClientMessage, Connection, Lobby, LobbyId, Player, PlayerId, PlayerToken, ServerMessage, Team,
};
use parry2d::na::Point;
use sickle_ui::{prelude::*, widgets::layout::label::SetLabelTextExt, SickleUiPlugin};
use terrain_editor::TerrainEditorPlugin;
use uuid::Uuid;

#[derive(clap::Parser, Resource, Clone)]
struct Options {
    username: String,
    #[arg(long)]
    direct_connect: Option<SocketAddr>,
    #[arg(long)]
    skip_lobby: bool,
    #[arg(long, default_value_t = String::from("localhost:27300"))]
    lobby_ip: String
}

fn main() {
    let options = Options::parse();

    let mut app = App::new();

    app.add_plugins(DefaultPlugins)
        .insert_resource(Time::<Fixed>::from_hz(60.0))
        .add_plugins(SickleUiPlugin)
        .add_plugins((ClientGamePlugin, TerrainEditorPlugin))
        .add_plugins(LobbyMenuPlugin(options.clone()))
        .add_plugins(GamePlugin {
            mode: engine::Mode::Client,
            run_in_state: TopLevelState::InGame,
        });

    match options.direct_connect {
        Some(addr) => {
            app.insert_state(TopLevelState::InGame);
            let mut conn = Connection {
                conn: TcpStream::connect(addr).unwrap(),
            };
            let id = PlayerId(Uuid::new_v4());
            conn.write(&GameToGameServer::Identify {
                id,
                token: PlayerToken(Uuid::nil()),
            })
            .unwrap();
            app.insert_resource(GameServerConn(conn))
                .insert_resource(MyPlayerId(id));
        }
        None => {
            app.insert_state(TopLevelState::LobbyMenu);

            if options.skip_lobby {
                app.insert_resource(SkipLobby);
            }
        }
    }

    app.run();
}

#[derive(Resource)]
struct SkipLobby;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States)]
enum TopLevelState {
    LobbyMenu,
    InGame,
}
