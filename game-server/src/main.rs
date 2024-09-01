#![feature(is_none_or)]

use std::{io::ErrorKind, net::TcpListener, sync::{atomic::{AtomicUsize, Ordering}, mpsc}, time::Duration};

use bevy::{
    app::ScheduleRunnerPlugin, prelude::*, state::app::StatesPlugin, utils::hashbrown::HashMap,
};
use clap::Parser;
use engine::{GameClientConns, GamePlugin, PlayerTeams};
use game_server::GameToGameServer;
use lobby_server::{Connection, Player, PlayerId, PortRange, Team, ToGameServer};

#[derive(clap::Parser)]
struct Options {
    #[arg(default_value_t = PortRange::single(0))]
    port: PortRange,
    #[arg(long)]
    direct_connect: bool,
    #[arg(long, default_value_t = 1)]
    player_count: usize,
}

fn main() {
    let options = Options::parse();

    let server = 'block: {
        for port in options.port.range() {
            match TcpListener::bind(("0.0.0.0", port)) {
                Ok(server) => break 'block server,
                Err(e) if e.kind() == ErrorKind::AddrInUse => {
                    continue;
                }
                Err(e) => {
                    println!("{e}");
                    break;
                }
            };
        }
        return;
    };

    let public_ip = reqwest::blocking::get("http://api.ipify.org")
        .unwrap()
        .text()
        .unwrap();

    let mut addr = server.local_addr().unwrap();
    addr.set_ip(public_ip.parse().unwrap());

    println!("{}", addr);

    let mut connections: HashMap<PlayerId, Connection> = HashMap::new();
    let mut players: HashMap<PlayerId, lobby_server::Player> = HashMap::new();
    let mut player_teams: HashMap<PlayerId, Team> = HashMap::new();

    if !options.direct_connect {
        // First connection is from lobby server
        let (stream, addr) = server.accept().unwrap();

        let ToGameServer::Prepare {
            players: players_list,
        } = Connection { conn: stream }.read().unwrap();

        let tokens = players_list
            .iter()
            .map(|(_, a, b)| (a.id, *b))
            .collect::<HashMap<_, _>>();

        player_teams = players_list.iter().map(|(t, p, _)| (p.id, *t)).collect();

        players = players_list
            .into_iter()
            .map(|(_, b, _)| (b.id, b))
            .collect();

        // Wait for players to connect

        loop {
            let (stream, addr) = server.accept().unwrap();
            let mut conn = Connection { conn: stream };

            let GameToGameServer::Identify { id, token } = conn.read().unwrap();
            let Some(player) = players.get(&id) else {
                eprintln!("Invalid player {}", id.0);
                continue;
            };

            if tokens.get(&id).copied().unwrap() != token {
                eprintln!(
                    "Invalid token {} for player {}; expected {}",
                    token.0,
                    player.name,
                    tokens.get(&id).unwrap().0
                );
                continue;
            }

            eprintln!("Player {} registered", players.get(&id).unwrap().name);
            connections.insert(id, conn);

            if connections.len() == players.len() {
                break;
            }
        }
    } else {
        for i in 0..options.player_count {
            let (stream, addr) = server.accept().unwrap();
            let mut conn = Connection { conn: stream };

            let GameToGameServer::Identify { id, token: _ } = conn.read().unwrap();
            connections.insert(id, conn);
            players.insert(
                id,
                Player {
                    id,
                    name: format!("Guest {i}"),
                    in_lobby: None,
                },
            );
        }
    }

    eprintln!("All players ready!");

    let (s, r) = mpsc::channel();

    for (id, mut conn) in &connections {
        let s = s.clone();
        let mut conn = Connection {
            conn: conn.conn.try_clone().unwrap(),
        };
    }

    App::new()
        .add_plugins((
            MinimalPlugins.set(ScheduleRunnerPlugin::run_loop(Duration::from_secs_f32(
                1.0 / 60.0,
            ))),
            StatesPlugin,
            GamePlugin {
                mode: engine::Mode::Server,
                run_in_state: State::Running,
            },
        ))
        .insert_non_send_resource(Channel(r))
        .insert_resource(PlayerCount(players.len()))
        .insert_resource(PlayerTeams(player_teams))
        .insert_resource(GameClientConns(connections))
        .insert_state(State::Running)
        .add_systems(Update, exit_on_all_conn_lost)
        .run();

    println!("All players disconnected - exiting...");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States)]
pub enum State {
    Running,
}

struct Channel(mpsc::Receiver<()>);
#[derive(Resource)]
struct PlayerCount(usize);

fn exit_on_all_conn_lost(
    channel: NonSend<Channel>,
    mut count: ResMut<PlayerCount>,
    mut exit: EventWriter<AppExit>,
) {
    while channel.0.try_recv().is_ok() {
        count.0 -= 1;
    }

    if count.0 == 0 {
        eprintln!("EXITING");
        exit.send(AppExit::Success);
    }
}
