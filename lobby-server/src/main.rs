#![feature(is_none_or)]

use std::{
    collections::HashMap, error::Error, fmt::Display, io::BufRead, net::{IpAddr, SocketAddr, TcpListener, TcpStream}, ops::Range, process::{Command, Stdio}, str::FromStr, sync::mpsc
};

use clap::Parser;
use lobby_server::*;
use uuid::Uuid;

#[derive(Debug, clap::Parser)]
struct Options {
    #[arg(long)]
    rewrite_to_localhost: bool,
    #[arg(long, default_value_t = 27300)]
    port: u16,
    #[arg(long, default_value_t = PortRange::new(27301, 27400))]
    game_server_port_range: PortRange,
    #[arg(long, default_value_t = Mode::default())]
    game_server_launch_mode: Mode,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum Mode {
    #[default]
    Cargo,
    Binary,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Mode::Cargo => "cargo",
            Mode::Binary => "binary",
        })
    }
}

fn main() {
    let options = Options::parse();

    let addr: IpAddr = "0.0.0.0".parse().unwrap();
    let server = TcpListener::bind((addr, options.port)).unwrap();

    let mut state = State {
        players: HashMap::new(),
        connections: HashMap::new(),
        lobbies: HashMap::new(),
        options,
    };

    let (s, r) = mpsc::channel();

    std::thread::spawn(move || loop {
        let (stream, addr) = server.accept().unwrap();
        let id = PlayerId(Uuid::new_v4());
        let mut conn = Connection {
            conn: stream.try_clone().unwrap(),
        };
        s.send(ChannelStuff::NewConnection(id, Connection { conn: stream }))
            .unwrap();

        let s = s.clone();
        std::thread::spawn(move || loop {
            match conn.read() {
                Ok(msg) => s.send(ChannelStuff::Message(id, msg)).unwrap(),
                Err(_) => {
                    s.send(ChannelStuff::LostConnection(id)).unwrap();
                    break;
                }
            }
        });
    });

    loop {
        let msg = r.recv().unwrap();
        state.handle_event(msg);
    }
}

enum ChannelStuff {
    NewConnection(PlayerId, Connection),
    LostConnection(PlayerId),
    Message(PlayerId, ClientMessage),
}

#[derive(Debug)]

pub struct State {
    players: HashMap<PlayerId, Player>,
    connections: HashMap<PlayerId, Connection>,
    lobbies: HashMap<LobbyId, Lobby>,
    options: Options,
}

impl State {
    fn handle_event(&mut self, event: ChannelStuff) {
        match event {
            ChannelStuff::NewConnection(id, conn) => {
                self.connections.insert(id, conn);
                self.players.insert(
                    id,
                    Player {
                        id,
                        name: format!("{}", id.0),
                        in_lobby: None,
                    },
                );
                println!("New connection: {}", id.0);
            }
            ChannelStuff::LostConnection(id) => {
                println!("Lost connection: {}", id.0);
                self.connections.remove(&id);
                let player = self.players.remove(&id).unwrap();
                if let Some(lobby_id) = player.in_lobby {
                    let lobby = self.lobbies.get_mut(&lobby_id).unwrap();
                    lobby.remove_player(id);
                    self.broadcast_lobby(lobby_id, ServerMessage::PlayerLeftLobby(id), None);
                }
            }
            ChannelStuff::Message(id, msg) => match msg {
                ClientMessage::SetName(name) => {
                    println!("Player {} has new name {name}", id.0);
                    let player = self.players.get_mut(&id).unwrap();
                    player.name = name.clone();
                    if let Some(lobby) = player.in_lobby {
                        self.broadcast_lobby(lobby, ServerMessage::PlayerNameUpdate(id, name), id);
                    }
                }
                ClientMessage::CreateNewLobby => {
                    if self.players.get(&id).unwrap().in_lobby.is_some() {
                        return;
                    }

                    let mut lobby = Lobby::new();
                    println!("Player {} created new lobby {}", id.0, lobby.id.0);
                    lobby.add_player(id);
                    self.players.get_mut(&id).unwrap().in_lobby = Some(lobby.id);
                    let lobby_id = lobby.id;
                    self.lobbies.insert(lobby_id, lobby);
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::LobbyJoined(lobby_id))
                        .unwrap();
                }
                ClientMessage::JoinLobby(lobby) => {
                    if self.players.get(&id).unwrap().in_lobby.is_some() {
                        return;
                    }

                    println!("Player {} joined lobby {}", id.0, lobby.0);

                    let lobby = self.lobbies.get_mut(&lobby).unwrap();
                    lobby.add_player(id);
                    self.players.get_mut(&id).unwrap().in_lobby = Some(lobby.id);
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::LobbyJoined(lobby.id))
                        .unwrap();
                    let lobby_id = lobby.id;
                    self.broadcast_lobby(lobby_id, ServerMessage::PlayerJoinedLobby(id), id);
                }
                ClientMessage::LeaveLobby => {
                    if self.players.get(&id).unwrap().in_lobby.is_none() {
                        return;
                    }

                    let lobby = self
                        .lobbies
                        .get_mut(&self.players.get(&id).unwrap().in_lobby.unwrap())
                        .unwrap();
                    println!("Player {} left lobby {}", id.0, lobby.id.0);
                    lobby.remove_player(id);
                    self.players.get_mut(&id).unwrap().in_lobby = None;
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::LobbyLeft)
                        .unwrap();
                    let lobby_id = lobby.id;
                    self.broadcast_lobby(lobby_id, ServerMessage::PlayerLeftLobby(id), None);
                }
                ClientMessage::GetLobbyInfo(lobby) => {
                    let lobby = self.lobbies.get(&lobby).unwrap();
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::LobbyInfo(lobby.clone()))
                        .unwrap();
                }
                ClientMessage::GetLobbyList => {
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::LobbyList(
                            self.lobbies
                                .iter()
                                .map(|(id, lobby)| (*id, lobby.players.values().flatten().count()))
                                .collect(),
                        ))
                        .unwrap();
                }
                ClientMessage::SwitchTeam(team) => {
                    let lobby = self
                        .lobbies
                        .get_mut(&self.players.get(&id).unwrap().in_lobby.unwrap())
                        .unwrap();
                    for (_, players) in &mut lobby.players {
                        if let Some(pos) = players.iter().position(|p| *p == id) {
                            players.remove(pos);
                        }
                    }

                    lobby.players.get_mut(&team).unwrap().push(id);
                    let lobby_id = lobby.id;
                    self.broadcast_lobby(lobby_id, ServerMessage::PlayerSwitchedTeam(id), None);
                }
                ClientMessage::GetPlayerInfo(pid) => {
                    println!("STATE: {self:#?}");
                    let player = self.players.get(&pid).unwrap().clone();
                    self.connections
                        .get_mut(&id)
                        .unwrap()
                        .write(&ServerMessage::PlayerInfo {
                            team: player.in_lobby.map(|lid: LobbyId| {
                                self.lobbies
                                    .get(&lid)
                                    .unwrap()
                                    .players
                                    .iter()
                                    .flat_map(|(t, p)| p.iter().map(|p| (*t, *p)))
                                    .find(|(_, p)| *p == pid)
                                    .map(|(t, _)| t)
                                    .unwrap()
                            }),
                            player,
                        })
                        .unwrap();
                }
                ClientMessage::GetMyId => self
                    .connections
                    .get_mut(&id)
                    .unwrap()
                    .write(&ServerMessage::YourPlayerId(id))
                    .unwrap(),
                ClientMessage::StartGame => {
                    let Some(lobby) = self.players.get(&id).unwrap().in_lobby else {
                        return;
                    };
                    let lobby = self.lobbies.get(&lobby).unwrap();

                    // start game server
                    let mut cmd = match self.options.game_server_launch_mode {
                        Mode::Cargo => {
                            let mut cmd = Command::new("cargo");
                            cmd.args([
                                "run",
                                "--bin=game-server",
                                "--release",
                                "--",
                            ]);
                            cmd
                        }
                        Mode::Binary => Command::new("./game-server"),
                    };
                    let mut child = cmd
                        .arg(&self.options.game_server_port_range.to_string())
                        .stdout(Stdio::piped())
                        .spawn()
                        .unwrap();
                    let stdout = child.stdout.take().unwrap();
                    let mut buf = String::new();
                    let mut reader = std::io::BufReader::new(stdout);
                    reader.read_line(&mut buf).unwrap();
                    let buf = buf.trim();
                    println!("Starting server on {}", buf);
                    let mut addr: SocketAddr = buf.parse().unwrap();
                    if self.options.rewrite_to_localhost {
                        addr.set_ip("127.0.0.1".parse().unwrap());
                    }
                    let stream = TcpStream::connect(addr).unwrap();
                    let mut conn = Connection { conn: stream };

                    let mut res = vec![];

                    for (team, players) in &lobby.players {
                        for pid in players {
                            let player = self.players.get(pid).unwrap();
                            let token = PlayerToken(Uuid::new_v4());
                            res.push((*team, player.clone(), token));
                            self.connections
                                .get_mut(&pid)
                                .unwrap()
                                .write(&ServerMessage::JoinGame(addr, token))
                                .unwrap();
                        }
                    }

                    conn.write(&ToGameServer::Prepare { players: res }).unwrap();

                    std::thread::spawn(move || {
                        let mut buf = String::new();
                        while reader.read_line(&mut buf).is_ok_and(|n| n > 0) {
                            print!("{buf}");
                            buf.clear();
                        }
                    });
                }
            },
        }
    }

    fn broadcast_lobby(
        &mut self,
        lobby: LobbyId,
        msg: ServerMessage,
        ignore_player: impl Into<Option<PlayerId>>,
    ) {
        let lobby = self.lobbies.get(&lobby).unwrap();
        let ignore_player: Option<PlayerId> = ignore_player.into();
        for player in lobby
            .players
            .values()
            .flatten()
            .filter(|p| ignore_player.is_none_or(|ignore| **p != ignore))
        {
            self.connections
                .get_mut(player)
                .unwrap()
                .write(&msg)
                .unwrap();
        }
    }
}
