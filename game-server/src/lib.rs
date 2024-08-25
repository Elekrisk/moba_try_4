use lobby_server::{PlayerId, PlayerToken};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameToGameServer {
    Identify { id: PlayerId, token: PlayerToken },
}
