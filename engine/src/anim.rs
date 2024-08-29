use std::time::Duration;

use bevy::{prelude::*, utils::HashMap};

#[derive(Component)]
pub struct AnimationInfo {
    pub animations: HashMap<AnimationKind, AnimationNodeIndex>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnimationKind {
    Idle,
    Moving,
    Custom(String),
}

#[derive(Component)]
pub struct AnimationController {
    next_animation: AnimationNodeIndex,
    pub animation_player_entity: Entity,
}

impl AnimationController {
    pub fn new(animation: AnimationNodeIndex, animation_player_entity: Entity) -> Self {
        Self {
            next_animation: animation,
            animation_player_entity,
        }
    }

    pub fn current_animation(&self) -> AnimationNodeIndex {
        self.next_animation
    }

    pub fn play(&mut self, animation: AnimationNodeIndex) {
        self.next_animation = animation;
    }
}

pub fn apply_animation(
    q: Query<&AnimationController, Changed<AnimationController>>,
    mut p: Query<(&mut AnimationPlayer, &mut AnimationTransitions)>,
) {
    for c in &q {
        println!("Animation player on {}", c.animation_player_entity);
        if let Ok((mut p, mut t)) = p.get_mut(c.animation_player_entity) {
            t.play(&mut p, c.next_animation, Duration::from_secs_f32(0.1)).repeat();
        } else {
            println!("No animation player or animation transitions");
        }
    }
}

pub fn bind_animation_controller(q: Query<Entity, Added<AnimationPlayer>>, p: Query<&Parent>, mut c: Query<&mut AnimationController>) {
    for player_entity in &q {
        let mut cur = player_entity;
        println!("FINDING CONTROLLER ENTITY");
        
        loop {
            if let Ok(mut controller) = c.get_mut(cur) {
                controller.animation_player_entity = player_entity;
                println!("CONTROLLER ENTITY {player_entity} FOUND");
                break;
            }
            
            if let Ok(parent) = p.get(cur) {
                println!("MOVING TO PARENT");
                cur = parent.get();
            } else {
                println!("CONTROLLER ENTITY NOT FOUND");
                break;
            }
        }
    }
}
