use avian3d::prelude::*;
use bevy::prelude::*;

mod fixed_update_util;
mod input;
mod quake_1;

pub(crate) use input::{AccumulatedInput, Jump, Movement};

pub(super) fn plugin(app: &mut App) {
    app.add_plugins((fixed_update_util::plugin, input::plugin, quake_1::plugin))
        .configure_sets(
            FixedPostUpdate,
            CharacterControllerSystems::ApplyMovement.in_set(PhysicsSystems::First),
        );
}

#[derive(Component, Clone, Copy, Reflect, Debug)]
#[reflect(Component)]
#[require(AccumulatedInput, CharacterControllerState, RigidBody = RigidBody::Kinematic)]
pub(crate) struct CharacterController {
    pub(crate) speed: Vec2,
    pub(crate) air_speed: f32,
    pub(crate) acceleration: f32,
    pub(crate) max_speed: f32,
    pub(crate) gravity: f32,
}

impl Default for CharacterController {
    fn default() -> Self {
        Self {
            speed: vec2(10., 9.),
            air_speed: 0.7,
            acceleration: 0.25,
            max_speed: 8.0,
            gravity: 20.,
        }
    }
}

#[derive(Component, Clone, Copy, Reflect, Default, Debug)]
#[reflect(Component)]
pub(crate) struct CharacterControllerState {
    pub(crate) grounded: Option<Entity>,
}

#[derive(SystemSet, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CharacterControllerSystems {
    ApplyMovement,
}
