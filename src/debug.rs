use avian3d::prelude::{ColliderAabb, CollidingEntities, LinearVelocity};
use bevy::prelude::*;

use crate::{
    Player,
    character_controller::{CharacterController, CharacterControllerState},
};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Startup, setup)
        .add_systems(Update, update_debug_text);
    app.register_required_components::<Player, CollidingEntities>();
}

fn setup(mut commands: Commands) {
    commands.spawn((Node::default(), Text::default(), DebugText));
}

fn update_debug_text(
    mut text: Single<&mut Text, With<DebugText>>,
    kcc: Single<
        (
            &CharacterControllerState,
            &LinearVelocity,
            &ColliderAabb,
            &CollidingEntities,
        ),
        With<CharacterController>,
    >,
    camera: Single<&Transform, With<Camera>>,
    names: Query<NameOrEntity>,
) {
    let (state, velocity, aabb, collisions) = kcc.into_inner();
    let velocity = velocity.0;
    let speed = velocity.length();
    let wish_velocity = state.velocity;
    let wish_speed = wish_velocity.length();
    let camera_position = camera.translation;
    let collisions = names
        .iter_many(collisions.iter())
        .map(|name| {
            name.name
                .map(|n| format!("{} ({})", name.entity, n))
                .unwrap_or_else(|| format!("{}", name.entity))
        })
        .collect::<Vec<_>>();
    text.0 = format!(
        "Speed: {speed:.3}\nVelocity: [{:.3}, {:.3}, {:.3}]\nWish Speed: {wish_speed:.3}\nWish Velocity: [{:.3}, {:.3}, {:.3}]\nCamera Position: [{:.3}, {:.3}, {:.3}]\nCollider Aabb:\n  min:[{:.3}, {:.3}, {:.3}]\n  max:[{:.3}, {:.3}, {:.3}]\nCollisions: {:#?}",
        velocity.x,
        velocity.y,
        velocity.z,
        wish_velocity.x,
        wish_velocity.y,
        wish_velocity.z,
        camera_position.x,
        camera_position.y,
        camera_position.z,
        aabb.min.x,
        aabb.min.y,
        aabb.min.z,
        aabb.max.x,
        aabb.max.y,
        aabb.max.z,
        collisions
    );
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub(crate) struct DebugText;
