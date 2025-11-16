use avian3d::prelude::{ColliderAabb, LinearVelocity};
use bevy::prelude::*;

use crate::character_controller::CharacterController;

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Startup, setup)
        .add_systems(Update, update_debug_text);
}

fn setup(mut commands: Commands) {
    commands.spawn((Node::default(), Text::default(), DebugText));
}

fn update_debug_text(
    mut text: Single<&mut Text, With<DebugText>>,
    kcc: Single<(&LinearVelocity, &ColliderAabb), With<CharacterController>>,
    camera: Single<&Transform, With<Camera>>,
) {
    let (velocity, aabb) = kcc.into_inner();
    let velocity = velocity.0;
    let speed = velocity.length();
    let camera_position = camera.translation;
    text.0 = format!(
        "Speed: {speed:.3}\nVelocity: [{:.3}, {:.3}, {:.3}]\nCamera Position: [{:.3}, {:.3}, {:.3}]\nCollider Aabb:\n  min:[{:.3}, {:.3}, {:.3}]\n  max:[{:.3}, {:.3}, {:.3}]",
        velocity.x,
        velocity.y,
        velocity.z,
        camera_position.x,
        camera_position.y,
        camera_position.z,
        aabb.min.x,
        aabb.min.y,
        aabb.min.z,
        aabb.max.x,
        aabb.max.y,
        aabb.max.z
    );
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub(crate) struct DebugText;
