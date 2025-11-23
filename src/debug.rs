use avian3d::prelude::*;
use bevy::prelude::*;

use crate::character_controller::{CharacterController, CharacterControllerState};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(Startup, setup)
        .add_systems(Update, update_debug_text);
}

fn setup(mut commands: Commands) {
    commands.spawn((Node::default(), Text::default(), DebugText));
    commands.spawn((
        Node {
            justify_self: JustifySelf::End,
            justify_content: JustifyContent::End,
            ..default()
        },
        Text::new(
            [
                "Controls:",
                "WASD: move",
                "Space: jump",
                "Space (hold): autohop",
                "Ctrl: crouch",
                "Wheel: zoom",
                "Esc: free mouse",
                "R: reset position",
            ]
            .join("\n"),
        ),
    ));
}

fn update_debug_text(
    mut text: Single<&mut Text, With<DebugText>>,
    kcc: Single<(&CharacterControllerState, &ColliderAabb), With<CharacterController>>,
    camera: Single<&Transform, With<Camera>>,
    names: Query<NameOrEntity>,
) {
    let (state, aabb) = kcc.into_inner();
    let velocity = state.velocity;
    let speed = velocity.length();
    let horizontal_speed = velocity.xz().length();
    let camera_position = camera.translation;
    let collisions = names
        .iter_many(state.touching_entities.iter())
        .map(|name| {
            name.name
                .map(|n| format!("{} ({})", name.entity, n))
                .unwrap_or_else(|| format!("{}", name.entity))
        })
        .collect::<Vec<_>>();
    let ground = state
        .grounded
        .and_then(|ground| names.get(ground.entity).ok())
        .map(|name| {
            name.name
                .map(|n| format!("{} ({})", name.entity, n))
                .unwrap_or(format!("{}", name.entity))
        });
    text.0 = format!(
        "Speed: {speed:.3}\nHorizontal Speed: {horizontal_speed:.3}\nVelocity: [{:.3}, {:.3}, {:.3}]\nCamera Position: [{:.3}, {:.3}, {:.3}]\nCollider Aabb:\n  min:[{:.3}, {:.3}, {:.3}]\n  max:[{:.3}, {:.3}, {:.3}]\nCollisions: {:#?}\nGround: {:?}",
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
        aabb.max.z,
        collisions,
        ground
    );
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub(crate) struct DebugText;
