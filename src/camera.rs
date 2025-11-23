use std::f32::consts::TAU;

use bevy::{
    input::common_conditions::input_just_pressed,
    prelude::*,
    window::{CursorGrabMode, CursorOptions},
};
use bevy_enhanced_input::prelude::*;

use crate::user_input::{Rotate, Zoom};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        (
            capture_cursor.run_if(input_just_pressed(MouseButton::Left)),
            release_cursor.run_if(input_just_pressed(KeyCode::Escape)),
        ),
    )
    .add_observer(rotate_camera)
    .add_observer(zoom_camera);
}

#[derive(Component, Clone, Reflect, Default, Debug)]
#[reflect(Component)]
pub(crate) struct CharacterCameraState {
    pub(crate) distance: f32,
    pub(crate) pitch: f32,
    pub(crate) yaw: f32,
}

fn rotate_camera(rotate: On<Fire<Rotate>>, mut camera: Single<&mut CharacterCameraState>) {
    let delta = rotate.value;
    camera.yaw += delta.x.to_radians();
    camera.pitch += delta.y.to_radians();
    camera.pitch = camera.pitch.clamp(-TAU / 4.0 + 0.01, TAU / 4.0 - 0.01);
}

fn zoom_camera(zoom: On<Fire<Zoom>>, mut camera: Single<&mut CharacterCameraState>) {
    camera.distance -= zoom.value;
    camera.distance = camera.distance.clamp(0.0, 10.0);
}

fn capture_cursor(mut cursor: Single<&mut CursorOptions>) {
    cursor.grab_mode = CursorGrabMode::Locked;
    cursor.visible = false;
}

fn release_cursor(mut cursor: Single<&mut CursorOptions>) {
    cursor.visible = true;
    cursor.grab_mode = CursorGrabMode::None;
}
