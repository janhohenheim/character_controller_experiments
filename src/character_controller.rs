use bevy::prelude::*;
use bevy_enhanced_input::prelude::*;

use crate::input::{Jump, Movement};

pub(super) fn plugin(app: &mut App) {
    app.add_observer(apply_movement).add_observer(apply_jump);
}

fn apply_movement(movement: On<Fire<Movement>>) {
    info!("Movement applied: {}", movement.value);
}

fn apply_jump(jump: On<Fire<Jump>>) {
    info!("Jump applied");
}
