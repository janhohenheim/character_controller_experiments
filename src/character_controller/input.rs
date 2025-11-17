use bevy::prelude::*;
use bevy_enhanced_input::prelude::*;

pub(super) fn plugin(app: &mut App) {
    app.add_observer(apply_movement)
        .add_observer(apply_jump)
        .add_observer(apply_crouch)
        .add_systems(
            RunFixedMainLoop,
            clear_accumulated_input.in_set(RunFixedMainLoopSystems::AfterFixedMainLoop),
        );
}

#[derive(Debug, InputAction)]
#[action_output(Vec2)]
pub(crate) struct Movement;

#[derive(Debug, InputAction)]
#[action_output(bool)]
pub(crate) struct Jump;

#[derive(Debug, InputAction)]
#[action_output(bool)]
pub(crate) struct Crouch;

/// Input accumulated since the last fixed update loop. Is cleared after every fixed update loop.
#[derive(Component, Clone, Copy, Reflect, Default, Debug)]
#[reflect(Component)]
pub(crate) struct AccumulatedInput {
    // The last non-zero move that was input since the last fixed update loop
    pub(crate) last_movement: Option<Vec2>,
    // Whether any frame since the last fixed update loop input a jump
    pub(crate) jumped: bool,
    // Whether any frame since the last fixed update loop input a crouch
    pub(crate) crouched: bool,
}

fn apply_movement(
    movement: On<Fire<Movement>>,
    mut accumulated_inputs: Query<&mut AccumulatedInput>,
) {
    if let Ok(mut accumulated_inputs) = accumulated_inputs.get_mut(movement.context) {
        accumulated_inputs.last_movement = Some(movement.value);
    }
}

fn apply_jump(jump: On<Fire<Jump>>, mut accumulated_inputs: Query<&mut AccumulatedInput>) {
    if let Ok(mut accumulated_inputs) = accumulated_inputs.get_mut(jump.context) {
        accumulated_inputs.jumped = true;
    }
}

fn apply_crouch(crouch: On<Fire<Crouch>>, mut accumulated_inputs: Query<&mut AccumulatedInput>) {
    if let Ok(mut accumulated_inputs) = accumulated_inputs.get_mut(crouch.context) {
        accumulated_inputs.crouched = true;
    }
}

fn clear_accumulated_input(mut accumulated_inputs: Query<&mut AccumulatedInput>) {
    for mut accumulated_input in &mut accumulated_inputs {
        *accumulated_input = default();
    }
}
