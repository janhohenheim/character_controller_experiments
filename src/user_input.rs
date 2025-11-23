use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    prelude::*,
};
use bevy_enhanced_input::prelude::{Release, *};

use crate::character_controller::{Crouch, Jump, Movement};

pub(super) fn plugin(app: &mut App) {
    app.add_input_context::<PlayerInput>();
}

#[derive(Component, Default)]
#[component(on_add = PlayerInput::on_add)]
pub(crate) struct PlayerInput;

#[derive(Debug, InputAction)]
#[action_output(Vec2)]
pub(crate) struct Rotate;

#[derive(Debug, InputAction)]
#[action_output(Vec2)]
pub(crate) struct Reset;

#[derive(Debug, InputAction)]
#[action_output(f32)]
pub(crate) struct Zoom;

impl PlayerInput {
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        world
            .commands()
            .entity(ctx.entity)
            .insert(actions!(PlayerInput[
                (
                    Action::<Movement>::new(),
                    DeadZone::default(),
                    Bindings::spawn((
                        Cardinal::wasd_keys(),
                        Axial::left_stick()
                    ))
                ),
                (
                    Action::<Jump>::new(),
                    bindings![KeyCode::Space, GamepadButton::South],
                ),
                (
                    Action::<Crouch>::new(),
                    bindings![KeyCode::ControlLeft, GamepadButton::LeftTrigger],
                ),
                (
                    Action::<Reset>::new(),
                    bindings![KeyCode::KeyR, GamepadButton::Select],
                    Release::default(),
                ),
                (Action::<Rotate>::new(),Negate::all(), Scale::splat(0.1),
                    Bindings::spawn((Spawn(Binding::mouse_motion()), Axial::right_stick()))),
                (
                    Action::<Zoom>::new(),
                    bindings![(Binding::mouse_wheel(), SwizzleAxis::YXZ)],
                )
            ]));
    }
}
