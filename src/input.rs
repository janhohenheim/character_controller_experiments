use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    prelude::*,
};
use bevy_enhanced_input::prelude::*;

use crate::Player;

pub(super) fn plugin(app: &mut App) {
    app.register_required_components::<Player, PlayerInput>()
        .add_input_context::<PlayerInput>();
}

#[derive(Component, Default)]
#[component(on_add = PlayerInput::on_add)]
struct PlayerInput;

impl PlayerInput {
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        world.commands().entity(ctx.entity).insert((
            Camera3d::default(),
            actions!(PlayerInput[
                (
                    Action::<Movement>::new(),
                    DeadZone::default(),
                    SmoothNudge::default(),
                    Bindings::spawn((
                        Cardinal::wasd_keys(),
                        Axial::left_stick()
                    ))
                ),
                (
                    Action::<Jump>::new(),
                    bindings![KeyCode::Space, GamepadButton::South],
                )
            ]),
        ));
    }
}

#[derive(Debug, InputAction)]
#[action_output(Vec2)]
pub(crate) struct Movement;

#[derive(Debug, InputAction)]
#[action_output(bool)]
pub(crate) struct Jump;
