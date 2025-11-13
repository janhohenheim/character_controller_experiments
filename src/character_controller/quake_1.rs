use avian3d::prelude::LinearVelocity;
use bevy::prelude::*;

use crate::character_controller::{
    CharacterController, CharacterControllerState, CharacterControllerSystems,
    input::AccumulatedInput,
};

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        FixedPostUpdate,
        (run_kcc, print_input).in_set(CharacterControllerSystems::ApplyMovement),
    );
}

fn print_input(input: Query<&AccumulatedInput>) {
    for input in input.iter() {
        println!("Input: {:?}", input);
    }
}

fn run_kcc(
    world: &mut World,
    mut kccs: Local<
        QueryState<(
            Entity,
            &CharacterController,
            &CharacterControllerState,
            &AccumulatedInput,
            &LinearVelocity,
        )>,
    >,
    mut input_scratch: Local<Vec<KccInput>>,
) {
    let dt = world.resource::<Time>().delta_secs();
    input_scratch.extend(
        kccs.iter(world)
            .map(|(entity, cfg, state, input, vel)| KccInput {
                entity,
                cfg: *cfg,
                state: *state,
                input: *input,
                dt,
                velocity: **vel,
            }),
    );
    for input in input_scratch.drain(..) {
        let velocity: Vec3 = match world.run_system_cached_with(air_move, input) {
            Ok(state) => state,
            Err(err) => {
                error!("Error running air_move system: {}", err);
                continue;
            }
        };
        **world
            .entity_mut(input.entity)
            .get_mut::<LinearVelocity>()
            .unwrap() = velocity;
    }
}

#[derive(Debug, Clone, Copy)]
struct KccInput {
    entity: Entity,
    cfg: CharacterController,
    state: CharacterControllerState,
    input: AccumulatedInput,
    velocity: Vec3,
    dt: f32,
}

fn air_move(In(inp): In<KccInput>, world: &mut World) -> Result<Vec3> {
    let KccInput {
        entity,
        cfg,
        state,
        input,
        dt,
        mut velocity,
    } = inp;

    let movement = input.last_movement.unwrap_or_default();
    let cfg_speed = cfg.speed.normalize_or_zero();
    let mut wish_vel =
        cfg_speed.y * movement.y * Vec3::NEG_Z + cfg_speed.x * movement.x * Vec3::NEG_X;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));
    if wish_speed > cfg.max_speed {
        wish_vel *= cfg.max_speed / wish_speed;
        wish_speed = cfg.max_speed;
    }

    if state.grounded.is_some() {
        velocity.z = 0.0;
        velocity = accelerate(wish_dir, wish_speed, velocity, cfg, dt);
        velocity.z -= cfg.gravity * dt;
        ground_move();
    } else {
        velocity = air_accelerate(wish_dir, wish_speed, velocity, cfg, dt);
        velocity.z -= cfg.gravity * dt;
        fly_move()
    }
    Ok(velocity)
}

fn accelerate(
    wish_dir: Dir3,
    wish_speed: f32,
    velocity: Vec3,
    cfg: CharacterController,
    dt: f32,
) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    // right here is where air strafing happens: `current_speed` is close to 0 when we want to move perpendicular to
    // our current velocity, making `add_speed` large.
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = (cfg.acceleration * dt * wish_speed).min(add_speed);
    velocity + accel_speed * wish_dir
}

fn air_accelerate(
    wish_dir: Dir3,
    wish_speed: f32,
    velocity: Vec3,
    cfg: CharacterController,
    dt: f32,
) -> Vec3 {
    let wish_speed = wish_speed.min(cfg.air_speed);
    accelerate(wish_dir, wish_speed, velocity, cfg, dt)
}

fn ground_move() {
    todo!();
}

fn fly_move() {
    todo!();
}

fn friction(
    velocity: Vec3,
    cfg: CharacterController,
    state: CharacterControllerState,
    dt: f32,
) -> Vec3 {
    let speed = velocity.length();
    if speed < 0.025 {
        return Vec3::ZERO;
    }

    // if the leading edge is over a dropoff, increase friction
    if state.grounded.is_some() {
        let start = todo!();
    }
    velocity
}
