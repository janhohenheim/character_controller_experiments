use avian3d::prelude::*;
use bevy::{ecs::system::SystemState, prelude::*};

use crate::character_controller::{
    CharacterController, CharacterControllerState, CharacterControllerSystems,
    input::AccumulatedInput, readonly_spatial_query::ReadOnlySpatialQuery,
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
            &GlobalTransform,
            &Collider,
            &ColliderAabb,
        )>,
    >,
    mut scratch: Local<Vec<(Transform, Vec3, Ctx)>>,
    mut transform_helper: Local<SystemState<TransformHelper>>,
) {
    let dt = world.resource::<Time>().delta_secs();
    scratch.extend(kccs.iter(world).map(
        |(entity, cfg, state, input, vel, transform, collider, aabb)| {
            (
                transform.compute_transform(),
                vel.0,
                Ctx {
                    entity,
                    cfg: cfg.clone(),
                    state: *state,
                    input: *input,
                    dt,
                    aabb: *aabb,
                    collider: collider.clone(),
                },
            )
        },
    ));
    for (transform, velocity, ctx) in scratch.drain(..) {
        let entity = ctx.entity;
        let (transform, velocity): (Transform, Vec3) =
            match world.run_system_cached_with(player_move, (transform, velocity, ctx)) {
                Ok(val) => val,
                Err(err) => {
                    error!("Error running air_move system: {}", err);
                    continue;
                }
            };
        let global_transform = match transform_helper.get(world).compute_global_transform(entity) {
            Ok(global_transform) => global_transform,
            Err(err) => {
                error!("Error computing global transform: {err}. Skipping entity.");
                transform.into()
            }
        };
        **world
            .entity_mut(entity)
            .get_mut::<LinearVelocity>()
            .unwrap() = velocity;
        *world.entity_mut(entity).get_mut::<Transform>().unwrap() = transform;
        *world
            .entity_mut(entity)
            .get_mut::<GlobalTransform>()
            .unwrap() = global_transform;
    }
}

#[derive(Debug)]
struct Ctx {
    entity: Entity,
    cfg: CharacterController,
    state: CharacterControllerState,
    input: AccumulatedInput,
    aabb: ColliderAabb,
    collider: Collider,
    dt: f32,
}

#[must_use]
fn player_move(
    In((mut transform, mut velocity, ctx)): In<(Transform, Vec3, Ctx)>,
    spatial: ReadOnlySpatialQuery,
) -> (Transform, Vec3) {
    // AngleVectors

    // NudgePositions

    // VectorCopy (pmove.cmd.angles, pmove.angles);

    // CategorizePosition

    // jumpbutton()

    velocity = friction(transform, velocity, &ctx, &spatial);
    (transform, velocity) = air_move(transform, velocity, &spatial, &ctx);
    // CategorizePosition
    (transform, velocity)
}

#[must_use]
fn air_move(
    mut transform: Transform,
    mut velocity: Vec3,
    spatial: &ReadOnlySpatialQuery,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    let movement = ctx.input.last_movement.unwrap_or_default();
    let cfg_speed = ctx.cfg.speed.normalize_or_zero();
    let mut wish_vel =
        cfg_speed.y * movement.y * Vec3::NEG_Z + cfg_speed.x * movement.x * Vec3::NEG_X;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));
    if wish_speed > ctx.cfg.max_speed {
        wish_vel *= ctx.cfg.max_speed / wish_speed;
        wish_speed = ctx.cfg.max_speed;
    }

    if ctx.state.grounded.is_some() {
        velocity.z = 0.0;
        velocity = accelerate(wish_dir, wish_speed, velocity, ctx);
        velocity.z -= ctx.cfg.gravity * ctx.dt;
        ground_move();
    } else {
        velocity = air_accelerate(wish_dir, wish_speed, velocity, ctx);
        velocity.z -= ctx.cfg.gravity * ctx.dt;
        (transform, velocity) = fly_move(transform, velocity, ctx, spatial);
    }
    (transform, velocity)
}

#[must_use]
fn accelerate(wish_dir: Dir3, wish_speed: f32, velocity: Vec3, ctx: &Ctx) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    // right here is where air strafing happens: `current_speed` is close to 0 when we want to move perpendicular to
    // our current velocity, making `add_speed` large.
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(ctx.cfg.acceleration * ctx.dt * wish_speed, add_speed);
    velocity + accel_speed * wish_dir
}

#[must_use]
fn air_accelerate(wish_dir: Dir3, wish_speed: f32, velocity: Vec3, ctx: &Ctx) -> Vec3 {
    let wish_speed = f32::min(wish_speed, ctx.cfg.air_speed);
    accelerate(wish_dir, wish_speed, velocity, ctx)
}

#[must_use]
fn ground_move() {
    todo!();
}

#[must_use]
fn friction(
    transform: Transform,
    velocity: Vec3,
    ctx: &Ctx,
    spatial: &ReadOnlySpatialQuery,
) -> Vec3 {
    let speed = velocity.length();
    if speed < 0.025 {
        return Vec3::ZERO;
    }

    let mut friction_hz = ctx.cfg.friction_hz;
    // if the leading edge is over a dropoff, increase friction
    if ctx.state.grounded.is_some() {
        // speed cannot be zero, we early return in that case already
        let vel_dir = velocity / speed;
        let mut start = transform.translation + vel_dir * 0.4;
        // min is negative, so this goes *down*
        start.y = transform.translation.y + ctx.aabb.min.y;
        let cast_dir = Dir3::NEG_Y;

        let trace = spatial.cast_shape(
            &ctx.collider,
            start,
            transform.rotation,
            cast_dir,
            &ShapeCastConfig::from_max_distance(0.85),
            &ctx.cfg.filter,
        );
        if trace.is_none() {
            friction_hz *= 2.0;
        }
    }
    let drop = if ctx.state.grounded.is_some() {
        let stop_speed = f32::max(speed, ctx.cfg.stop_speed);
        stop_speed * friction_hz * ctx.dt
    } else {
        0.0
    };
    let new_speed = f32::max(speed - drop, 0.0);
    velocity / speed * new_speed
}

#[must_use]
fn clip_velocity(velocity: Vec3, normal: Vec3, overbounce: f32) -> Vec3 {
    let backoff = velocity.dot(normal) * overbounce;
    let change = normal * backoff;
    let mut new_velocity = velocity - change;
    const STOP_EPSILON: f32 = 0.0025;
    for i in 0..3 {
        if new_velocity[i].abs() < STOP_EPSILON {
            new_velocity[i] = 0.0;
        }
    }
    new_velocity
}

#[must_use]
fn fly_move(
    mut transform: Transform,
    mut velocity: Vec3,
    ctx: &Ctx,
    spatial: &ReadOnlySpatialQuery,
) -> (Transform, Vec3) {
    let mut time_left = ctx.dt;
    const NUM_BUMPS: usize = 4;
    const MAX_CLIP_PLANES: usize = 4;
    let original_velocity = velocity;
    let mut num_planes: usize;
    let mut planes = [Vec3::ZERO; MAX_CLIP_PLANES];

    for _ in 0..NUM_BUMPS {
        let Ok((cast_dir, cast_len)) = Dir3::new_and_length(time_left * velocity) else {
            break;
        };
        let trace = spatial.cast_shape(
            &ctx.collider,
            transform.translation,
            transform.rotation,
            cast_dir,
            &ShapeCastConfig::from_max_distance(cast_len),
            &ctx.cfg.filter,
        );
        num_planes = 0;
        let Some(trace) = trace else {
            transform.translation += cast_dir * cast_len;
            // moved the entire distance
            break;
        };
        transform.translation += cast_dir * trace.distance;

        if trace.distance > 0.0 {
            // actually covered some distance
            num_planes = 0;
        }

        let fraction = trace.distance / cast_len;
        // account for the time the cast spent while tracing
        time_left -= time_left * fraction;

        if num_planes >= MAX_CLIP_PLANES {
            velocity = Vec3::ZERO;
            break;
        }
        planes[num_planes] = trace.normal1;
        num_planes += 1;
        let mut i = 0;
        while i < num_planes {
            velocity = clip_velocity(original_velocity, planes[i], 0.025);
            let mut j = 0;
            while j < num_planes {
                if i != j && velocity.dot(planes[j]) < 0.0 {
                    break;
                }
                j += 1;
            }
            if j == num_planes {
                break;
            }
            i += 1;
        }
        if i != num_planes {
            // go along this plane
        } else {
            // go along the crease
            if num_planes != 2 {
                velocity = Vec3::ZERO;
                break;
            }
            let dir = planes[0].cross(planes[1]);
            let d = dir.dot(velocity);
            velocity = dir * d;
        }
        // if original velocity is against the original velocity, stop dead
        // to avoid tiny occilations in sloping corners
        if velocity.dot(original_velocity) <= 0.0 {
            velocity = Vec3::ZERO;
            break;
        }
    }
    (transform, velocity)
}
