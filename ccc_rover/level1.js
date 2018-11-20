const wheel_base = 1.00;
const steering_angle = 30.00;

const turn_radius = wheel_base / Math.sin(steering_angle * (Math.PI / 180));

console.log(turn_radius);
