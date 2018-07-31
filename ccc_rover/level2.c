#include <math.h>
#include <stdio.h>

#define PI 3.14159265359

static inline float deg_to_rad(float degrees);
static inline float rad_to_deg(float radians);

const float distance = 1.00;
const float wheel_base = 1.00;
const float steering_angle = 30.00;

int main()
{
	// Radius of the circle we move in.
	float radius = wheel_base / sin(deg_to_rad(steering_angle));
	// float radius = 1.0;

	// Angle from starting position (0°)s.
	float angle = rad_to_deg(distance / radius);

	// Delta X/Y from starting position.
	float x = ((radius * -1.0) + radius * cos(deg_to_rad(angle))) * -1.0;
	float y = radius * sin(deg_to_rad(angle));

	// printf("radius: %.2f, angle: %.2f, relative x/y: %.2f/%.2f\n",
	// 	radius, angle, x, y);

	printf("%.2f %.2f %.2f\n", x, y, angle);
}

static inline float deg_to_rad(float degrees)
{
	return degrees * (PI / 180.0);
}

static inline float rad_to_deg(float radians)
{
	return radians * (180.0 / PI);
}