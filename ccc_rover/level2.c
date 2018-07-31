#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define PI 3.14159265359

static inline float deg_to_rad(float degrees);
static inline float rad_to_deg(float radians);

float distance;
float wheel_base;
float steering_angle;

int main(int argc, char *argv[])
{
	if (argc < 4) {
		printf("error: missing arguments");
		return -1;
	}

	distance = atof(argv[1]);
	wheel_base = atof(argv[2]);
	steering_angle = atof(argv[3]);

	// Radius of the circle we move in.
	float radius = wheel_base / sin(deg_to_rad(steering_angle));
	// float radius = 1.0;

	// Angle from starting position (0°)s.
	float angle = rad_to_deg(distance / radius);

	// Delta X/Y from starting position.
	float x = ((radius * cos(deg_to_rad(angle))) * -1.0) + radius;
	float y = radius * sin(deg_to_rad(angle));

	// printf("radius: %.2f, angle: %.2f, relative x/y: %.2f/%.2f\n",
	// 	radius, angle, x, y);

	printf("%.2f %.2f %.2f\n", x, y, angle);

	/*
	for (int a = 0; a < 360; a++) {
		const float r = 1.0;

		float x = ((r * cos(deg_to_rad(a))) * -1.0) + r;
		float y = sin(deg_to_rad(a));

		printf("%3d° : %.2f/%.2f\n", a, x, y);
	}
	*/
}

static inline float deg_to_rad(float degrees)
{
	return degrees * (PI / 180.0);
}

static inline float rad_to_deg(float radians)
{
	return radians * (180.0 / PI);
}