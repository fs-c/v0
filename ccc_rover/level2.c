#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define PI 3.14159265359

static inline double deg_to_rad(double degrees);
static inline double rad_to_deg(double radians);

void solve(double wheel_base, double distance, double steering_angle);

int debug = 1;

int main(int argc, char *argv[])
{
	if (argc < 4) {
		printf("error: missing arguments");
		return -1;
	}

	float wheel_base = atof(argv[1]);
	float distance = atof(argv[2]);
	float steering_angle = atof(argv[3]);

	solve(wheel_base, distance, steering_angle);
}

void solve(double wheel_base, double distance, double steering_angle)
{
	// Is the turning circle 'flipped' or not (ergo steering left or right)?
	// 0: Right, 1: Left
	int flipped = 0;

	// Radius of the circle we move in.
	double radius = wheel_base / sin(deg_to_rad(steering_angle));

	if (radius < 0) {
		radius *= -1.0;

		flipped = 1;
	}

	// Angle from starting position (0°)s.
	double angle = rad_to_deg(distance / radius);

	// Delta X/Y from starting position.
	double x = radius * cos(deg_to_rad(angle));
	double y = radius * sin(deg_to_rad(angle));

	x *= flipped ? 1.0 : -1.0;
	x += flipped ? -radius : radius;

	if (debug) {
		printf("[ base %.2f, dist %.2f, sang %.2f ] : ",
			wheel_base, distance, steering_angle);
		printf("rds=%.4f ang=%.4f delta=%.4f/%.4f\n",
			radius, angle, x, y);
	}

	printf("%.2f %.2f %.2f\n", x, y, angle);
}

static inline double deg_to_rad(double degrees)
{
	return degrees * (PI / 180.0);
}

static inline double rad_to_deg(double radians)
{
	return radians * (180.0 / PI);
}