#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define PI 3.14159265359

static inline double deg_to_rad(double degrees);
static inline double rad_to_deg(double radians);

double distance;
double wheel_base;
double steering_angle;

int flipped = 0;

int main(int argc, char *argv[])
{
	if (argc < 4) {
		printf("error: missing arguments");
		return -1;
	}

	wheel_base = atof(argv[1]);
	distance = atof(argv[2]);
	steering_angle = atof(argv[3]);

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

	printf("[ base %.2f, dist %.2f, sang %.2f ] : ",
		wheel_base, distance, steering_angle);
	printf("rds=%.4f ang=%.4f delta=%.4f/%.4f\n",
		radius, angle, x, y);

	printf("%.2f %.2f %.2f\n", x, y, angle);

	/*
	for (int a = 0; a < 360; a++) {
		const double r = 1.0;

		double x = ((r * cos(deg_to_rad(a))) * -1.0) + r;
		double y = sin(deg_to_rad(a));

		printf("%3d° : %.2f/%.2f\n", a, x, y);
	}
	*/
}

static inline double deg_to_rad(double degrees)
{
	return degrees * (PI / 180.0);
}

static inline double rad_to_deg(double radians)
{
	return radians * (180.0 / PI);
}