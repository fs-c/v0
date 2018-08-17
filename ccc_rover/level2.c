#include "common.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PI 3.14159265359

void solve(double wheel_base, double distance, double steering_angle);
void parse_input(const char *string, const int str_len, double **arr);

const int debug = 1;

const int in_len = 6;
char *const input[] = {
	"1.00 1.00 30.00",
	"2.13 4.30 23.00",
	"1.75 3.14 -23.00",
	"2.70 45.00 -34.00",
	"4.20 -5.30 20.00",
	"9.53 8.12 0.00"
};
 
int main(int argc, char *argv[])
{
	double *args = malloc(sizeof(double) * 3);
	for (int i = 0; i < in_len; i++) {
		parse_input(input[i], sizeof(input[i]), &args);

		solve(args[0], args[1], args[2]);
	}
}

void parse_input(const char *string, const int str_len, double **arr)
{
	int i = 0;
	char *token = NULL;
	char *str = malloc(str_len);

	strcpy(str, string);

	while ((token = strsep(&str, " ")))
		(*arr)[i++] = atof(token);
}

void solve(double wheel_base, double distance, double steering_angle)
{
	// Is the turning circle 'flipped' or not (ergo steering left or right)?
	// Open to: 0 - Right, 1 - Left
	int flipped = 0;

	// Radius of the circle we move in.
	double radius = get_radius(wheel_base, steering_angle);

	if (radius < 0) {
		// Never work with a negative radius.	
		radius *= -1.0;

		flipped = 1;
	}

	// Delta angle from starting position.
	double angle = rad_to_deg(distance / radius);

	// X/Y from circle origin.
	double x, y;
	get_circle_point(radius, angle, &x, &y);

	// Make relative to starting position.
	x *= flipped ? 1.0 : -1.0;
	x += flipped ? -radius : radius;

	if (flipped) {
		angle = 360 - angle;
	}

	if (debug) {
		printf("[ base %.2f, dist %.2f, sang %.2f ] : ",
			wheel_base, distance, steering_angle);
		printf("rds=%.2f ang=%.2f delta=%.2f/%.2f\n",
			radius, angle, x, y);
	}

	printf("%.2f %.2f %.2f\n", x, y, angle);
}
