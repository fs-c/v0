#include <math.h>
#include <stdio.h>

#define PI 3.14159265359

int main()
{
	const float wheel_base = 1.00;
	const float steering_angle = 30.00;

	float turn_radius = wheel_base / sin(steering_angle * (PI / 180.0));

	printf("float: %f\n", turn_radius);
}
