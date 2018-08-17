#include <math.h>

#define PI 3.14159265359

double deg_to_rad(double degrees) 
{
	return degrees * (PI / 180.0);
}

double rad_to_deg(double radians)
{
	return radians * (180.0 / PI);
}

double get_radius(double wheel_base, double steering_angle)
{
	return wheel_base / sin(deg_to_rad(steering_angle));
}

void get_circle_point(double radius, double angle, double *x, double *y)
{
	*x = radius * cos(deg_to_rad(angle));
	*y = radius * sin(deg_to_rad(angle));
}