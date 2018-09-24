#include "vt100.h"

#include <math.h> 	// cos(), sin()
#include <stdio.h>	// printf()
#include <unistd.h>
#include <sys/ioctl.h>	// ioctl()	

int buf_x;
int buf_y;

void screen_clear()
{
	printf("\e[2J");

	// ESC[2J should do this by itself but... yeah, it doesn't.
	cursor_move(0, 0);
}

void status_draw()
{
	line_erase(0);

	// Since we need to move around to draw this, save beforehand.
	cursor_pos_save();

	cursor_move(0, 0);
	printf("%3d / %-3d | %s", buf_x, buf_y, last_error);

	cursor_pos_restore();
}

void line_erase(int line)
{
	int y = constrain_above(line, 0);

	if (y != line) {
		last_error = "attempted to erase foreign line";
	}

	cursor_move(0, y);
	printf("\e[2K");
	cursor_move_back();
}

void get_terminal_dimensions(int *lines, int *columns)
{
	struct winsize w;
    	ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

	*lines = w.ws_row;
	*columns = w.ws_col;
}

void get_circle_point(int radius, int angle, int *x, int *y)
{
	float radians = degrees_to_radians(angle);

	*x = round_float(radius * cos(radians));
	*y = round_float(radius * sin(radians));
}
