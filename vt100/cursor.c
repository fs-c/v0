#include "vt100.h"

#include <stdio.h>

// Current X and Y coordinates of the cursor.
int cur_x;
int cur_y;

// Coordinate buffers for convenient save and restore.
int buf_x;
int buf_y;

// The previous coordinates, for convenient cursor position restoring.
int last_x;
int last_y;

char *last_error;

void cursor_move(int x, int y)
{
	last_x = cur_x;
	last_y = cur_y;

	cur_x = constrain_above(x, 0);
	cur_y = constrain_above(y, 0);

	if (cur_x != x || cur_y != y) {
		last_error = "attempted to move beyond cursor constraints";
	}

	// Note the argument order since this expects "ESC line;column H"
	// where line is equivalent to Y and column to X.
	printf("\e[%d;%dH", cur_y, cur_x);
}

void cursor_move_by(int delta_x, int delta_y)
{
	cursor_move(cur_x + delta_x, cur_y + delta_y);
}

void cursor_move_back()
{
	cursor_move(last_x, last_y);
}

void cursor_pos_save()
{
	buf_x = cur_x;
	buf_y = cur_y;
}

void cursor_pos_restore()
{
	cursor_move(buf_x, buf_y);

	buf_x = buf_y = 0;	
}
