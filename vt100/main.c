#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define DRAW_CHAR '#'

/* Operations performed on the whole terminal 'screen'.
 */
void screen_clear(void);

/* Responsible for the top status bar.
 * Seperated from draw_ since it is independent of the "main screen".
 */
void status_draw();
void status_update();

/* Handling cursor movement.
 */
void curser_move_back();
void cursor_move(int x, int y);
void cursor_move_by(int delta_x, int delta_y);
void cursor_pos_save();
void cursor_pos_restore();

/* Operations on specific lines.
 */
void line_erase(int line);

/* Drawing functions.
 *
 * All lengths are inclusive.
 * All drawings start from the given position and set the cursor to that
 * position on exit.
 * Shapes have their 0/0 on the top left.
 */
void draw_square(int x, int y, int width);
void draw_line_vertical(int x, int y, int length);
void draw_line_horizontal(int x, int y, int length);

/* Utility functions.
 */
static inline int constrain_above(int orig, int min);

// The current X and Y coordinates of the cursor.
int cur_x = 0;
int cur_y = 0;

// Coordinate buffers for convenient save and restore.
int buf_x = 0;
int buf_y = 0;

// The previous coordinates, for convenient cursor position restoring.
int last_x = 0;
int last_y = 0;

// Most recent warning reported by any function.
char *last_error = NULL;

int main()
{
	// Disable stdout buffering.
	setbuf(stdout, NULL);

	cursor_pos_save();

	int i = 0;
	while (++i) {
		screen_clear();

		draw_square(i, i, 10);

		status_draw();
		last_error = NULL; // Only show errors per frame.

		nanosleep((struct timespec[]){{1, 0}}, NULL);
	}

	return 0;
}

void screen_clear()
{
	printf("\e[2J");

	// ESC[2J should do this by itself but... yeah, it doesn't.
	cursor_move(0, 0);
}

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

void status_draw()
{
	// Since we need to move around to draw this, save beforehand.
	int sx = cur_x, sy = cur_y;

	cursor_move(0, 0);
	printf("%3d / %-3d | %s", sx, sy, last_error);
	cursor_move_back();
}

void status_update()
{
	line_erase(0);
	status_draw();
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

void draw_line_horizontal(int x, int y, int length)
{
	cursor_move(x, y);

	for (int i = 0; i < length + 1; i++) {
		putchar(DRAW_CHAR);
	}

	cursor_move(x, y); // Obsolete, but comply with standard.
}

void draw_line_vertical(int x, int y, int length)
{
	cursor_move(x, y);

	for (int i = 0; i < length + 1; i++) {
		printf("%c", DRAW_CHAR);
		cursor_move_by(0, 1);
	}

	cursor_move(x, y); // See above.
}

void draw_square(int x, int y, int width)
{
	// Monospace width != height, this is just an approximation.
	int height = width / 2;

	draw_line_horizontal(x, y, width);
	draw_line_vertical(x, y, height - 1);

	draw_line_horizontal(x, y + height, width);
	draw_line_vertical(x + width, y, height - 1);

	cursor_move(x, y);
}

/**
 * Return `orig` or `min` if it is not in the range of [min, Infinity[.
 */
static inline int constrain_above(int orig, int min)
{
	return orig < min ? min : orig;
}