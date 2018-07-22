#include "vt100.h"

#include <stdio.h>

#define DRAW_CHAR '#'
// Approximation of monospace height to width ratio.
#define CHAR_RATIO 0.5

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
	int height = width * CHAR_RATIO;

	draw_line_horizontal(x, y, width);
	draw_line_vertical(x, y, height - 1);

	draw_line_horizontal(x, y + height, width);
	draw_line_vertical(x + width, y, height - 1);

	cursor_move(x, y);
}
