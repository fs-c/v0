#define PI 3.14159265

// Most recent warning reported by any function.
extern char *last_error;

// Current X and Y coordinates of the cursor.
extern int cur_x;
extern int cur_y;

// Coordinate buffers written to by save and restore pos.
extern int buf_x;
extern int buf_y;

// Previous coordinates, for convenient cursor position restoring.
extern int last_x;
extern int last_y;

// Most recent error or warning reported by any function.
extern char *last_error;

/* Moves the cursor to the specified coordinates or 0/0 if they were invalid. In
 * that case, last_error will contain a warning message.
 * Saves the most recent cursor position (before the change).
 */
void cursor_move(int x, int y);

/* Moves the cursor by delta_x and delta_y relative to the current cursor
 * position.
 */
void cursor_move_by(int delta_x, int delta_y);

/* Reverts the last cursor movement. Note that the last movement may have not
 * been through a direct call to cursor_move.
 */
void cursor_move_back(void);

/* Saves the current position to the position buffer.
 */
void cursor_pos_save(void);

/* Moves the cursor to the position stored in the position buffer and resets it.
 */
void cursor_pos_restore(void);

/* Draws a horizontal line from X/Y of a given length (inclusive).
 */
void draw_line_horizontal(int x, int y, int length);

/* See draw_line_horizontal.
 */
void draw_line_vertical(int x, int y, int length);

/* Draws a dot. Duh.
 */
void draw_dot(int x, int y);

/* Draws a square where X/Y is the top left corner with a given width
 * (inclusive).
 */
void draw_square(int x, int y, int width);

/* Draws a circle with a given radius. Drawn inside a square with X/Y being
 * it's top left coordinates.
 */
void draw_circle(int x, int y, int radius);

/* Draws the diagonal of a rectangle with the given width and height from the
 * bottom left to the top right. Relative 0/0 is top left, as always.
 */
void draw_diagonal(int x, int y, int width, int height);

/* Moves the terminal down by it's height, effectively clearing it.
 */
void screen_clear(void);

/* Draws the status bar which contains current X and Y coordinates of the
 * cursor and the most recent warning/error reported by any function.
 */
void status_draw(void);

/*
 * Erase the line at the given index (starts at zero, inclusive) or zero if it
 * was invalid in which case last_error will contain a warning.
 */
void line_erase(int line);

/* Returns the current terminal window's lines and columns through the two
 * output pointers.
 */
void get_terminal_dimensions(int *lines, int *columns);

/* Returns a point on a cirle's circumference given an angle in degrees and a
 * radius.
 */
void get_circle_point(int radius, int angle, int *x, int *y);

/* Return `orig` or `min` if it is not in the range of [min, Infinity[.
 */
static inline int constrain_above(int orig, int min)
{
	return orig < min ? min : orig;
}

/* Convert degrees to radians.
 */
static inline float degrees_to_radians(int degrees)
{
	return degrees * (PI / 180);
}

/* Round a floating point value to an int.
 */
static inline int round_float(float x)
{
	int ix = (int)x;
	return (x - ix) > 0.5 ? ix + 1 : ix;
}