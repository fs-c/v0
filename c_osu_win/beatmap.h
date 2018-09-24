#ifndef BEATMAP_H
#define BEATMAP_H

#define TAPTIME_MS 2
#define MAX_LENGTH 1024

#define NUM_COLS 4
#define COL_WIDTH 512

struct hitpoint {
	int column;
	int end_time;
	int start_time;
};

typedef struct hitpoint hitpoint;

struct action {
	int time;
	int down;
	char key;
};

typedef struct action action;

/**
 * Parse a beatmap file (*.osu) into an array of hitpoint structs pointed to by 
 * **points.
 * Returns the number of points parsed and stored.
 */
int parse_beatmap(char *file, hitpoint **points);

/**
 * Parses a raw beatmap line into a hitpoint struct pointed to by *point.
 * Returns the number of tokens read.
 */
int parse_beatmap_line(char *line, hitpoint *point);

/**
 * Parses a total of count hitmapts from **points into **actions.
 * Returns the number of actions parsed and stored, which should be count * 2.
 */
int parse_hitpoints(int count, hitpoint **points, action **actions);

/**
 * Populates *start and *end with data from hitpoint *point.
 */
void hitpoint_to_action(hitpoint *point, action *start, action *end);

/**
 * Sort the array of actions given through **actions by time.
 * Returns nonzero on failure.
 */
int sort_actions(int count, action **actions);

#endif /* BEATMAP_H */