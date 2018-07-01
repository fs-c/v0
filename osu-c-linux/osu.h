#ifndef _OSU_H_
#define _OSU_H_

#include <sys/types.h>

#define NUM_KEYS 4
#define COL_WIDTH (512 / NUM_KEYS)

#define TAPTIME_MS 3

// 0x36e59ec (I64, I32) and 0x36e5c1c (I32) are both maptime.
#define MAPTIME_ADDR 0x36e5c1c

struct action {
	int time;
	int type;
	int code;
};

struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct action action;
typedef struct hitpoint hitpoint;

/**
 * Parses the hitpoints from a given beatmap (*.osu) file and loads them into
 * an empty array of hitpoints pointed at by **points.
 * Returns the number of parsed points.
 */
int parse_hitpoints(char *path, hitpoint **points);

/**
 * Parses a raw hitpoint line from a beatmap file into a hitpoint struct pointed
 * to by *point.
 * Returns the number of tokens which were read, which doesn't always equal the
 * number of actual values loaded into the struct!
 */
int parse_hitpoint(char *line, hitpoint *point);

/**
 * Converts an array of hitpoint structs (pointed at by **points) into an array
 * of action structs (pointed to by **actions, to be empty).
 * Every hitpoint is converted into two actions for keyup and keydown
 * respectively. Note that actions are not sorted.
 * Returns the number of actions written.
 */
int hitpoints_to_actions(int count, hitpoint **points, action **actions);

/**
 * Populates *ac1 and *ac2 with data from hitpoint *point.
 */
void hitpoint_to_action(hitpoint *point, action *ac1, action *ac2);

/**
 * Selection sort (by time) on the array of actions pointed at by **actions.
 * Returns nonzero value on failure.
 */
int sort_actions(int size, action **actions);

/**
 * Gets and stores the runtime of the currently playing song, internally
 * referred to as `maptime` in *val.
 * Returns the number of bytes read.
 */
ssize_t get_maptime(pid_t pid, int32_t *val);

#endif