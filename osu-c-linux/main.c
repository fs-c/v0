#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/uio.h>

#define NUM_KEYS 4
#define COL_WIDTH (512 / NUM_KEYS)

#define TAPTIME_MS 3

// 0x36e59ec (I64, I32) and 0x36e5c1c (I32) are both maptime.
#define MAPTIME_ADDR 0x36e5c1c

struct action {
	int time;
	int type;
};

struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct action action;
typedef struct hitpoint hitpoint;

static inline ssize_t get_maptime(pid_t pid, int32_t *val);

int parse_hitpoint(char *line, hitpoint *point);
int parse_hitpoints(char *path, hitpoint **points);

int opterr;
char *optarg = 0;

int main(int argc, char **argv)
{
	if (argc < 2) {
		printf("usage: <executable> -m <map path> -p <osu! pid>\n");
		return 1;
	}

	pid_t pid;
	char *map, c;

	while ((c = getopt(argc, argv, "m:p:")) != -1) {
		switch (c) {
		case 'm': map = optarg;
			break;
		case 'p': pid = strtol(optarg, NULL, 10);
			break;
		}
	}

	int read;
	hitpoint *points;

	if (!(read = parse_hitpoints(map, &points))) {
		printf("something went wrong while parsing hitpoints from %s\n",
			map);
		return EXIT_FAILURE;
	}

	if (kill(pid, 0) < 0) {
		printf("pid %d does not exist\n", pid);
		return EXIT_FAILURE;
	}
}

/**
 * Gets and stores the runtime of the currently playing song, internally
 * referred to as `maptime` in *val.
 * Returns the number of bytes read.
 */
static inline ssize_t get_maptime(pid_t pid, int32_t *val)
{
	ssize_t nread;
	size_t size = sizeof(int32_t);

	struct iovec local[1];
	struct iovec remote[1];

	local[0].iov_len = size;
	local[0].iov_base = val;

	remote[0].iov_len = size;
	remote[0].iov_base = (void *)MAPTIME_ADDR;

	nread = process_vm_readv(pid, local, 1, remote, 1, 0);
	
	return nread;
}

/**
 * Parses the hitpoints from a given beatmap (*.osu) file and loads them into
 * an empty array of hitpoints pointed at by **points.
 * Returns the number of parsed points.
 */
int parse_hitpoints(char *path, hitpoint **points)
{
	FILE *stream;

	if ((stream = fopen(path, "r")) == 0) {
		return -1;
	}

	int pparsed = 0;	// Number of hitpoints parsed/read.

	size_t nread;		// Local to loop, number of characters read.
	size_t len = 0;
	char *line = 0;
	char insct = 0;		// Currently in the HitObjects section?
	while ((nread = getline(&line, &len, stream)) != -1) {
		if (!insct && (strstr(line, "[HitObjects]") != 0)) {
			insct = 1;

			*points = malloc(sizeof(hitpoint));

			continue;
		} else if (!insct) continue;

		hitpoint point;
		parse_hitpoint(line, &point);

		*points = realloc(*points, ++pparsed * sizeof(hitpoint));
		(*points)[pparsed - 1] = point;
	}

	free(line);
	fclose(stream);

	return pparsed;
}

/**
 * Parse a raw hitpoint line from a beatmap file into a hitpoint struct pointed
 * to by *point.
 * Returns the number of tokens which were read, which doesn't always equal the
 * number of actual values loaded into the struct!
 */
int parse_hitpoint(char *line, hitpoint *point)
{
	char *ln = strdup(line), i = 0, *token, *eln;

	while (token = strsep(&ln, ",")) {
		int tval = strtol(token, NULL, 10);

		switch (i) {
		case 0: point->column = tval / COL_WIDTH;	// X
			break;
		case 2: point->stime = tval;			// Time
			break;
		case 3: point->type = tval;			// Type mask
			break;
		case 5: // Extra string, first token is end time.
			eln = strdup(token);

			int etime = strtol(strsep(&eln, ":"), NULL, 10);
			point->etime = etime ? etime
				: point->stime + TAPTIME_MS;
			break;
		}

		i++;
	}

	free(ln);
	// TODO: Why can't I free(eln) here? (Do I even have to?)

	return i;
}

int hitpoints_to_actions(int count, hitpoint **points, action **actions)
{
	hitpoint *curp;

	*actions = malloc(sizeof(action));

	int i = 0;
	int j = 0;

	while (i++ < count) {
		curp = (*points) + i;
		
		*actions = realloc(*actions, (j += 2) * sizeof(action));

		action *keyup = (*actions) + (j - 2);
		action *keydw = (*actions) + (j - 1);

		keyup->type = 1;
		keyup->time = curp->stime;

		keydw->type = 2;
		keydw->time = curp->etime;
	}

	return j;
}