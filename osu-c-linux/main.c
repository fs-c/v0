#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/uio.h>

#define NUM_KEYS 4
#define COL_WIDTH (512 / NUM_KEYS)

#define MAPTIME_ADDR 0x36e59ec

struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct hitpoint hitpoint;

static int32_t get_maptime(pid_t pid);

int parse_hitpoint(char *line, hitpoint *point);
int parse_hitpoints(char *path, hitpoint **points);

int opterr;
char *optarg = 0;

int main(int argc, char **argv)
{
	if (argc < 2) {
		printf("usage: <executable> -m <path to beatmap>\n");
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

	while (1) {
		
	}
}

static inline int32_t get_maptime(pid_t pid)
{
	int32_t buf[1];
	ssize_t nread;
	struct iovec local[1];
	struct iovec remote[1];

	local[0].iov_base = buf;
	local[0].iov_len = sizeof(short);

	remote[0].iov_base = (void *)MAPTIME_ADDR;
	remote[0].iov_len = sizeof(short);

	nread = process_vm_readv(pid, local, 1, remote, 1, 0);
	
	return *buf;
}

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

			point->etime = strtol(strsep(&eln, ":"), NULL, 10);
			break;
		}

		i++;
	}

	free(ln);
	// TODO: Why can't I free(eln) here? (Do I even have to?)

	return i;
}

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