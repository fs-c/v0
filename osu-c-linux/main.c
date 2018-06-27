#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#define NUM_KEYS 4
#define COL_WIDTH (512 / NUM_KEYS)

struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct hitpoint hitpoint;

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

	char *map, c;

	while ((c = getopt(argc, argv, "m:")) != -1) {
		switch (c) {
		case 'm':
			map = optarg;
			break;
		}
	}

	hitpoint *points;

	int read = parse_hitpoints(map, &points);

	printf("%d hitpoints read\n", read);

	for (int i = 0; i < read; i++) {
		hitpoint p = points[i];
		printf("stime: %d, etime: %d, type: %d, col: %d\n", p.stime, p.etime, p.type, p.column);
	}
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