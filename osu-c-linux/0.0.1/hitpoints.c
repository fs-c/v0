#include <stdio.h> // fopen(), getline()
#include <stdlib.h> // malloc(), realloc(), free()
#include <string.h> // strstr(), strsep(), strdup()

#include "osu.h"

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
 * Parses a raw hitpoint line from a beatmap file into a hitpoint struct pointed
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