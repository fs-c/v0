#include "osu.h"

#include <stdio.h>

int parse_beatmap(char *file, hitpoint **points)
{
	FILE *stream;
	char line[MAX_LENGTH];

	if ((stream = fopen(file, "r")) == NULL) {
		return 0;
	}

	int parsed = 0;		// Number of hitpoints parsed.
	int in_section = 0;	// Currently in the hitobjects section?
	while (fgets(line, sizeof(line), stream)) {
		if (strcmp(line, "[HitObjects]") == 0) {
			in_section = 1;

			*points = malloc(sizeof(hitpoint));

			continue;
		} else if (!in_section) continue;

		hitpoint point;

		parse_beatmap_line(line, &point);

		*points = realloc(*points, ++parsed * sizeof(hitpoint));
		(*points)[parsed - 1] = point;
	}

	return parsed;
}

int parse_beatmap_line(char *line, hitpoint *point)
{
	char *token, *extln, *ln = strdup(line), segval = 0, i = 0;

	// Line is expexted to follow the following format:
	// x, y, time, type, hitSound, extras (= a:b:c:d:)
	while (token = strtok(ln, ",")) {
		segval = strtol(token, NULL, 10);

		switch (i++) {
		// X
		case 0: point->column = segval / (COL_WIDTH / NUM_COLS);
			break;
		// Start time
		case 2: point->start_time = segval;
			break;
		// Extra string, first element is either 0 or end time
		case 5:
			// If end is 0 this is not a hold note, just tap.
			point->end_time = strtol(strtok(token, ":"), NULL, 10)
				|| point->start_time + TAPTIME_MS;

			break;
		}
	}

	free(ln);
	free(token);

	return i;
}

int parse_hitpoints(int count, hitpoint **points, action **actions)
{
	// Allocate enough memory for all actions.
	*actions = malloc((2 * count) * sizeof(action));

	hitpoint *cur_point;
	int num_actions = 0, i = 0;

	while (i < count) {
		cur_point = (*points) + i++;

		// Don't care about the order here.
		action *end = *actions + num_actions++;
		action *start = *actions + num_actions++;

		hitpoint_to_action(cur_point, start, end);
	}

	return num_actions;
}

// TODO: This really shouldn't be here but it was causing weird errors. Fuck
// 	 you, compiler, outplayed ya!
const char COL_KEYS[] = { 'd', 'f', 'j', 'k' };

void hitpoint_to_action(hitpoint *point, action *start, action *end)
{
	end->time = point->end_time;
	start->time = point->start_time;

	end->down = 0;		// Keyup.
	start->down = 1;	// Keydown.

	const char key = COL_KEYS[point->column];

	end->key = key;
	start->key = key;
}

int sort_actions(int count, action **actions)
{
	int min, i, j;
	action *act = *actions, tmp;

	// For every element.
	for (i = 0; i < count; i++) {
		min = i;

		// For the subarray starting at a[j].
		for (j = i + 1; j < count; j++)
			// Find the smallest element.
			if ((act + j)->time < (act + min)->time) min = j;

		// Swap current element with the smallest element of subarray.
		tmp = act[i];
		act[i] = act[min];
		act[min] = tmp;
	}

	return (i + 1) - count;
}