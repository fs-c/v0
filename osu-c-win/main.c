#include "osu.h"

#include <stdio.h>

HANDLE game_proc = NULL;

int main(int argc, char **argv)
{
	char *map = argv[1];

	hitpoint *points;
	int num_points = 0;

	if ((num_points = parse_beatmap(map, &points)) == 0 || !points) {
		printf("failed to parse beatmap (%s)\n", map);
		return EXIT_FAILURE;
	}

	action *actions;
	int num_actions = 0;

	if ((num_actions = parse_hitpoints(num_points, &points, &actions)) == 0
		|| !actions) {
		printf("failed to parse hitpoints\n");
		return EXIT_FAILURE;
	}

	if (sort_actions(num_actions, &actions) != 0) {
		printf("failed sorting actions\n");
		return EXIT_FAILURE;
	}

	free(points);

	for (int i = 0; i < 50; i++) {
		action *a = actions + i;
		printf("%d / %d (%c) / %d", a->time, a->key, a->key, a->down);
	}

	return 0;
}