#include "osu.h"

#include <stdio.h>

size_t time_address;
HANDLE game_proc = NULL;

void dbg_print_actions(int count, action** actions);
void dbg_print_hitpoints(int count, hitpoint **points);

int main(int argc, char **argv)
{
	char *map = argv[1];

	hitpoint *points;
	int num_points = 0;

	if ((num_points = parse_beatmap(map, &points)) == 0 || !points) {
		printf("failed to parse beatmap (%s)\n", map);
		return EXIT_FAILURE;
	}

	printf("parsed %d hitpoints\n", num_points);

	action *actions;
	int num_actions = 0;

	if ((num_actions = parse_hitpoints(num_points, &points, &actions)) == 0
		|| !actions) {
		printf("failed to parse hitpoints\n");
		return EXIT_FAILURE;
	}

	printf("parsed %d actions\n", num_actions);

	free(points);

	if (sort_actions(num_actions, &actions) != 0) {
		printf("failed sorting actions\n");
		return EXIT_FAILURE;
	}

	DWORD game_proc_id;
	if (!(game_proc_id = get_process_id("osu!.exe"))) {
		printf("couldn't find game process");
		return EXIT_FAILURE;
	}

	if (!(game_proc = OpenProcess(PROCESS_VM_READ, 0, game_proc_id))) {
		printf("failed to get handle to game process\n");
		return EXIT_FAILURE;
	}

	printf("got handle to osu! process\n");

	if (!(time_address = find_pattern(game_proc, TIME_SIGNATURE))) {
		printf("failed to find address of the gametime\n");
		return EXIT_FAILURE;
	}

	printf("%d", time_address);

	while (1) {
		INT32 time = get_gametime();
		printf("%d\n", time);
	}

	return 0;
}

void dbg_print_actions(int count, action **actions)
{
	for (int i = 0; i < count; i++) {
		action *a = *actions + i;
		printf("%d / %d (%c) / %d\n", a->time, a->key, a->key, a->down);
	}
}

void dbg_print_hitpoints(int count, hitpoint **points)
{
	for (int i = 0; i < count; i++) {
		hitpoint *p = *points + i;
		printf("%d - %d / %d\n", p->start_time, p->end_time, p->column);
	}
}