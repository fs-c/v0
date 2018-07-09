#include "osu.h"

#include <time.h>
#include <stdio.h>
#include <signal.h> 

#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>

void dbg_print_actions(int count, action** actions);
void dbg_print_hitpoints(int count, hitpoint **points);

static inline void send_keypress(int code, int down);

static inline int char_to_modcode(char c);

Display *display;

int main(int argc, char **argv)
{
	char *map = argv[1];
        int pid = strtol(argv[2], NULL, 10);

	if (!(display = XOpenDisplay(NULL))) {
		printf("failed to open X display\n");
		return EXIT_FAILURE;
	}

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

	int32_t time;
	int cur_i = 0;
	action *cur_a = actions;

	while (cur_i < num_actions) {
		time = get_maptime(pid);

		while ((cur_a = actions + cur_i)->time <= time) {
			cur_i++;
					
			send_keypress(cur_a->key, cur_a->down);		
		}

		nanosleep((struct timespec[]){{0, 1000000L}}, NULL);
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

static inline void send_keypress(int code, int down)
{
	XTestFakeKeyEvent(display, char_to_modcode(code), down, CurrentTime);

	XFlush(display);
}

static inline int char_to_modcode(char c)
{
	return c == 'd' ? 40 : c == 'f' ? 41 : c == 'j' ? 44 : c == 'k' ? 45 : 0;
}