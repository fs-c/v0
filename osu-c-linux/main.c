#include <stdio.h>
#include <signal.h>
#include <xdotool/xdo.h>

#include "osu.h"

static inline void send_keypress(char code, int down);

xdo_t *window;

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

	window = xdo_new(":0.0");

	while ((c = getopt(argc, argv, "m:p:")) != -1) {
		switch (c) {
		case 'm': map = optarg;
			break;
		case 'p': pid = strtol(optarg, NULL, 10);
			break;
		}
	}

	if (kill(pid, 0) < 0) {
		printf("pid %d does not exist\n", pid);
		return EXIT_FAILURE;
	}

	int hpread;
	hitpoint *points;
	if (!(hpread = parse_hitpoints(map, &points))) {
		printf("failed parsing hitpoints from %s\n", map);
		return EXIT_FAILURE;
	}	

	int acread;
	action *actions;
	if (!(acread = hitpoints_to_actions(hpread, &points, &actions))) {
		printf("failed converting hitpoints to actions\n");
		return EXIT_FAILURE;
	}

	free(points);	
	sort_actions(acread, &actions);

	int cur = 0;

	while (1) {
		action ca;
		int32_t time;
		get_maptime(pid, &time);

		while ((ca = *(actions + cur)).time <= time) {
			cur++;

			puts("performing action");

			send_keypress(ca.code, ca.type);
		}
	}
}

/**
 * Send a keyup or keydown event to X11.
 */
static inline void send_keypress(char code, int down)
{
	if (down) {
		xdo_send_keysequence_window_down(window, CURRENTWINDOW, &code, 0);
	} else {
		xdo_send_keysequence_window_up(window, CURRENTWINDOW, &code, 0);
	}
}
