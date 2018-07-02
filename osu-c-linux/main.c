#include <time.h> // nanosleep()
#include <stdio.h> // printf()
#include <signal.h> // kill()
#include <unistd.h> // getopt()
#include <stdlib.h> // strotol(), free()
#include <stdbool.h>

#include <X11/Xlib.h> // XOpenDisplay()
#include <X11/extensions/XTest.h> // XTestFakeKeyEvent()

#include "osu.h"

static inline void send_keypress(int code, int down);

int opterr;
char *optarg = 0;

Display *display;

int main(int argc, char **argv)
{
	if (argc < 2) {
		printf("usage: <executable> -m <map path> -p <osu! pid>\n");
		return 1;
	}

	pid_t pid;
	char *map, c;

	while ((c = getopt(argc, argv, "m:p:d:")) != -1) {
		switch (c) {
		case 'm': map = optarg;
			break;
		case 'p': pid = strtol(optarg, NULL, 10);
			break;
		}
	}

	if ((display = XOpenDisplay(NULL)) == NULL) {
		printf("failed to open X display\n");
		return EXIT_FAILURE;
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

	// TODO: Can this even (reasonably) fail?
	if (sort_actions(acread, &actions)) {
		printf("failed to sort the actions array\n");
		return EXIT_FAILURE;
	}

	int curi = 0;
	action *cura;
	int32_t time;
	register ssize_t nread;

	while (curi < acread) {
		if ((nread = get_maptime(pid, &time)) != sizeof(int32_t)) {
			printf("failed reading maptime\n");
			continue;
		}

		printf("%d\n", time);

		// For each action that is (over)due.
		while ((cura = actions + curi)->time >= time) {
			curi++;

			send_keypress(cura->code, cura->type);
		}

		nanosleep((struct timespec[]){{0, 1000000L}}, NULL);
	}
}

/**
 * Simulate a keypress.
 */
static inline void send_keypress(int code, int down)
{
	bool dwn = down ? true : false; // TODO: Is this necessary?

	printf("%d / %d\n", code, down);

	XTestFakeKeyEvent(display, code, dwn, CurrentTime);
	XFlush(display);
}
