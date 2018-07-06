#include <time.h> // nanosleep()
#include <stdio.h> // printf()
#include <signal.h> // kill()
#include <unistd.h> // getopt()
#include <stdlib.h> // strotol(), free()
#include <stdbool.h>

#include <X11/Xlib.h> // XOpenDisplay()
#include <X11/extensions/XTest.h> // XTestFakeKeyEvent()

#include "osu.h"

static void print_usage();
static inline void send_keypress(int code, int down);

int opterr;
char *optarg = 0;

Display *display;

int main(int argc, char **argv)
{
	if (argc < 2) {
		print_usage();
		return EXIT_FAILURE;
	}

	pid_t pid = 0;
	char *map = NULL, c;

	while ((c = getopt(argc, argv, "m:p:d:")) != -1) {
		switch (c) {
		case 'm': map = optarg;
			break;
		case 'p': pid = strtol(optarg, NULL, 10);
			break;
		}
	}

	if (!map || !pid) {
		print_usage();
		return EXIT_FAILURE;
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
	hitpoint *points = NULL;
	if (!(hpread = parse_hitpoints(map, &points)) || !points) {
		printf("failed parsing hitpoints from %s\n", map);
		return EXIT_FAILURE;
	}

	printf("read %d hitpoints\n", hpread);

	int acread;
	action *actions = NULL;
	if (!(acread = hitpoints_to_actions(hpread, &points, &actions))
		|| !actions)
	{
		printf("failed converting hitpoints to actions\n");
		return EXIT_FAILURE;
	}

	printf("converted %d hitpoints to %d actions\n", hpread, acread);

	free(points);

	// TODO: Can this even (reasonably) fail?
	if (sort_actions(acread, &actions)) {
		printf("failed to sort the actions array\n");
		return EXIT_FAILURE;
	}

	int curi = 0;
	int32_t time;
	action *cura = NULL;

	while (curi < acread) {
		if (get_maptime(pid, &time) != sizeof(int32_t)) {
			printf("failed reading maptime\n");
			continue;
		}

		// For each action that is (over)due.
		while ((cura = actions + curi)->time < time && cura) {
			curi++;

			send_keypress(cura->code, cura->type);

			printf("%d\n", cura->time - time);
		}

		// 10^6 ns = 1 ms.
		nanosleep((struct timespec[]){{0, 1000000L}}, NULL);
	}
}

static void print_usage()
{
	printf("usage: <executable> -m <map path> -p <osu! process id>\n");
}

/**
 * Simulate a keypress.
 */
static inline void send_keypress(int code, int down)
{
	if (!XTestFakeKeyEvent(display, code, down, CurrentTime)) {
		printf("failed sending keyevent\n");
	}

	XSync(display, false);
}
