#include "vt100.h"

#include <time.h>	// nanosleep()
#include <stdio.h>	// printf(), putcahr()

char *last_error = NULL;

int main()
{
	// Disable stdout buffering.
	setbuf(stdout, NULL);

	int i = 0;
	while (++i) {
		screen_clear();

		draw_square(i, i, 10);

		status_draw();
		last_error = NULL; // Only show errors per frame.

		nanosleep((struct timespec[]){{1, 0}}, NULL);
	}

	return 0;
}