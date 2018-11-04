#include "rythmic.h"

pid_t game_proc_id = 0;
RECT game_window_rect = {0};
HWND game_window_handle = NULL;
HANDLE game_proc_handle = NULL;

static void fatal(const char *message);

int main()
{
	setbuf(stdout, NULL);

	if (!(game_proc_id = get_process_id(OSU_PROC_NAME))) {
		fatal("couldn't get process id");
	}

	if (!(game_proc_handle = get_process_handle(game_proc_id))) {
		fatal("couldn't open handle to process");
	}

	if (!(get_window_handle(game_proc_id, (void **)&game_window_handle))) {
		fatal("couldn't open handle to window");
	}

	if (!(get_window_coordinates(game_window_handle, &game_window_rect))) {
		fatal("couldn't get window coordinates");
	}

	return 0;
}

static void fatal(const char *message)
{
	if (message) {
		printf("fatal: %s\n", message);
	}

	exit(1);
}
