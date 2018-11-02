#include "rythmic.h"

static void fatal(const char *message);

int main()
{
	pid_t game_proc_id = 0;
	if (!(game_proc_id = get_process_id(OSU_PROC_NAME))) {
		fatal("couldn't get process id");
	}

	HANDLE game_proc_handle = NULL;
	if (!(game_proc_handle = get_process_handle(game_proc_id))) {
		fatal("couldn't open handle to process");
	}

	HWND game_window_handle = NULL;
	if (!(get_window_handle(game_proc_id, (void **)&game_window_handle))) {
		fatal("couldn't open handle to window");
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
