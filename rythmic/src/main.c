#include "rythmic.h"

pid_t game_proc_id = 0;
RECT game_window_rect = { 0 };
void *game_time_address = NULL;
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

	if (!(game_time_address = get_game_time_address())) {
		fatal("couldn't get time address");
	}

	void *a = 0x13260c4;

	debug("%d, %d", a == game_time_address, (int)a - (int)game_time_address);

	debug("%#x -- %#x", game_time_address, a);

	int32_t val1 = 0;
	int32_t val2 = 0;

	while (1) {
		read_game_memory(a, &val1, 4, NULL);

		get_game_time(&val2);

		debug("%d / %d", val1, val2);

		Sleep(200);
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
