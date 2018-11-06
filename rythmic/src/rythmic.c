#include "rythmic.h"

pid_t game_proc_id = 0;
RECT game_window_rect = { 0 };
void *game_time_address = NULL;
HWND game_window_handle = NULL;
HANDLE game_proc_handle = NULL;

static void play();
static int standby();
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

	while (standby())
		play();

	return 0;
}

/* Block until the user is in a beatmap.
 */
static int standby()
{
	/* Wide characters are used here because it's not unlikely that a
	   beatmap (and therefore title) might contain rather exotic
	   characters. */

	wchar_t title[256];
	size_t title_len = 0;

	while ((title_len = get_window_title(&title, sizeof(title))))
		if (wcscmp(title, L"osu!"))
			break;
		else Sleep(200);

	return 0;
}

/* Load and play the current beatmap, return once the user exits it.
 */
static void play()
{
	/* TODO */
}

/* Log an error message and exit with status 1.
 */
static void fatal(const char *message)
{
	if (message) {
		printf("fatal: %s\n", message);
	}

	exit(1);
}
