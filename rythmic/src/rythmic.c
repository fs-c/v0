#include "rythmic.h"

char osu_path[256];

pid_t game_proc_id = 0;
RECT game_window_rect = { 0 };
void *game_time_address = NULL;
HWND game_window_handle = NULL;
HANDLE game_proc_handle = NULL;

static void play();
static int standby();
static void fatal(const char *message);

/* TODO: These should probably be moved somewhere else */
static size_t get_osu_path(char *out_path, const size_t path_size);
static size_t get_env_var(char *name, char *out_var, const size_t var_size);

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

	if (!(get_osu_path(osu_path, sizeof(osu_path)))) {
		fatal("couldn't get osu path");
	}

	while (standby())
		play();

	return 0;
}

/* Block until the user is in a beatmap.
 */
static int standby()
{
	char title[256];
	size_t title_len = 0;

	while ((title_len = get_window_title(&title, sizeof(title))))
		if (strcmp(title, "osu!"))
			break;
		else Sleep(200);
	
	char path[512];
	size_t path_len = 0;

	if (!(path_len = find_beatmap(osu_path, title, path, sizeof(path)))) {
		printf("warning: failed to parse beatmap path\n");

		return 0;
	}

	debug("map: %s", path);

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

static size_t get_osu_path(char *out_path, const size_t path_size)
{
	size_t home_len = 0;
	char home[path_size];

	if (!(home_len = get_env_var(HOME_ENV, home, path_size))) {
		debug("couldn't fetch HOME_ENV (%s)", HOME_ENV);

		return 0;
	}

	strcpy_s(out_path, path_size, home);
	strcpy_s(out_path + home_len, path_size - home_len,
		DEFAULT_OSU_PATH);
	
	debug("osu path is '%s'", out_path);

	return strlen(out_path);
}

static size_t get_env_var(char *name, char *out_var, const size_t var_size)
{
	size_t var_len = 0;
	char *var = getenv(name);

	if (!var || !(var_len = strlen(var))) {
		debug("environment variable %s does not exist", name);

		return 0;
	}

	strcpy_s(out_var, var_size, var);

	return var_len;
}
