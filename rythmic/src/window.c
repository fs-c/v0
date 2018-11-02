#include "rythmic.h"

#include <tlhelp32.h>

struct handle_data {
	HWND window_handle;
	unsigned long process_id;
};

static WINBOOL __stdcall enum_windows_callback(HWND handle, LPARAM param);

int get_window_handle(const pid_t process_id, void **out_window_handle)
{
	struct handle_data data = { 0, process_id };
	if ((EnumWindows((WNDENUMPROC)enum_windows_callback, (LPARAM)&data)) == 0) {
		debug_winerror("failed enumerating windows");

		return 0;
	}

	debug("got window handle for process with ID %d", (int)process_id);

	HWND handle = data.window_handle;
	*out_window_handle = malloc(sizeof(HWND));

	memcpy(out_window_handle, &handle, sizeof(HWND));

	return 1;
}

static WINBOOL __stdcall enum_windows_callback(HWND handle, LPARAM param)
{
	struct handle_data *data = (struct handle_data *)param;

	DWORD process_id = 0;
	GetWindowThreadProcessId(handle, &process_id);

	if (process_id != data->process_id)
		return 1;

	data->window_handle = handle;
	return 0;
}
