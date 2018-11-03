#ifndef RYTHMIC_H
#define RYTHMIC_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <windows.h>
#include <inttypes.h>
#include <sys/types.h>

#define OSU_PROC_NAME "osu!.exe"

#define HOME_ENV "USERPROFILE"
#define SIGNATURE "\xDB\x5D\xE8\x8B\x45\xE8\xA3"

#define SEPERATOR '\\'
#define DEFAULT_OSU_PATH "\\AppData\\Local\\osu!\\Songs\\"

#ifdef DEBUG

/* printf with added file and function name */
#define debug(...)                                      \
	printf("[debug] [%s:%s] ", __FILE__, __func__); \
	printf(__VA_ARGS__);                            \
	putchar('\n');					\

/* Prints formatted GetLastError alongside a message */
#define debug_winerror(message) 					\
	wchar_t buf[256];						\
	FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM 			\
		| FORMAT_MESSAGE_IGNORE_INSERTS, NULL, GetLastError(),	\
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),		\
		buf, (sizeof(buf) / sizeof(wchar_t)), NULL);		\
	printf("[debug] [%s:%s] [winerror] %s: %ls", __FILE__, __func__,\
		message, buf);						\

#else

#define debug(...) \
	;

#define debug_winerror(code) \
	;

#endif /* DEBUG */

extern pid_t game_proc_id;
extern HANDLE game_proc_handle;

extern RECT game_window_rect;
extern HWND game_window_handle;

/* process.c */
pid_t get_process_id(const char *proc_name);
HANDLE get_process_handle(const int proc_id);

/* window.c */
int get_window_coordinates(HWND window_handle, RECT *window_rect);
int get_window_handle(const pid_t process_id, void **out_window_handle);

/* mouse.c */
void set_mouse_position(int x, int y);

#endif /* RYTHMIC_H */
