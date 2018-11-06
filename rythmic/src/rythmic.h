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
#define debug(...)					\
	printf("[debug] [%s:%s] ", __FILE__, __func__);	\
	printf(__VA_ARGS__);				\
	putchar('\n');					\

/* Prints formatted GetLastError alongside a message */
#define debug_winerror(...) 						\
	debug(__VA_ARGS__);						\
	char wemsg[256];						\
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM 			\
		| FORMAT_MESSAGE_IGNORE_INSERTS, NULL, GetLastError(),	\
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),		\
		wemsg, (sizeof(wemsg) / sizeof(char)), NULL);		\
	printf("[debug] [winerror] %s", wemsg);				\

#else

#define debug(...) \
	;

#define debug_winerror(...) \
	;

#endif /* DEBUG */

extern pid_t game_proc_id;
extern HANDLE game_proc_handle;

extern RECT game_window_rect;
extern HWND game_window_handle;

extern void *game_time_address;

/* process.c */
pid_t get_process_id(const char *proc_name);
HANDLE get_process_handle(const int proc_id);

/* window.c */
#define get_window_title(title, max_len) 			\
	GetWindowText(game_window_handle, *title, max_len)	\

int get_window_coordinates(HWND window_handle, RECT *window_rect);
int get_window_handle(const pid_t process_id, void **out_window_handle);

/* mouse.c */
void set_mouse_position(int x, int y);

/* memory.c */
#define read_game_memory(offset, buffer, size, read)			\
	ReadProcessMemory(game_proc_handle, (LPCVOID)offset, buffer,	\
		size, (SIZE_T *)read);					\

#define get_game_time(time)						\
	read_game_memory(game_time_address, time, 4, NULL);		\

void *get_game_time_address();

/* beatmap.c */
size_t find_beatmap(char *base, char *partial, char *map,
	const size_t map_size);

#endif /* RYTHMIC_H */
