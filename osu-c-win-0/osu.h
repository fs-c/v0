#include <windows.h>

#define TAPTIME_MS 3
#define TIME_ADDRESS 0x015159EC

struct action {
	char ch;
	int time;
	int down;
};

typedef struct action action;

/* proc.c */
inline const INT32 get_gametime();
inline const DWORD get_process_id(char *name);

/* beatmap.c */
int parse_beatmap(char *path, action **actions);

extern HANDLE game_proc;