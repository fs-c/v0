#include <windows.h>

#define TAPTIME_MS 2
#define TIME_ADDRESS 0x052F59EC

extern HANDLE game_proc;

/* proc.c */
INT32 get_gametime();
DWORD get_process_id(char *name);