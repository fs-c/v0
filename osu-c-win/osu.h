#include <windows.h>

#define TIME_ADDRESS 0x052F59EC

extern HANDLE game_proc;

/**
 * proc.c
 * Returns the length the current maps song has been running for (aka the
 * gametime) from the game process.
 */
INT32 get_gametime();

/**
 * proc.c
 * Returns the ID of the process with the given name. Returns zero if the
 * process was not found or fetching the processes failed. Case sensitive.
 */
DWORD get_process_id(char *name);