#ifndef OSU_H
#define OSU_H

#include <stdbool.h>
#include <windows.h>

#include "beatmap.h"

#define TIME_SIGNATURE "\xDB\x5D\xE8\x8B\x45\xE8\xA3"

extern HANDLE game_proc;
extern void *time_address;

/**
 * process.c
 * Returns the length the current maps song has been running for (aka the
 * gametime) from the game process.
 */
INT32 get_gametime();

/**
 * process.c
 * Returns the ID of the process with the given name. Returns zero if the
 * process was not found or fetching the processes failed. Case sensitive.
 */
DWORD get_process_id(char *name);

/**
 * process.c
 * Returns the address of the (end of the) first occurence of the given pattern
 * in the given processes memory.
 * Returns zero if the pattern was not found.
 */
void *find_pattern(HANDLE process, char *pattern);

/**
 * action.c
 * Executes count of the actions passed to it.
 * Returns nonzero on failure.
 */
int execute_actions(int count, action *actions);

#endif /* OSU_H */