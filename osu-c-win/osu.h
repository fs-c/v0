#ifndef OSU_H
#define OSU_H

#include <windows.h>

#include "beatmap.h"

#define TIME_ADDRESS 0x017159E0

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

/**
 * action.c
 * Executes count of the actions passed to it.
 * Returns nonzero on failure.
 */
int execute_actions(int count, action *actions);

#endif /* OSU_H */