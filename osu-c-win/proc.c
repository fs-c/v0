#include "osu.h"

#include <stdio.h>
#include <tlhelp32.h>

DWORD get_process_id(char *name)
{
	DWORD proc_id;
	HANDLE proc_list = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	PROCESSENTRY32 entry = {0};
	entry.dwSize = sizeof(PROCESSENTRY32);

	if (!Process32First(proc_list, &entry)) {
		CloseHandle(proc_list);

		return 0;
	}

	while (Process32Next(proc_list, &entry)) {
		if (_stricmp((char *)entry.szExeFile, name) == 0) {
			proc_id = entry.th32ProcessID;
		}
	}

	CloseHandle(proc_list);

	return proc_id;
}

INT32 get_gametime()
{
	INT32 time;

	ReadProcessMemory(game_proc, (void *)TIME_ADDRESS, &time,
		sizeof(INT32), NULL);

	return time;
}