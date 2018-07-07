#include <tlhelp32.h>

#include "osu.h"

inline const DWORD get_process_id(DWORD *name)
{
	DWORD proc_id = NULL;
	HANDLE proc_list = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, NULL);

	PROCESSENTRY32 entry = {0};
	entry.dwSize = sizeof(PROCESSENTRY32);

	if (Process32First(proc_list, &entry)) {
		CloseHandle(proc_list);

		return proc_id;
	}

	while (Process32Next(proc_list, &entry)) {
		if (_wcsicmp(entry.szExeFile, name) == 0) {
			proc_id = entry.th32ProcessID;
		}
	}

	CloseHandle(proc_list);

	return proc_id;
}

inline const INT32 get_gametime()
{
	INT32 time;

	ReadProcessMemory(game_proc, (void *)TIME_ADDRESS, &time,
		sizeof(INT32), NULL);

	return time;
}