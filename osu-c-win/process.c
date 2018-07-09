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
		if (strcmp((char *)entry.szExeFile, name) == 0) {
			proc_id = entry.th32ProcessID;
		}
	}

	CloseHandle(proc_list);

	return proc_id;
}
// TODO: This function looks horrible, do something about it.
//	 It's also really inefficient, maybe implement Boyer-Moore's algo?
void *find_pattern(HANDLE process, char *pattern)
{
	bool hit = false;
	size_t chk_s = 4096;
	size_t sig_s = sizeof(pattern);

	unsigned char *chk;

	// Move through process memory in chunks.
	for (size_t chk_i = 0; chk_i < INT_MAX; chk_i += chk_s) {
		ReadProcessMemory(process, (void *)chk_i, &chk, chk_s, NULL);

		// For every piece of the chunk...
		for (size_t pc_i = 0; pc_i < chk_s; pc_i++) {
			hit = true;

			// ...check if it matches the pattern.
			for (size_t sig_i = 0; sig_i < sig_s; sig_i++) {
				if (chk[pc_i + sig_i] != pattern[sig_i]) {
					hit = false;
				}
			}

			if (hit) {
				return (void *)(chk_i + pc_i);
			}
		}
	}

	return NULL;
}

INT32 get_gametime()
{
	INT32 time;

	ReadProcessMemory(game_proc, (LPCVOID)time_address, &time,
		sizeof(INT32), NULL);

	return time;
}