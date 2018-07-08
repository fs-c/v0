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
DWORD find_pattern(HANDLE process, char *pattern)
{
	int hit = 0;
	int read_size = 4096;
	int sig_size = sizeof(pattern);

	char *chunk = malloc(read_size);

	// Move through process memory and get chunks.
	for (size_t i = 0; i < INT_MAX; i += read_size - sig_size) {
		ReadProcessMemory(process, (LPCVOID)i, &chunk, read_size, NULL);

		// For every piece of the chunk...
		for (int j = 0; j < read_size; j++) {
			hit = 1;
			
			// ...check if it matches the pattern/signature.
			for (int k = 0 ; k < sig_size && hit; k++) {
				if (chunk[j + k] != pattern[k]) {
					hit = 0;
				}
			}

			if (hit) {
				return i + j;
			}
		}
	}

	return 0;
}

INT32 get_gametime()
{
	INT32 time;

	ReadProcessMemory(game_proc, (LPCVOID)time_address, &time,
		sizeof(INT32), NULL);

	return time;
}