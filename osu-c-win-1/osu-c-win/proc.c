#include "stdafx.h"

#include <tlhelp32.h>

inline const DWORD get_process_id(WCHAR *name)
{
	DWORD proc_id;
	HANDLE proc_list = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	PROCESSENTRY32 entry = { 0 };
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
