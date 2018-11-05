#include "rythmic.h"

#include <tlhelp32.h>

HANDLE get_process_handle(const int proc_id)
{
	HANDLE proc_handle = NULL;

	if (!(proc_handle = OpenProcess(PROCESS_VM_READ, 0, proc_id))) {
		debug_winerror("failed to get handle to game process");

		return NULL;
	}

	debug("got handle to process with ID %d",
		(int)proc_id);

	return proc_handle;
}

pid_t get_process_id(const char *proc_name)
{
	if (!proc_name) {
		debug("received invalid process name");

		return 0;
	}

	pid_t proc_id = 0;
	HANDLE proc_list = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	PROCESSENTRY32 entry = { 0 };
	entry.dwSize = sizeof(PROCESSENTRY32);

	if (!(Process32First(proc_list, &entry))) {
		debug_winerror("couldn't get processes");

		goto exit_fail;
	}

	while (Process32Next(proc_list, &entry)) {
		if (_stricmp((char *)entry.szExeFile, proc_name) == 0) {
			CloseHandle(proc_list);

			proc_id = (pid_t)entry.th32ProcessID;

			debug("found process with id %d and name %s",
				(int)proc_id, proc_name);

			return proc_id;
		}
	}

	debug("couldn't find process with name '%s'", proc_name);

exit_fail:
	CloseHandle(proc_list);

	return 0;
}
