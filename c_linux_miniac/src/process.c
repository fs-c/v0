#include "process.h"

static inline void *check_chunk(const unsigned char *sig, size_t sig_size,
	unsigned char *buf, size_t buf_size);

unsigned long get_process_id(const char *name)
{
	unsigned long proc_id = 0;

#ifdef ON_LINUX
	char *cmd = (char *)calloc(1, 200);
	sprintf(cmd, "pidof %s", name);

	FILE *f = popen(cmd, "r");
	size_t read = fread(cmd , 1, 200, f);

	fclose(f);

	proc_id = read ? atoi(cmd) : 0;

	free(cmd);
#endif /* ON_LINUX */

#ifdef ON_WINDOWS
	HANDLE proc_list = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	PROCESSENTRY32 entry = { 0 };
	entry.dwSize = sizeof(PROCESSENTRY32);

	if (!Process32First(proc_list, &entry)) {
		goto end;
	}

	while (Process32Next(proc_list, &entry)) {
		if (_stricmp((char *)entry.szExeFile, name) == 0) {
			proc_id = (unsigned long)entry.th32ProcessID;

			goto end;
		}
	}

end:
	CloseHandle(proc_list);
#endif /* ON_WINDOWS */

	debug("process ID for %s is %ld", name, proc_id);

	return proc_id;
}

void *get_time_address()
{
#ifdef ON_WINDOWS
	void *time_address = NULL;
	void *time_ptr = find_pattern((unsigned char *)SIGNATURE,
		sizeof(SIGNATURE) - 1);

	if (!ReadProcessMemory(game_proc, (void *)time_ptr, &time_address,
		sizeof(DWORD), NULL))
	{
		return NULL;
	}

	return time_address;
#endif

#ifdef ON_LINUX
	return (void *)LINUX_TIME_ADDRESS;
#endif
}

void *find_pattern(const unsigned char *signature, unsigned int sig_len)
{
	const size_t read_size = 4096;
	unsigned char chunk[read_size];

	// Get reasonably sized chunks of memory...
	for (size_t off = 0; off < INT_MAX; off += read_size - sig_len) {
		if (!(read_game_memory((void *)off, chunk, read_size))) {
			continue;
		}

		// ...and check if they contain our signature.
		void *hit = check_chunk(signature, sig_len, chunk, read_size);

		if (hit)
			return (void *)(off + (intptr_t)hit);
	}

	return NULL;
}

// TODO: Use a more efficient pattern matching algorithm.
static inline void *check_chunk(const unsigned char *sig, size_t sig_size,
	unsigned char *buf, size_t buf_size)
{
	// Iterate over the buffer...
	for (size_t i = 0; i < buf_size; i++) {
		int hit = true;

		// ...to check if it includes the pattern/sig.
		for (size_t j = 0; j < sig_size && hit; j++) {
			hit = buf[i + j] == sig[j];
		}

		if (hit) {
			return (void *)(i + sig_size);
		}
	}

	return NULL;
}
