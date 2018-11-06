#include "rythmic.h"

#define MEMORY_CHUNK_SIZE 4096

typedef unsigned char BYTE;

static void *find_pattern(const BYTE *signature,
	const size_t sig_len);
static inline void *check_chunk(const BYTE *signature, const size_t sig_len,
	const BYTE *buffer, const size_t buf_len);

void *get_game_time_address()
{
	void *address_ptr = find_pattern((BYTE *)SIGNATURE,
		sizeof(SIGNATURE) - 1), *adress = NULL;

	if (!address_ptr)
		return NULL;

	size_t read = 0;
	read_game_memory(address_ptr, &adress, sizeof(DWORD),
		&read);

	if (!read)
		return NULL;

	return adress;
}

static void *find_pattern(const BYTE *signature, const size_t sig_len)
{
	const size_t read_size = MEMORY_CHUNK_SIZE;
	BYTE chunk[read_size];

	for (size_t off = 0; off < INT_MAX; off += read_size - sig_len) {
		read_game_memory((void *)off, chunk, read_size, NULL);

		void *hit = check_chunk(signature, sig_len, chunk, read_size);

		if (hit)
			return (void *)((intptr_t)off + (intptr_t)hit);
	}

	return NULL;
}

/* TODO: Implement a more efficient pattern matching algorithm */
static inline void *check_chunk(const BYTE *signature, const size_t sig_len,
	const BYTE *chunk, const size_t buf_len)
{
	for (size_t i = 0; i < buf_len; i++) {
		int hit = 1;

		for (size_t j = 0; j < sig_len && hit; j++)
			hit = chunk[i + j] == signature[j];

		if (hit)
			return (void *)(i + sig_len);
	}

	return NULL;
}
