#include <stdio.h>
#include <unistd.h>

static inline void handle_chunk(char *chunk, size_t chunk_size);

int cols = 8;
int col_size = 2;

int main()
{
	const int chunk_size = cols * col_size;

	int ci, r;
	// This is not valid ANSI C but that's okay for now.
	char buf[chunk_size + 1];

	while (r = read(0, buf, chunk_size)) {
		buf[r] = '\0';

		printf("%08x: ", ci++ * chunk_size);
		
		handle_chunk(buf, chunk_size);
	}
}

static inline void handle_chunk(char *chunk, size_t chunk_size)
{
	int col_i = 0, out = 0;

	for (int i = 0; i < chunk_size; i++) {
		if (chunk[i] == '\0')
			out = 1;

		if (out) {
			printf("%02x", chunk[i]);

			char c = chunk[i];
			if (c == '\n' || c == '\t' || c == ' ')
				chunk[i] = '.';
		} else {
			printf("  ");
		}

		if (++col_i >= col_size) {
			col_i = 0;

			printf(" ");
		}
	}
	printf(" %s\n", chunk);
}