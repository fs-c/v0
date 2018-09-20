#include <stdio.h>
#include <unistd.h>

void handle_chunk(char *chunk, size_t chunk_actual, size_t chunk_max);

int cols = 8;
int col_size = 2;

int main()
{
	const int chunk_size = cols * col_size;

	int ci, r;
	// Because a const isn't constant enough.
	char buf[(cols * col_size) + 1];

	while (r = read(0, buf, chunk_size)) {
		printf("%08x: ", ci++ * chunk_size);
		
		handle_chunk(buf, r, chunk_size);
	}
}

void handle_chunk(char *chunk, size_t chunk_actual, size_t chunk_max)
{
	int col_i = 0;

	for (int i = 0; i < chunk_max; i++) {
		if (i < chunk_actual) {
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

	chunk[chunk_max] = '\0';
	printf(" %s\n", chunk);
}