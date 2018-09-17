#include <stdio.h>
#include <unistd.h>

int main()
{
	const int cols = 8;
	const int col_size = 2;
	const int chunk_size = cols * col_size;

	int ci, r;
	char buf[chunk_size + 1];

	while (r = read(0, buf, chunk_size)) {
		int col_i = 0;

		printf("%08x: ", ci++ * chunk_size);

		for (int i = 0; i < chunk_size; i++) {
			if (i < r) {
				char c = buf[i];

				printf("%02x", buf[i]);

				if (c == '\n' || c == '\t' || c == ' ')
					buf[i] = '.';
			} else {
				printf("  ");
			}

			if (++col_i >= col_size) {
				col_i = 0;

				printf(" ");
			}
		}

		buf[chunk_size] = '\0';
		printf(" %s\n", buf);
	}
}