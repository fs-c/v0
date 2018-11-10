#include <stdio.h>
#include <inttypes.h>

#define dbgread(dest, size, count)					\
	rd = fread((void *)dest, size, count, stream);			\
	printf("got: %d, wanted: %d * %d (%d)\n",			\
		(int)(rd * size), (int)size, (int)count, size * count);	\

uint64_t uleb_extract();
size_t read_osu_string(void *out_buffer);

FILE *stream;
size_t rd = 0;

typedef unsigned char BYTE;

int main(int argc, char *argv[])
{
	setbuf(stdout, NULL);
	setbuf(stream, NULL);

	if (argc < 2) {
		printf("usage: %s <path to .osr file>\n", argv[0]);

		return 1;
	}

	if (!(stream = fopen(argv[1], "r"))) {
		printf("couldn't open file %s\n", argv[1]);

		return 1;
	} else printf("opened file %s for reading\n", argv[1]);

	BYTE mode;
	dbgread(&mode, sizeof(BYTE), 1);

	printf("game mode: %d\n", (int)mode);

	int32_t version;
	dbgread(&version, sizeof(version), 1);

	printf("game version: %d\n", version);

	char beatmap_hash[4096];
	read_osu_string(beatmap_hash);

	printf("beatmap hash: %s\n", beatmap_hash);
}

size_t read_osu_string(void *out_buffer)
{
	BYTE indicator;
	dbgread(&indicator, sizeof(BYTE), 1);

	if (indicator != 0x0b) {
		printf("string indicator set to invalid value");

		return 0;
	}

	uint64_t len = uleb_extract();

	printf("len: %d\n", len);

	/*
	for (size_t i = 0; i < len; i++) {
		dbgread(out_buffer++, sizeof(char), 1);
	}
	*/

	return len;
}

uint64_t uleb_extract()
{
#ifdef notdef
	/* This is taken from the opensolaris linker, I don't think they will
	   mind much. */
	uint64_t	dot = *dotp;
	uint64_t	res = 0;
	int		more = 1;
	int		shift = 0;
	int		val;

	data += dot;

	while (more) {
		/*
		 * Pull off lower 7 bits
		 */
		val = (*data) & 0x7f;

		/*
		 * Add prepend value to head of number.
		 */
		res = res | (val << shift);

		/*
		 * Increment shift & dot pointer
		 */
		shift += 7;
		dot++;

		/*
		 * Check to see if hi bit is set - if not, this
		 * is the last byte.
		 */
		more = ((*data++) & 0x80) >> 7;
	}

	*dotp = dot;
	return (res);
#endif

	BYTE by = 0;
	int shift = 0;
	uint64_t res = 0;

	while (1) {
		dbgread(&by, sizeof(by), 1);

		res |= (by & 0x7f) << shift;

		if ((by & 0x80) >> 7)
			break;
		
		shift += 7;
	}

	return res;
}
