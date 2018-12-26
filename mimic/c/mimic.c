#include <stdio.h>
#include <inttypes.h>

#define sread(dest, size, count)					\
	rd = fread(dest, size, count, stream);				\
	if (rd != count)						\
		printf("fread failed (%d, %d)\n", (int)rd, (int)count);	\

typedef unsigned char BYTE;

void u16_clendian(uint16_t *x);
void u32_clendian(uint32_t *x);

uint64_t read_uleb128();
size_t read_osu_string(char *out_buffer);

FILE *stream;
size_t rd = 0;

int little_endian = 0;

int main(int argc, char *argv[])
{
	setbuf(stdout, NULL);

	if (argc < 2) {
		printf("usage: %s <path to .osr file>\n", argv[0]);

		return 1;
	}

	if (!(stream = fopen(argv[1], "r"))) {
		printf("couldn't open file %s\n", argv[1]);

		return 1;
	} else printf("opened file %s for reading\n", argv[1]);

	int test_endian = 1;
	if (*(char *)&test_endian == 1) {
		printf("on a little-endian system\n");

		little_endian = 1;
	} else printf("on a big-endian system\n");

	/* ruleset */
	BYTE mode;
	sread(&mode, sizeof(BYTE), 1);
	printf("game mode: %d\n", (int)mode);

	/* game version */
	int32_t version;
	sread(&version, sizeof(version), 1);
	printf("game version: %d\n", version);

	char string_buffer[2048];

	/* md5 hash of beatmap */
	read_osu_string(string_buffer);
	printf("beatmap hash: '%s'\n", string_buffer);

	/* player name */
	read_osu_string(string_buffer);
	printf("player name: '%s'\n", string_buffer);
	
	/* md5 hash of the replay */
	read_osu_string(string_buffer);
	printf("replay hash: '%s'\n", string_buffer);

	uint16_t count = 0;

	sread(&count, sizeof(uint16_t), 1);
	printf("count300: %d\n", (int)count);
	sread(&count, sizeof(uint16_t), 1);
	printf("count100: %d\n", (int)count);
	sread(&count, sizeof(uint16_t), 1);
	printf("count50: %d\n", (int)count);
	sread(&count, sizeof(uint16_t), 1);
	printf("countGeki: %d\n", (int)count);
	sread(&count, sizeof(uint16_t), 1);
	printf("countKatu: %d\n", (int)count);
	sread(&count, sizeof(uint16_t), 1);
	printf("countMiss: %d\n", (int)count);

	int32_t score;
	sread(&score, sizeof(score), 1);
	printf("score: %d\n", score);

	int16_t max_combo;
	sread(&max_combo, sizeof(max_combo), 1);
	printf("max_combo: %d\n", max_combo);

	char perfect;
	sread(&perfect, sizeof(perfect), 1);
	printf("perfect: %d\n", perfect);

	int32_t mods_used;
	sread(&mods_used, sizeof(mods_used), 1);
	printf("mods used: %d\n", mods_used);

	read_osu_string(string_buffer);
	printf("life bar: '%s'\n", string_buffer);

	int64_t timestamp; 
	sread(&timestamp, sizeof(timestamp), 1);
	printf("timestamp: %ld\n", timestamp);

	int32_t data_len;
	sread(&data_len, sizeof(data_len), 1);
	printf("data_len: %d\n", data_len);
}

size_t read_osu_string(char *out_buffer)
{
	BYTE indicator;
	sread(&indicator, sizeof(BYTE), 1);

	if (indicator != 0x0b) {
		printf("string indicator set to invalid value\n");

		return 0;
	}

	uint64_t len = read_uleb128();

	printf("  len: %d\n", (int)len);

	for (size_t i = 0; i < len; i++) {
		sread(out_buffer++, sizeof(char), 1);
	}

	*out_buffer = '\0';

	return len;
}

/* Taken from Appendix C of Section 7.6 of the DWARF 3 spec.
 */
uint64_t read_uleb128()
{
	BYTE by = 0;
	int shift = 0;
	uint64_t result = 0;

	while (1) {
		sread(&by, sizeof(by), 1);

		result |= ((by & 0x7f) << shift);

		if (!(by & 0x80))
			break;

		shift += 7;
	}

	return result;
}

void u16_clendian(uint16_t *x)
{
	*x = (*x >> 8) | (*x << 8);
}

void u32_clendian(uint32_t *x)
{
	*x = (*x >> 24) | 
		((*x << 8) & 0x00FF0000) |
		((*x >> 8) & 0x0000FF00) |
		(*x << 24);
}
