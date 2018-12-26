#include "read.h"

#include <stdio.h>

FILE *stream;
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

	set_stream(stream);

	int test_endian = 1;
	if (*(char *)&test_endian == 1) {
		printf("on a little-endian system\n");

		little_endian = 1;
	} else printf("on a big-endian system\n");

	/* ruleset */
	BYTE mode = read_byte();
	printf("game mode: %d\n", (int)mode);

	/* game version */
	int32_t version = read_int32();
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

	/* Discard the next 6 shorts */
	sread(string_buffer, sizeof(int16_t), 6);

	int32_t score = read_int32();
	printf("score: %d\n", score);

	int16_t max_combo = read_int16();
	printf("max_combo: %d\n", max_combo);

	BYTE perfect = read_byte();
	printf("perfect: %d\n", perfect);

	int32_t mods_used = read_int32();
	printf("mods used: %d\n", mods_used);

	read_osu_string(string_buffer);
	printf("life bar: '%s'\n", string_buffer);

	int64_t timestamp = read_int64();
	printf("timestamp: %ld\n", timestamp);

	int32_t data_len = read_int32();
	printf("data_len: %d\n", data_len);
}
