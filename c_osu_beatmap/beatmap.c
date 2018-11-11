#include "beatmap.h"

size_t parse_beatmap(char *path);

int main(int argc, char *argv[])
{
	if (argc < 2) {
		printf("usage: %s <path to osu beatmap>\n", argv[0]);

		return EXIT_FAILURE;
	}

	parse_beatmap(argv[1]);
}

size_t parse_beatmap(char *path)
{
	FILE *stream;

	if (!(stream = fopen(path, "r"))) {
		debug("couldn't open file %s", path);

		return EXIT_FAILURE;
	}

	
}