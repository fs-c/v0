#include <stdio.h>

#include "osu.h";

int parse_hitpoints(char *path, hitpoint **points)
{
	FILE *stream;

	int pread;
	size_t nread;
	size_t len = 0;
	char *line = 0;

	if ((stream = fopen(path, "r")) == 0) {
		return -1;
	}

	while ((nread = getline(&line, &len, stream)) != -1) {

	}

	free(line);
	fclose(stream);

	return pread;
}