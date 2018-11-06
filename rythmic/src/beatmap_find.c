#include "rythmic.h"

#include <dirent.h>

/**
 * Searches for a file or folder in `base`, matching all directory entries
 * against `partial`. The best match is returned through *out_file.
 * Returns the length of the matched path or zero on failure.
 */
static size_t find_partial_file(char *base, char *partial,
	char *out_file, const size_t out_size);

/**
 * Given a base, returns the number of concurrent characters which match
 * partial.
 */
static int partial_match(char *base, char *partial);

size_t find_beatmap(char *base, char *partial, char *map,
	const size_t map_size)
{
	size_t folder_len = 0;
	char folder[map_size];

	if (!(folder_len = find_partial_file(base, partial, folder,
		map_size)))
	{
		debug("couldn't find folder (partial: %s)", partial);

		return 0;
	}

	const size_t base_len = strlen(base);

	strcpy_s(map, map_size, base);
	/* Add folder name */
	strcpy_s(map + base_len, map_size, folder);
	/* Add trailing seperator and null terminate the string */
	strcpy_s(map + base_len + folder_len, map_size,
		(char[2]){ SEPERATOR, '\0' });

	size_t beatmap_len = 0;
	char beatmap[map_size];

	if (!(beatmap_len = find_partial_file(map, partial, beatmap,
		map_size)))
	{
		debug("couldn't find beatmap in %s", map);

		return 0;
	}

	/* map is now the absolute path to our beatmap */
	strcpy_s(map + base_len + folder_len + 1, map_size, beatmap);

	const size_t map_len = base_len + folder_len + 1 + beatmap_len;

	/* Verify that the file we found is a beatmap */
	if (strcmp(map + map_len - 4, ".osu")) {
		debug("%s is not a beatmap", map);

		return 0;
	}

	return map_len;
}

static size_t find_partial_file(char *base, char *partial,
	char *out_file, const size_t out_size)
{
	DIR *dp;
	struct dirent *ep;

	if (!(dp = opendir(base))) {
		debug("couldn't open directory %s", base);

		return 0;
	}

	int best_match = 0;

	while((ep = readdir(dp))) {
		char *name = ep->d_name;
		int score = partial_match(name, partial + 8);

		if (score > best_match) {
			best_match = score;

			strcpy_s(out_file, out_size, name);
		}
	}

	closedir(dp);

	return strlen(out_file);
}

/* TODO: I'm certain there's a more elegant way to go about this. */
static int partial_match(char *base, char *partial)
{
	int i = 0;
	int m = 0;

	while (*base) {
		char c = partial[i];
		if (c == '.') {
			i++;
			continue;
		}

		if (*base++ == c) {
			i++;
			m++;
		}
	}

	return m;
}
