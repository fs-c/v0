#include "rythmic.h"

struct beatmap {
	int mode;
	int set_id;
	int map_id;

	char title[256];
	char artist[256];
	char version[256];

	int circle_size;
	int overall_difficulty;

	int silder_tickrate;
	int slider_multiplier;
};

struct hitobject {
	int x;
	int y;
	int time;
	int type;
};

size_t parse_beatmap(char *path, struct beatmap *map,
	struct hitobjects *objects)
{
	
}
