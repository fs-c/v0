#include <stdio.h>
#include <unistd.h>

#include "osu.h"

int opterr;
char *optarg = 0;

int main(int argc, char **argv)
{
	char *map, c;

	while ((c = getopt(argc, argv, "m:")) != -1) {
		switch (c) {
		case 'm':
			map = optarg;
			break;
		}
	}

	hitpoint point = { 0, 0, 0 };
}