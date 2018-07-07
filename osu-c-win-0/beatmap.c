#include <stdio.h>

#include "osu.h"

struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct hitpoint hitpoint;

static inline char col_to_char(int col);
static char *strsep(char **sp, char *sep);
static int string_to_actions(char *line, action *ac1, action *ac2);

int parse_beatmap(char *path, action **actions)
{
	FILE *stream;

	if ((stream = fopen(path, "r")) == 0) {
		return -1;
	}

	int acs_parsed;

	size_t nread;
	size_t len = 0;
	char *line = 0;
	char in_obj = 0;
	while ((nread = fgets(&line, &len, stream)) != -1) {
		if (!in_obj && (strstr(line, "[HitObjects]") != 0)) {
			in_obj = 1;

			*actions = malloc(sizeof(action) * 2);

			continue;
		} else if (!in_obj) continue;

		action start, end;
		string_to_actions(line, &start, &end);

		size_t nsiz = (acs_parsed += 2) * sizeof(action);
		*actions = realloc(*actions, nsiz);

		(*actions)[acs_parsed - 1] = end;
		(*actions)[acs_parsed - 2] = start;
	}

	free(line);
	fclose(stream);

	return acs_parsed;
}

static int string_to_actions(char *line, action *start, action *end)
{
	hitpoint pnt;
	char *ln = _strdup(line), i = 0, *token, *eln;

	// CSV Syntax: x, y, time, type, hitSound, extras
	// Extras: time:a:b:c:d:
	while (token = strsep(&ln, ",")) {
		int tval = strtol(token, NULL, 10);

		switch (i) {
		case 0: pnt.column = tval / (512 / 4);
			break;
		case 2: pnt.stime = tval;
			break;
		case 3: pnt.type = tval;
			break;
		case 5: // Extra string, first token is end time.
			eln = strdup(token);

			int etime = strtol(strsep(&eln, ":"), NULL, 10);
			pnt.etime = etime ? etime
				: pnt.stime + TAPTIME_MS;
			break;
		}

		i++;
	}

	end->down = 0;
	start->down = 1;

	end->time = pnt.etime;
	start->time = pnt.stime;

	char code = col_to_char(pnt.column);

	end->ch = code;
	start->ch = code;
}

// Thanks, Dan Cross. (http://unixpapa.com/incnote/string.html)
static char *strsep(char **sp, char *sep)
{
	char *p, *s;
	if (sp == NULL || *sp == NULL || **sp == '\0') return(NULL);
	s = *sp;
	p = s + strcspn(s, sep);
	if (*p != '\0') *p++ = '\0';
	*sp = p;
	return(s);
}

static inline char col_to_char(int col)
{
	return col == 0 ? 'd' : col == 1 ? 'f' : col == 2 ? 'j' : col == 3 ? 'k'
		: 0;
}