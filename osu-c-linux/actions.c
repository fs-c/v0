#include <stdlib.h> // malloc(), realloc()

#include "osu.h"

static inline int col_to_modcode(int col);

/**
 * Converts an array of hitpoint structs (pointed at by **points) into an array
 * of action structs (pointed to by **actions, to be empty).
 * Every hitpoint is converted into two actions for keyup and keydown
 * respectively. Note that actions are not sorted.
 * Returns the number of actions written.
 */
int hitpoints_to_actions(int count, hitpoint **points, action **actions)
{
	hitpoint *curp;

	*actions = malloc((2 * count) * sizeof(action));

	int i = 0;
	int j = 0;

	while (i < count) {
		curp = (*points) + i++;

		action *keydw = (*actions) + j++;
		action *keyup = (*actions) + j++;

		hitpoint_to_action(curp, keydw, keyup);
	}

	// TODO: Check if all of the memory originally allocated for *actions
	// 	 is actually in use. (Don't use realloc because of performance)

	return j;
}

// TODO: Ugly and too specific; make a charcode to modcode function.
static inline int col_to_modcode(int col)
{
	return col == 0 ? 40 : col == 1 ? 41 : col == 2 ? 44 : col == 3 ? 45 : 0;
}

/**
 * Populates *ac1 and *ac2 with data from hitpoint *point.
 */
void hitpoint_to_action(hitpoint *point, action *ac1, action *ac2)
{
	ac1->type = 1; // Keydown.
	ac1->time = point->stime;

	ac2->type = 0; // Keyup.
	ac2->time = point->etime;

	int code = col_to_modcode(point->column);

	ac1->code = code;
	ac2->code = code;
}

/**
 * Selection sort (by time) on the array of actions pointed at by **actions.
 * Returns nonzero value on failure.
 */
int sort_actions(int size, action **actions)
{
	int i, j, min;
	action *act = *actions, tmp;

	for (i = 0; i < (size - 1); i++) {
		min = i;

		for (j = i + 1; j < size; j++)
			if ((act + j)->time < (act + min)->time) min = j;

		tmp = *(act + i);
		*(act + i) = *(act + min);
		*(act + min) = tmp;
	}

	return (i + 1) - size;
}