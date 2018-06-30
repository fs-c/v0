#include "osu.h"

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

	*actions = malloc(2 * sizeof(action));

	int i = 0;
	int j = 0;

	while (i < count) {
		curp = (*points) + i++;
		
		*actions = realloc(*actions, (j += 2) * sizeof(action));

		action *keydw = (*actions) + (j - 2);
		action *keyup = (*actions) + (j - 1);

		hitpoint_to_action(curp, keydw, keyup);
	}

	return j;
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

	int col = point->column;
	// Get the character code corresponding to the column number.
	// TODO: Ugly.
	char code = col == 0 ? 'd' : col == 1 ? 'f' : col == 2 ? 'j'
		: col == 3 ? 'k' : 'i';

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

		tmp = *(act + min);

		*(act + min) = *(act + j);
		*(act + j) = tmp;
	}

	return (i + 2) - size;
}
