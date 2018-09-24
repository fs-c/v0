#include "osu.h"

#include <stdio.h>

int execute_actions(int count, action *actions)
{
	printf("executing %d actions", count);

	INPUT *inputs;

	inputs = malloc(sizeof(INPUT) * count);

	int i;
	for (i = 0; i < count; i++) {
		INPUT *in = inputs + i;
		action *action = actions + i;

		in->ki.wVk = 0;
		in->ki.time = 0;
		in->ki.dwExtraInfo = 0;
		in->ki.wScan = action->key;
		in->ki.dwFlags = KEYEVENTF_UNICODE
			| (action->down ? 0x00 : KEYEVENTF_KEYUP);
	}

	SendInput(count, inputs, sizeof(INPUT));

	return i - count + 1;
}