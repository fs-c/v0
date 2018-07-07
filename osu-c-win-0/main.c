#include <stdio.h>

#include "osu.h"

inline void set_key_pressed(char key, int pressed);

int main(int argc, char **argv)
{
	char *map = argv[1];

	int acread = 0;
	action *actions = NULL;
	if (!(acread = parse_beatmap(map, &actions)) || !actions) {
		printf("failed to parse beatmap");
		return 1;
	}
}

inline void set_key_pressed(char key, int pressed)
{
	INPUT key_press = {0};

	key_press.type = INPUT_KEYBOARD;

	key_press.ki.wScan = 0;
	key_press.ki.dwExtraInfo = 0;
	key_press.ki.dwFlags = (pressed ? 0 : KEYEVENTF_KEYUP);
	key_press.ki.wVk = VkKeyScanEx(key, GetKeyboardLayout(NULL)) & 0xFF;

	SendInput(1, &key_press, sizeof(INPUT));
}