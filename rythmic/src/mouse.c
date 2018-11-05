#include "rythmic.h"

void set_mouse_position(int x, int y)
{
	INPUT input;

	input.type = INPUT_MOUSE;
	
	input.mi.dx = x;
	input.mi.dy = y;
	input.mi.mouseData = 0;
	input.mi.dwFlags = MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE;
	input.mi.time = 0;
	input.mi.dwExtraInfo = 0;

	if (!(SendInput(1, &input, sizeof(INPUT)))) {
		debug_winerror("failed sending input to %d/%d", x, y);

		return;
	}
}
