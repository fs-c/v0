#include "rythmic.h"

void set_mouse_position(int x, int y)
{
	SendMessage(game_window_handle, WM_MOUSEMOVE, 0, MAKELPARAM(x, y));
}
