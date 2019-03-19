#include <iostream>

#include <Windows.h>

inline void log_win_err(const std::string &msg)
{
	std::cerr << msg.c_str() << " (" << GetLastError() << ")" << std::endl;
}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		std::cerr << "usage: " << argv[0] << " <window title> "
			  << std::endl;

		return 1;
	}

	std::string title{argv[1]};

	HWND window = nullptr;
	if (!(window = FindWindowA(nullptr, title.c_str()))) {
		log_win_err("couldn't find window");

		return 1;
	}

	std::cout << "found window: " << window << std::endl;

	char *test_title = nullptr;
	if (!GetWindowTextA(window, test_title, 256)) {
		log_win_err("couldn't fetch window title");

		return 1;
	}

	std::cout << "window title: " << test_title << std::endl;
}
