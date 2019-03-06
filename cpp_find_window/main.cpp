#include <iostream>

#include <Windows.h>

int main(int argc, char *argv[])
{
	if (argc < 2) {
		std::cerr << "usage: " << argv[0] << " <window title> "
			<< std::endl;

		return 1;
	}

	std::string title {argv[1]};

	HWND window = nullptr;
	if (!FindWindowA(nullptr, title.c_str())) {
		std::cerr << "couldn't find window (" << GetLastError() << ")"
			<< std::endl;

		return 1;
	}

	std::cout << "found window: " << window << std::endl;
}
