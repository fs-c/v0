#include <iostream>

int main(int argc, char *argv[])
{
	if (argc < 4) {
		std::cerr << "usage: " << argv[0] << " <radius>";

		return 1;
	}

	double radius = atoi(argv[1]);
} 
