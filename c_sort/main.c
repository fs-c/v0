#include <stdio.h>
#include <stdlib.h>

#define RANGE 99999
#define NUMBERS 20000

int main()
{
	srand(time(NULL));

	int *num_array = malloc(sizeof(int) * NUMBERS);
	for (int i = 0; i < NUMBERS; i++)
		num_array[i] = rand() % RANGE;
	
	
}