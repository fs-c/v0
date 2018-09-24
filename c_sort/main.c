#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define RANGE 99999
#define NUM_TOTAL 20000

int bubble_sort(int *num_array, int num_total);

int main()
{
	struct timeval start_time, end_time;
	gettimeofday(&start_time, NULL);

	srand(start_time.tv_usec);

	int *num_array = malloc(sizeof(int) * NUM_TOTAL);
	for (int i = 0; i < NUM_TOTAL; i++)
		num_array[i] = rand() % RANGE;

	gettimeofday(&start_time, NULL);

	int iter = bubble_sort(num_array, NUM_TOTAL);

	gettimeofday(&end_time, NULL);

	for (int i = 0; i < NUM_TOTAL; i += 1000)
		printf("%d: %d\n", i, num_array[i]);

	printf("went through %d iterations\n", iter);

	printf("executed in %f seconds\n",
		(double)(end_time.tv_usec - start_time.tv_usec) / 1000000 +
		(double)(end_time.tv_sec - start_time.tv_sec));
}

/* Optimized bubble sort as also showcased on the Bubble Sort Wikipedia article,
   takes about 1.3s for 20000 elements. */
int bubble_sort(int *num_array, int num_total)
{
	int new_total = 0, iters = 0, temp = 0, i;

	do {
		new_total = 0;

		for (i = 1; i < num_total; i++) {
			if (num_array[i - 1] > num_array[i]) {
				temp = num_array[i];
				num_array[i] = num_array[i - 1];
				num_array[i - 1] = temp;

				new_total = i;
			}
		}

		iters += i;
		num_total = new_total;
	} while (num_total);

	return iters;
}
