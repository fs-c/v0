#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define RANGE 99999
#define NUM_TOTAL 20000

int bubble_sort(int *array, int total);
int insertion_sort(int *array, int total);

static inline __attribute__((__hot__)) void swap(int *e1, int *e2);

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
   takes about 0.65s for 20000 elements. */
int bubble_sort(int *array, int total)
{
	int new_total = 0, iters = 0, i;

	do {
		new_total = 0;

		for (i = 1; i < total; i++) {
			if (array[i - 1] > array[i]) {
				swap(array + i, array + i - 1);

				new_total = i;
			}
		}

		iters += i;
		total = new_total;
	} while (total);

	return iters;
}

/* Simple insertion sort -- this could be optimized further. Takes about 0.19s
   for 20000 elements. */
int insertion_sort(int *array, int total)
{
	int iters = 0, i = 1, j;

	while (i < total) {
		j = i++;

		while (j > 0 && array[j - 1] > array[j]) {
			swap(array + j, array + j - 1);

			j--;
		}

		iters += j;
	}

	return iters;
}

static inline __attribute__((hot)) void swap(int *e1, int *e2)
{
	register int t = *e1;
	*e1 = *e2;
	*e2 = t;
}
