#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#define RANGE 99999
#define NUM_TOTAL 20000

int bubble_sort(int *array, int total);
int insertion_sort(int *array, int total);
int selection_sort(int *array, int total);

static inline __attribute__((__hot__)) void swap(int *e1, int *e2);

int main()
{
	struct timeval start_time, end_time;
	gettimeofday(&start_time, NULL);

	srand(start_time.tv_usec);

	int *num_array = malloc(sizeof(int) * NUM_TOTAL);
	for (int i = 0; i < NUM_TOTAL; i++)
		num_array[i] = rand() % RANGE;
		// num_array[i] = NUM_TOTAL - i;

	gettimeofday(&start_time, NULL);

	int iter = selection_sort(num_array, NUM_TOTAL);

	gettimeofday(&end_time, NULL);

	int show = 20, ind = 0;
	for (int i = 0; i < show; i++, ind = i * (NUM_TOTAL / show))
		printf("%d: %d\n", ind, num_array[ind]);

	printf("went through %d iterations\n", iter);

	double exec_time = (double)(end_time.tv_usec - start_time.tv_usec)
		/ 1000000 + (double)(end_time.tv_sec - start_time.tv_sec);
	printf("executed in %f seconds\n~%fs per element\n", exec_time,
		exec_time / NUM_TOTAL);
}

/* https://en.wikipedia.org/wiki/Bubble_sort
 * Optimized version. Random: ~0.38s/20000
 */
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

/* https://en.wikipedia.org/wiki/Insertion_sort
 * Simple version, could be optimized further. Random: ~0.20s/20000
 */
int insertion_sort(int *array, int total)
{
	int iters = 0, j = 0, i = 1;

	while (i < total) {
		j = i++;

		while (j > 0 && array[j - 1] > array[j]) {
			swap(array + j, array + j - 1);

			j--;
		}

		iters += i - 1 - j;
	}

	return iters + i;
}

/* https://en.wikipedia.org/wiki/Selection_sort
 * Basic implementation. Random: ~0.18s/20000 and O(n²)
 */
int selection_sort(int *array, int total)
{
	int iters = 0, i, j;
	
	for (i = 0; i < total - 1; i++) {
		int min = i;

		for (j = i + 1; j < total - 1; j++)
			if (array[j] < array[min])
				min = j;

		if (min != i)
			swap(array + i, array + min);
		
		iters += j;
	}

	return iters + i;
}

/* This will incur heavy perfomance losses unless the compiler actually inlines
   it -- always use at least -O1. */
static inline __attribute__((hot)) void swap(int *e1, int *e2)
{
	register int t = *e1;
	*e1 = *e2;
	*e2 = t;
}
