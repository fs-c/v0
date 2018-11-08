#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#define RANGE 99999
#define NUM_TOTAL 20000

#define SHOW_SAMPLE 0

#define swap(e1, e2)		\
	temp = *(e1);		\
	*(e1) = *(e2);		\
	*(e2) = temp;		\

/* All sorting functions return the number of iterations performed.
 */
int bubble_sort(int *array, int total);
int insertion_sort(int *array, int total);
int selection_sort(int *array, int total);

static inline double time_diff(struct timeval start, struct timeval end);

int (*funcs[])(int *, int) = { bubble_sort, insertion_sort, selection_sort };
char *func_names[] = { "bubble_sort", "insertion_sort", "selection_sort" };

int temp = 0;

int main()
{
	struct timeval start_time, end_time;
	gettimeofday(&start_time, NULL);

	srand(start_time.tv_usec);

	const int array_size = sizeof(int) * NUM_TOTAL;

	/* Create a random, an ordered and a reversed array to benchmark the
	   sorting algorithms. All algos will run over a copy of these arrays to
	   get a fair and accurate time.  */

	int *ran_array = malloc(array_size);
	int *ord_array = malloc(array_size);
	int *rev_array = malloc(array_size);	
	for (int i = 0; i < NUM_TOTAL; i++) {
		ord_array[i] = i;
		rev_array[i] = NUM_TOTAL - i;
		ran_array[i] = rand() % RANGE;
	}

	int *arrays[] = { ran_array, ord_array, rev_array };
	char *array_names[] = {
		"random_array", "ordered_array", "reversed_array"
	};

	/* For every function... */
	for (int i = 0; i < (signed)(sizeof(funcs) / sizeof(void *)); i++) {
		int (*func)(int *array, int total) = funcs[i];

		printf("%s:\n", func_names[i]);

		/* For every array... */
		for (int j = 0; j < 3; j++) {
			printf("\t%s:\n", array_names[j]);

			/* Always work on a copy of the array, we don't want to
			   sort the original. */
			int *array = malloc(array_size);
			memcpy(array, arrays[j], array_size);
			
			gettimeofday(&start_time, NULL);

			int iter = (*func)(array, NUM_TOTAL);

			gettimeofday(&end_time, NULL);

			double exec_time = time_diff(start_time, end_time);

			printf("\t\titerations: %d\n", iter);
			printf("\t\texec time: %fs (%fs/e)\n", exec_time,
				exec_time / NUM_TOTAL);

#ifdef SHOW_SAMPLE
			int ind = 0;
			double fac = NUM_TOTAL / (SHOW_SAMPLE);
			for (int k = 0; k < SHOW_SAMPLE; k++, ind = k * fac)
				printf("\t\t\t%d: %d\n", ind, array[ind]);
#endif

		}

		printf("\n");
	}
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
 * Simple version, could be optimized further. Random: ~0.19s/20000
 */
int insertion_sort(int *array, int total)
{
	int iters = 0, j = 0, i = 1;

	while ((j = i++) < total - 1) {
		while (j > 0 && array[j - 1] > array[j]) {
			swap(array + j, array + j - 1);

			j--;
		}

		iters += i - 1 - j;
	}

	return iters + i;
}

int fast_insertion_sort(int *array, int total)
{
	int i = 0, t, j = 0, iters = 0;

	for (int i = 0; i < total; i++) {
		j = i;
		t = array[i];

		while ((j > 0) && (array[j - 1] > t)) {
			array[j] = array[j];

			j--;
		}

		array[j] = t;
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

static inline double time_diff(struct timeval start, struct timeval end)
{
	return (double)(end.tv_usec - start.tv_usec)
		/ 1000000 + (double)(end.tv_sec - start.tv_sec);
}
