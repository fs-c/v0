#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int in_len = 6;
char *const input[] = {
	"1.00 1 5.00 23.00",
	"1.00 3 6.00 23.00 10.00 -23.00 23.50 23.00",
	"0.50 2 10.00 0.00 500.00 3.00",
	"2.70 3 5.00 10.00 5.00 -10.00 20.00 0.00",
	"4.20 1 -100.00 -12.00",
	"9.53 10 -1.00 1.00 -2.00 2.00 3.00 -3.00 4.00 4.00 5.00 -5.00 6.00 6.00 7.00 7.00 -8.00 8.00 9.00 9.00 10.00 -10.00"
};

struct segment {
	double dist;
	double angle;
};

int main()
{
	double *args;

	for (int i = 0; i < in_len; i++) {
		parse_input(input[i], sizeof(input[i]), args);

		printf("%d %d %d\n", args[0], args[1], args[2]);
	}

	free(args);
}

void parse_input(const char *string, const int str_len, double *base,
	struct segment *segs)
{
	int i = 0;
	char *token = NULL;
	char *str = malloc(str_len);

	strcpy(str, string);

	while ((token = strsep(&str, " ")) && ++i) {
		if (i == 1) {
			*base = atof(token);
			continue;
		}

		
	}

	free(str);
}