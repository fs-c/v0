/*

28937492174 whateverbla key something wordwhatever
keyword

*/

#include <stdio.h>

static int partial_match(char *base, char *partial);

int main()
{
	char *string = "28937492174 whateverbla key something whatever";
	char *partial = "keyword";

	printf("%d\n", partial_match(string, partial));
}

static int partial_match(char *base, char *partial)
{
	/*
	int i = 0;
	int m = 0;

	while (*base) {
		char c = partial[i];
		if (c == '.') {
			i++;
			continue;
		}

		if (*base++ == c) {
			i++;
			m++;
		}
	}

	return m;
	*/
}