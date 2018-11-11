#ifndef BEATMAP_H
#define BEATMAP_H

#include <stdio.h>
#include <stdlib.h>

/* printf with added file and function name */
#define debug(...)					\
	printf("[debug] [%s:%s] ", __FILE__, __func__);	\
	printf(__VA_ARGS__);				\
	putchar('\n');					\

#endif /* BEATMAP_H */