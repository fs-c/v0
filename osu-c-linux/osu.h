struct hitpoint {
	int type;
	int stime;
	int etime;
	int column;
};

typedef struct hitpoint hitpoint;

int parse_hitpoint(char *path, hitpoint **points);