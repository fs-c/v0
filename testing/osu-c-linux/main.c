#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/uio.h>

ssize_t process_vm_readv(int, struct iovec *, int, struct iovec *, int, int);

char *optarg;

int main(int argc, char **argv)
{
	char c;

	pid_t pid;
	char *map;
	int offset = 0;

	while ((c = getopt(argc, argv, "o:p:m:")) != -1) {
		switch (c) {
		case 'p':
			pid = strtol(optarg, NULL, 10);
			break;
		case 'm':
			map = optarg;
			break;
		case 'o':
			offset = strtol(optarg, NULL, 10);
			break;
		}
	}

	char cmd[1000];
	short cmdlen = snprintf(cmd, 1000, "node ../osu-node/player2.js -i -o %d -m %s",
		offset, map);

	short buf[1];
	ssize_t nread;
	struct iovec local[1];
	struct iovec remote[1];

	local[0].iov_base = buf;
	local[0].iov_len = sizeof(short);

	remote[0].iov_base = (void *)0x36e59ec;
	remote[0].iov_len = sizeof(short);

	short mns = 0;
	while (1) {
		nread = process_vm_readv(pid, local, 1, remote, 1, 0);

		short val = *buf;

		printf("%d\n", val);

		if (val < 50 && val > 0) {
			system(cmd);
			printf("running command %s\n", cmd);
			break;
		}

		mns = val < 0;

		nanosleep((const struct timespec[]){{0, 1000000L}}, NULL);
	}

	printf("exited loop\n");
	
	return 0;
}