#include <sys/uio.h>

#include "osu.h"

// TODO: Where the hell is this thing defined normally?
ssize_t process_vm_readv(pid_t pid,
                         const struct iovec *local_iov,
                         unsigned long liovcnt,
                         const struct iovec *remote_iov,
                         unsigned long riovcnt,
                         unsigned long flags);

/**
 * Gets and stores the runtime of the currently playing song, internally
 * referred to as `maptime` in *val.
 * Returns the number of bytes read.
 */
ssize_t get_maptime(pid_t pid, int32_t *val)
{
	ssize_t nread;
	size_t size = sizeof(int32_t);

	struct iovec local[1];
	struct iovec remote[1];

	local[0].iov_len = size;
	local[0].iov_base = val;

	remote[0].iov_len = size;
	remote[0].iov_base = (void *)MAPTIME_ADDR;

	nread = process_vm_readv(pid, local, 1, remote, 1, 0);
	
	return nread;
}