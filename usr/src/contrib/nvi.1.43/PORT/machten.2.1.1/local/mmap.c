#include <sys/types.h>

#include <stdlib.h>

#include "compat.h"

/*
 * This function emulates mmap() by reading `len' bytes from the file
 * descriptor `fd' and returning a pointer to that memory.  The "mapped"
 * region can later be deallocated with munmap().
 *
 * Note: ONLY reading is supported and only reading of the exact size
 * of the file will work.
 */
char *
mmap_read(addr, len, prot, flags, fd, off)
	char *addr;
	size_t len;
	int prot;
	int flags;
	int fd;
	off_t off;
{
	char *ptr;

	if ((ptr = (char *)malloc(len)) == 0)
		return (-1);
	if (read(fd, ptr, len) < 0) {
		free(ptr);
		return (-1);
	}
	return (ptr);
}

int
munmap_read(addr, len)
	char *addr;
	size_t len;
{
	free(addr);
	return (0);
}
