#include <sys/types.h>
/*
 * Include <sys/file.h>, not <fcntl.h>, the flock(2)
 * #defines were found there on historical systems.
 */
#include <sys/file.h>

#include <errno.h>
#include <unistd.h>

#include <compat.h>

/*
 * Use lockf(2) locking to fake flock(2) locking.
 */
int
flock(fd, operation)
	int fd, operation;
{
	if (!lockf(fd,
	    operation & LOCK_UN ? F_ULOCK :
	    operation & LOCK_NB ? F_TLOCK : F_LOCK, 0))
		return (0);
	if (errno == EACCES || errno == EAGAIN)
		errno = EWOULDBLOCK;
	return (-1);
}
