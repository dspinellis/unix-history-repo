#include <sys/types.h>
/*
 * Include <sys/file.h>, not <fcntl.h>, the flock(2)
 * #defines were found there on historical systems.
 */
#include <sys/file.h>

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <compat.h>

/*
 * Use fcntl(2) locking to fake flock(2) locking.
 *
 * DO NOT USE fcntl(2) UNLESS YOU HAVE TO, THERE ARE SOME SYSTEMS
 * (I.E. ULTRIX) WHERE LOCKS AREN'T RELEASED WHEN PROCESSES DIE.
 */
int
flock(fd, operation)
	int fd, operation;
{
	struct flock arg;

	switch (operation & ~LOCK_NB) {
	case LOCK_EX:
		arg.l_type = F_WRLCK;
		break;
	case LOCK_SH:
		arg.l_type = F_RDLCK;
		break;
	case LOCK_UN:
		arg.l_type = F_UNLCK;
		break;
	default:
		abort();
	}

	arg.l_start = arg.l_len = 0;
	arg.l_pid = 0;
	arg.l_whence = 0;		/* SEEK_SET */
	
	if (!fcntl(fd, operation & LOCK_NB ? F_SETLK : F_SETLKW, &arg))
		return (0);
	if (errno == EACCES || errno == EAGAIN)
		errno = EWOULDBLOCK;
	return (-1);
}
