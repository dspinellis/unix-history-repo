/*  $Revision: 1.4 $
**
*/
#include "configdata.h"
#include <sys/types.h>
#if	defined(LOCK_NONE)
#include <sys/stat.h>
#endif	/* defined(LOCK_NONE) */
#if	defined(LOCK_FLOCK)
#include <sys/file.h>
#endif	/* defined(LOCK_FLOCK) */
#if	defined(LOCK_LOCKF)
#if	defined(DO_HAVE_UNISTD)
#include <unistd.h>
#endif	/* defined(DO_HAVE_UNISTD) */
#include <fcntl.h>
#endif	/* defined(LOCK_LOCKF) */
#if	defined(LOCK_FCNTL)
#include <fcntl.h>
#if	!defined(SEEK_SET)
#define SEEK_SET	0
#endif	/* !defined(SEEK_SET) */
#endif	/* defined(LOCK_FCNTL) */


/*
**  Try to lock a file descriptor.
*/
int
LockFile(fd, Block)
    int		fd;
    BOOL	Block;
{
#if	defined(LOCK_NONE)
    struct stat	Sb;

    return fstat(fd, &Sb);
#endif	/* defined(LOCK_NONE) */

#if	defined(LOCK_FLOCK)
    return flock(fd, Block ? LOCK_EX : LOCK_EX | LOCK_NB);
#endif	/* defined(LOCK_FLOCK) */

#if	defined(LOCK_LOCKF)
    return lockf(fd, Block ? F_LOCK : F_TLOCK, 0L);
#endif	/* defined(LOCK_LOCKF) */

#if	defined(LOCK_FCNTL)
    struct flock	fl;

    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    return fcntl(fd, Block ? F_SETLKW : F_SETLK, &fl);
#endif	/* defined(LOCK_FCNTL) */
}
