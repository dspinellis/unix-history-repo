/*
	rewinddir -- rewind a directory stream

	last edit:	25-Apr-1987	D A Gwyn

	This is not simply a call to seekdir(), because seekdir()
	will use the current buffer whenever possible and we need
	rewinddir() to forget about buffered data.
*/

#include	<sys/errno.h>
#include	<sys/types.h>
#include	"usr.dirent.h"

#ifndef	GETDENTS
extern off_t	lseek();

extern int	errno;

#ifndef NULL
#define	NULL	0
#endif

#ifndef SEEK_SET
#define	SEEK_SET	0
#endif

void
rewinddir( dirp )
	register DIR		*dirp;	/* stream from opendir() */
	{
	if ( dirp == NULL || dirp->dd_buf == NULL )
		{
		errno = EFAULT;
		return;			/* invalid pointer */
		}

	dirp->dd_loc = dirp->dd_size = 0;	/* invalidate buffer */
	(void)lseek( dirp->dd_fd, (off_t)0, SEEK_SET );	/* may set errno */
	}
#else
int	_rewinddir_stub () {};
#endif
