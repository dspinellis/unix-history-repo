/*
	telldir -- report directory stream position

	last edit:	25-Apr-1987	D A Gwyn

	NOTE:	4.nBSD directory compaction makes seekdir() & telldir()
		practically impossible to do right.  Avoid using them!
*/

#include	<sys/errno.h>
#include	<sys/types.h>
#include	"usr.dirent.h"

#ifndef	GETDENTS
extern off_t	lseek();

extern int	errno;

#ifndef SEEK_CUR
#define	SEEK_CUR	1
#endif

off_t
telldir( dirp )				/* return offset of next entry */
	DIR	*dirp;			/* stream from opendir() */
	{
	if ( dirp == NULL || dirp->dd_buf == NULL )
		{
		errno = EFAULT;
		return -1;		/* invalid pointer */
		}

	if ( dirp->dd_loc < dirp->dd_size )	/* valid index */
		return ((struct dirent *)&dirp->dd_buf[dirp->dd_loc])->d_off;
	else				/* beginning of next directory block */
		return lseek( dirp->dd_fd, (off_t)0, SEEK_CUR );
	}
#else
int	_telldir_stub () {};
#endif
