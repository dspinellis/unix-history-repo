/*
	seekdir -- reposition a directory stream

	last edit:	24-May-1987	D A Gwyn

	An unsuccessful seekdir() will in general alter the current
	directory position; beware.

	NOTE:	4.nBSD directory compaction makes seekdir() & telldir()
		practically impossible to do right.  Avoid using them!
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

typedef int	bool;			/* Boolean data type */
#define	false	0
#define	true	1

void
seekdir( dirp, loc )
	register DIR	*dirp;		/* stream from opendir() */
	register off_t	loc;		/* position from telldir() */
	{
	register bool	rewind;		/* "start over when stymied" flag */

	if ( dirp == NULL || dirp->dd_buf == NULL )
		{
		errno = EFAULT;
		return;			/* invalid pointer */
		}

	/* A (struct dirent)'s d_off is an invented quantity on 4.nBSD
	   NFS-supporting systems, so it is not safe to lseek() to it. */

	/* Monotonicity of d_off is heavily exploited in the following. */

	/* This algorithm is tuned for modest directory sizes.  For
	   huge directories, it might be more efficient to read blocks
	   until the first d_off is too large, then back up one block,
	   or even to use binary search on the directory blocks.  I
	   doubt that the extra code for that would be worthwhile. */

	if ( dirp->dd_loc >= dirp->dd_size	/* invalid index */
	  || ((struct dirent *)&dirp->dd_buf[dirp->dd_loc])->d_off > loc
					/* too far along in buffer */
	   )
		dirp->dd_loc = 0;	/* reset to beginning of buffer */
	/* else save time by starting at current dirp->dd_loc */

	for ( rewind = true; ; )
		{
		register struct dirent	*dp;

		/* See whether the matching entry is in the current buffer. */

		if ( (dirp->dd_loc < dirp->dd_size	/* valid index */
		   || readdir( dirp ) != NULL	/* next buffer read */
		   && (dirp->dd_loc = 0, true)	/* beginning of buffer set */
		     )
		  && (dp = (struct dirent *)&dirp->dd_buf[dirp->dd_loc])->d_off
			<= loc		/* match possible in this buffer */
		   )	{
			for ( /* dp initialized above */ ;
			      (char *)dp < &dirp->dd_buf[dirp->dd_size];
			      dp = (struct dirent *)((char *)dp + dp->d_reclen)
			    )
				if ( dp->d_off == loc )
					{	/* found it! */
					dirp->dd_loc =
						(char *)dp - dirp->dd_buf;
					return;
					}

			rewind = false;	/* no point in backing up later */
			dirp->dd_loc = dirp->dd_size;	/* set end of buffer */
			}
		else			/* whole buffer past matching entry */
			if ( !rewind )
				{	/* no point in searching further */
				errno = EINVAL;
				return;	/* no entry at specified loc */
				}
			else	{	/* rewind directory and start over */
				rewind = false;	/* but only once! */

				dirp->dd_loc = dirp->dd_size = 0;

				if ( lseek( dirp->dd_fd, (off_t)0, SEEK_SET )
					!= 0
				   )
					return;	/* errno already set (EBADF) */

				if ( loc == 0 )
					return; /* save time */
				}
		}
	}
#else
int	_seekdir_stub () {};
#endif
