/*
	opendir -- open a directory stream

	last edit:	16-Jun-1987	D A Gwyn
*/

#include	<sys/errno.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	"usr.dirent.h"

#ifndef	GETDENTS
#ifdef BSD_SYSV
#define open	_open			/* avoid emulation overhead */
#endif

typedef char	*pointer;		/* (void *) if you have it */

extern void	free();
extern pointer	malloc();
extern int	open(), close(), fstat();

extern int	errno;

#ifndef NULL
#define	NULL	0
#endif

#ifndef O_RDONLY
#define	O_RDONLY	0
#endif

#ifndef S_ISDIR				/* macro to test for directory file */
#define	S_ISDIR( mode )		(((mode) & S_IFMT) == S_IFDIR)
#endif

DIR *
opendir( dirname )
	char		*dirname;	/* name of directory */
	{
	register DIR	*dirp;		/* -> malloc'ed storage */
	register int	fd;		/* file descriptor for read */
	struct stat	sbuf;		/* result of fstat() */

	if ( (fd = open( dirname, O_RDONLY )) < 0 )
		return NULL;		/* errno set by open() */

	if ( fstat( fd, &sbuf ) != 0 || !S_ISDIR( sbuf.st_mode ) )
		{
		(void)close( fd );
		errno = ENOTDIR;
		return NULL;		/* not a directory */
		}

	if ( (dirp = (DIR *)malloc( sizeof(DIR) )) == NULL
	  || (dirp->dd_buf = (char *)malloc( (unsigned)DIRBUF )) == NULL
	   )	{
		register int	serrno = errno;
					/* errno set to ENOMEM by sbrk() */

		if ( dirp != NULL )
			free( (pointer)dirp );

		(void)close( fd );
		errno = serrno;
		return NULL;		/* not enough memory */
		}

	dirp->dd_fd = fd;
	dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

	return dirp;
	}
#else
int	_opendir_stub () {};
#endif
