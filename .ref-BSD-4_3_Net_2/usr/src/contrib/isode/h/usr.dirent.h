/* usr.dirent.h - ISODE's version of DAG's SVR3 directory access routines */

/*
	<dirent.h> -- definitions for SVR3 directory access routines

	last edit:	25-Apr-1987	D A Gwyn

	Prerequisite:	<sys/types.h>
*/

#ifndef	_CONFIG_
#include "config.h"		/* system-specific configuration */
#endif
#ifndef	GETDENTS
#if	defined(SUNOS4)
#define	GETDENTS
#endif
#endif

#ifdef	GETDENTS
#include <dirent.h>
#else
#include	"sys.dirent.h"

#define	DIRBUF		8192		/* buffer size for fs-indep. dirs */
	/* must in general be larger than the filesystem buffer size */

typedef struct
	{
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	*dd_buf;		/* -> directory block */
	}	DIR;			/* stream data from opendir() */

#define	opendir		_opendir
#define	readdir		_readdir
#define	telldir		_telldir
#define	seekdir		_seekdir
#define	rewinddir	_rewinddir
#define	closedir	_closedir

extern DIR		*opendir();
extern struct dirent	*readdir();
extern off_t		telldir();
extern void		seekdir();
extern void		rewinddir();
extern int		closedir();

#ifndef NULL
#define	NULL	0			/* DAG -- added for convenience */
#endif
#endif	/* not GETDENTS */

#define	getcwd		_getcwd

extern char             *getcwd ();


#define	scandir	_scandir
#define	alphasort _alphasort

extern char *direntversion;
