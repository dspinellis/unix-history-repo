/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dirent.h	8.1 (Berkeley) %G%
 */

#ifndef _DIRENT_H_
#define _DIRENT_H_

/*
 * The kernel defines the format of directory entries returned by 
 * the getdirentries(2) system call.
 */
#include <sys/dirent.h>

#ifdef _POSIX_SOURCE
typedef void *	DIR;
#else

#define	d_ino		d_fileno	/* backward compatibility */

/* definitions for library routines operating on directories. */
#define	DIRBLKSIZ	1024

/* structure describing an open directory. */
typedef struct _dirdesc {
	int	dd_fd;		/* file descriptor associated with directory */
	long	dd_loc;		/* offset in current buffer */
	long	dd_size;	/* amount of data returned by getdirentries */
	char	*dd_buf;	/* data buffer */
	int	dd_len;		/* size of data buffer */
	long	dd_seek;	/* magic cookie returned by getdirentries */
} DIR;

#define	dirfd(dirp)	((dirp)->dd_fd)

#ifndef NULL
#define	NULL	0
#endif

#endif /* _POSIX_SOURCE */

#ifndef KERNEL

#include <sys/cdefs.h>

__BEGIN_DECLS
DIR *opendir __P((const char *));
struct dirent *readdir __P((DIR *));
void rewinddir __P((DIR *));
int closedir __P((DIR *));
#ifndef _POSIX_SOURCE
long telldir __P((const DIR *));
void seekdir __P((DIR *, long));
int scandir __P((const char *, struct dirent ***,
    int (*)(struct dirent *), int (*)(const void *, const void *)));
int alphasort __P((const void *, const void *));
int getdirentries __P((int, char *, int, long *));
#endif /* not POSIX */
__END_DECLS

#endif /* !KERNEL */

#endif /* !_DIRENT_H_ */
