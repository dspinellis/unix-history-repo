/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dirent.h	5.1 (Berkeley) %G%
 */

/*
 * A directory entry has a struct direct at the front of it,
 * containing its inode number, the length of the entry, and the
 * length of the name contained in the entry.  These are followed
 * by the name padded to a 4 byte boundary with null bytes.  All
 * names are guaranteed null terminated.  The maximum length of a
 * name in a directory is MAXNAMLEN.
 */
#define	MAXNAMLEN	255

struct	dirent {
	u_long	d_ino;			/* inode number of entry */
	u_short	d_reclen;		/* length of this record */
	u_short	d_namlen;		/* length of string in d_name */
	char	d_name[MAXNAMLEN + 1];	/* name must be no longer than this */
};

/*
 * Definitions for library routines operating on directories.
 */
#define DIRBLKSIZ	512

typedef struct _dirdesc {
	int	dd_fd;
	long	dd_loc;
	long	dd_size;
	char	dd_buf[DIRBLKSIZ];
} DIR;

#define dirfd(dirp)	((dirp)->dd_fd)

#ifndef NULL
#define NULL 0
#endif
extern	DIR *opendir();
extern	struct dirent *readdir();
extern	long telldir();
extern	void seekdir();
#define rewinddir(dirp)	seekdir((dirp), (long)0)
extern	void closedir();
