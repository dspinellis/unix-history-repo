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
 *	@(#)dirent.h	5.4 (Berkeley) %G%
 */

#ifndef _DIRENT_
#define _DIRENT_

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
	u_long	d_fileno;		/* file number of entry */
	u_short	d_reclen;		/* length of this record */
	u_short	d_namlen;		/* length of string in d_name */
	char	d_name[MAXNAMLEN + 1];	/* name must be no longer than this */
};
#define d_ino d_fileno		/* backward compatibility */

/*
 * Definitions for library routines operating on directories.
 */
#define	DIRBLKSIZ	1024

#define	NDIRHASH	8	/* Num of hash lists, must be a power of 2 */
#define	LOCHASH(i)	((i)&(NDIRHASH-1))

/*
 * This structure describes an open directory.
 */
typedef struct _dirdesc {
	int	dd_fd;		/* file descriptor associated with directory */
	long	dd_loc;		/* offset in current buffer */
	long	dd_size;	/* amount of data returned by getdirentries */
	char	*dd_buf;	/* data buffer */
	int	dd_len;		/* size of data buffer */
	long	dd_seek;	/* magic cookie returned by getdirentries */
	long	dd_loccnt;	/* next magic cookie for seekdir */
	struct	ddloc *dd_hash[NDIRHASH];   /* hash list heads for ddlocs */
} DIR;

#define dirfd(dirp)	((dirp)->dd_fd)

/*
 * One of these structures is malloced to describe the current directory
 * position each time telldir is called. It records the current magic 
 * cookie returned by getdirentries and the offset within the buffer
 * associated with that return value.
 */
struct ddloc {
	struct	ddloc *loc_next;/* next structure in list */
	long	loc_index;	/* key associated with structure */
	long	loc_seek;	/* magic cookie returned by getdirentries */
	long	loc_loc;	/* offset of entry in buffer */
};

#ifndef NULL
#define NULL 0
#endif
extern	DIR *opendir();
extern	struct dirent *readdir();
extern	long telldir();
extern	void seekdir();
#define rewinddir(dirp)	seekdir((dirp), (long)0)
extern	void closedir();
#endif /* _DIRENT_ */
