/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dirent.h	8.1 (Berkeley) %G%
 */

/*
 * The dirent structure defines the format of directory entries returned by 
 * the getdirentries(2) system call.
 *
 * A directory entry has a struct dirent at the front of it, containing its
 * inode number, the length of the entry, and the length of the name
 * contained in the entry.  These are followed by the name padded to a 4
 * byte boundary with null bytes.  All names are guaranteed null terminated.
 * The maximum length of a name in a directory is MAXNAMLEN.
 */

struct dirent {
	unsigned long	d_fileno;	/* file number of entry */
	unsigned short	d_reclen;	/* length of this record */
	unsigned char	d_type; 	/* file type, see below */
	unsigned char	d_namlen;	/* length of string in d_name */
#ifdef _POSIX_SOURCE
	char	d_name[255 + 1];	/* name must be no longer than this */
#else
#define	MAXNAMLEN	255
	char	d_name[MAXNAMLEN + 1];	/* name must be no longer than this */
#endif
};

/*
 * File types
 */
#define	DT_UNKNOWN	 0
#define	DT_FIFO		 1
#define	DT_CHR		 2
#define	DT_DIR		 4
#define	DT_BLK		 6
#define	DT_REG		 8
#define	DT_LNK		10
#define	DT_SOCK		12

/*
 * Convert between stat structure types and directory types.
 */
#define	IFTODT(mode)	(((mode) & 0170000) >> 12)
#define	DTTOIF(dirtype)	((dirtype) << 12)
