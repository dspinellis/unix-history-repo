/*
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dir.h	7.12 (Berkeley) %G%
 */

#ifndef _DIR_H_
#define	_DIR_H_

/*
 * A directory consists of some number of blocks of DIRBLKSIZ
 * bytes, where DIRBLKSIZ is chosen such that it can be transferred
 * to disk in a single atomic operation (e.g. 512 bytes on most machines).
 *
 * Each DIRBLKSIZ byte block contains some number of directory entry
 * structures, which are of variable length.  Each directory entry has
 * a struct direct at the front of it, containing its inode number,
 * the length of the entry, and the length of the name contained in
 * the entry.  These are followed by the name padded to a 4 byte boundary
 * with null bytes.  All names are guaranteed null terminated.
 * The maximum length of a name in a directory is MAXNAMLEN.
 *
 * The macro DIRSIZ(fmt, dp) gives the amount of space required to represent
 * a directory entry.  Free space in a directory is represented by
 * entries which have dp->d_reclen > DIRSIZ(fmt, dp).  All DIRBLKSIZ bytes
 * in a directory block are claimed by the directory entries.  This
 * usually results in the last entry in a directory having a large
 * dp->d_reclen.  When entries are deleted from a directory, the
 * space is returned to the previous entry in the same directory
 * block by increasing its dp->d_reclen.  If the first entry of
 * a directory block is free, then its dp->d_ino is set to 0.
 * Entries other than the first in a directory do not normally have
 * dp->d_ino set to 0.
 */
#define DIRBLKSIZ	DEV_BSIZE
#define	MAXNAMLEN	255

struct	direct {
	u_long	d_ino;			/* inode number of entry */
	u_short	d_reclen;		/* length of this record */
	u_char	d_type; 		/* file type, see below */
	u_char	d_namlen;		/* length of string in d_name */
	char	d_name[MAXNAMLEN + 1];	/* name with length <= MAXNAMLEN */
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

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#if (BYTE_ORDER == LITTLE_ENDIAN)
#define DIRSIZ(oldfmt, dp) \
    ((oldfmt) ? \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_type+1 + 3) &~ 3)) : \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3)))
#else
#define DIRSIZ(oldfmt, dp) \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))
#endif
#define OLDDIRFMT	1
#define NEWDIRFMT	0

/*
 * Template for manipulating directories.
 * Should use struct direct's, but the name field
 * is MAXNAMLEN - 1, and this just won't do.
 */
struct dirtemplate {
	u_long	dot_ino;
	short	dot_reclen;
	u_char	dot_type;
	u_char	dot_namlen;
	char	dot_name[4];		/* must be multiple of 4 */
	u_long	dotdot_ino;
	short	dotdot_reclen;
	u_char	dotdot_type;
	u_char	dotdot_namlen;
	char	dotdot_name[4];		/* ditto */
};

/*
 * This is the old format of directories, sanz type element.
 */
struct odirtemplate {
	u_long	dot_ino;
	short	dot_reclen;
	u_short	dot_namlen;
	char	dot_name[4];		/* must be multiple of 4 */
	u_long	dotdot_ino;
	short	dotdot_reclen;
	u_short	dotdot_namlen;
	char	dotdot_name[4];		/* ditto */
};
#endif /* !_DIR_H_ */
