/*
 * Copyright (c) 1982, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dinode.h	7.9 (Berkeley) 6/28/90
 */

/*
 * This structure defines the on-disk format of an inode.
 */

#define	NDADDR	12		/* direct addresses in inode */
#define	NIADDR	3		/* indirect addresses in inode */

struct dinode {
	u_short	di_mode;	/*  0: mode and type of file */
	short	di_nlink;	/*  2: number of links to file */
	uid_t	di_uid;		/*  4: owner's user id */
	gid_t	di_gid;		/*  6: owner's group id */
	u_quad	di_qsize;	/*  8: number of bytes in file */
	time_t	di_atime;	/* 16: time last accessed */
	long	di_atspare;
	time_t	di_mtime;	/* 24: time last modified */
	long	di_mtspare;
	time_t	di_ctime;	/* 32: last time inode changed */
	long	di_ctspare;
	daddr_t	di_db[NDADDR];	/* 40: disk block addresses */
	daddr_t	di_ib[NIADDR];	/* 88: indirect blocks */
	long	di_flags;	/* 100: status, currently unused */
	long	di_blocks;	/* 104: blocks actually held */
	long	di_gen;		/* 108: generation number */
	long	di_spare[4];	/* 112: reserved, currently unused */
};

#if BYTE_ORDER == LITTLE_ENDIAN || defined(tahoe) /* ugh! -- must be fixed */
#define	di_size		di_qsize.val[0]
#else /* BYTE_ORDER == BIG_ENDIAN */
#define	di_size		di_qsize.val[1]
#endif
#define	di_rdev		di_db[0]

/* file modes */
#define	IFMT		0170000		/* type of file */
#define	IFIFO		0010000		/* named pipe (fifo) */
#define	IFCHR		0020000		/* character special */
#define	IFDIR		0040000		/* directory */
#define	IFBLK		0060000		/* block special */
#define	IFREG		0100000		/* regular */
#define	IFLNK		0120000		/* symbolic link */
#define	IFSOCK		0140000		/* socket */

#define	ISUID		04000		/* set user id on execution */
#define	ISGID		02000		/* set group id on execution */
#define	ISVTX		01000		/* save swapped text even after use */
#define	IREAD		0400		/* read, write, execute permissions */
#define	IWRITE		0200
#define	IEXEC		0100
