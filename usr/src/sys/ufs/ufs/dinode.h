/*
 * Copyright (c) 1982, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dinode.h	7.19 (Berkeley) %G%
 */

/*
 * The root inode is the root of the file system.  Inode 0 can't be used for
 * normal purposes and historically bad blocks were linked to inode 1, thus
 * the root inode is 2.  (Inode 1 is no longer used for this purpose, however
 * numerous dump tapes make this assumption, so we are stuck with it).
 */
#define	ROOTINO	((ino_t)2)

/*
 * A dinode contains all the meta-data associated with a UFS file.
 * This structure defines the on-disk format of a dinode.
 */

#define	NDADDR	12		/* direct addresses in inode */
#define	NIADDR	3		/* indirect addresses in inode */

struct dinode {
	u_short		di_mode;	/*   0: mode and type of file */
	short		di_nlink;	/*   2: number of links to file */
	union {
		u_short	oldids[2];	/*   4: ffs: old user and group ids */
		ino_t	inumber;	/*   4: lfs: inode number */
	} di_u;
	u_quad_t	di_size;	/*   8: number of bytes in file */
	struct timespec	di_atime;	/*  16: time last accessed */
	struct timespec	di_mtime;	/*  24: time last modified */
	struct timespec	di_ctime;	/*  32: last time inode changed */
	daddr_t		di_db[NDADDR];	/*  40: disk block addresses */
	daddr_t		di_ib[NIADDR];	/*  88: indirect blocks */
	long		di_flags;	/* 100: status, currently unused */
	long		di_blocks;	/* 104: blocks actually held */
	long		di_gen;		/* 108: generation number */
	u_long		di_uid;		/* 112: owner's user id */
	u_long		di_gid;		/* 116: owner's group id */
	long		di_spare[2];	/* 120: reserved, currently unused */
};

/*
 * The di_db fields may be overlaid with other information for
 * file types that do not have associated disk storage. Block
 * and character devices overlay the first data block with their
 * dev_t value. Short symbolic links place their path in the
 * di_db area.
 */
#define	di_ouid		di_u.oldids[0]
#define	di_ogid		di_u.oldids[1]
#define	di_inumber	di_u.inumber
#define	di_rdev		di_db[0]
#define di_shortlink	di_db
#define	MAXSYMLINKLEN	((NDADDR + NIADDR) * sizeof(daddr_t))

/* file modes */
#define	IFMT		0170000		/* mask of file type */
#define	IFIFO		0010000		/* named pipe (fifo) */
#define	IFCHR		0020000		/* character special device */
#define	IFDIR		0040000		/* directory */
#define	IFBLK		0060000		/* block special device */
#define	IFREG		0100000		/* regular file */
#define	IFLNK		0120000		/* symbolic link */
#define	IFSOCK		0140000		/* UNIX domain socket */

#define	ISUID		04000		/* set user identifier when exec'ing */
#define	ISGID		02000		/* set group identifier when exec'ing */
#define	ISVTX		01000		/* save execution information on exit */
#define	IREAD		0400		/* read permission */
#define	IWRITE		0200		/* write permission */
#define	IEXEC		0100		/* execute permission */
