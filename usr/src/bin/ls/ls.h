/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ls.h	5.16 (Berkeley) %G%
 */

#define NO_PRINT	1

extern long blocksize;		/* block size units */

extern int f_accesstime;	/* use time of last access */
extern int f_flags;		/* show flags associated with a file */
extern int f_inode;		/* print inode */
extern int f_longform;		/* long listing format */
extern int f_sectime;		/* print the real time for all files */
extern int f_size;		/* list size in short listing */
extern int f_statustime;	/* use time of last mode change */
extern int f_type;		/* add type character for non-regular files */

typedef struct {
	FTSENT *list;
	int entries;
	int maxlen;
	u_long btotal;
	u_long s_block;
	u_long s_inode;
	u_long s_nlink;
	u_long s_size;
	u_long s_user;
	u_long s_group;
	u_long s_flags;
} DISPLAY;

typedef struct {
	char *user;
	char *group;
	char *flags;
	char data[1];
} NAMES;
