/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ls.h	5.15 (Berkeley) %G%
 */

/*
 * Specify maximum width of flags output. Determined from flags_from_fid
 * routine in print.c
 */
#define FLAGSWIDTH	10
	
#define NO_PRINT	1

extern int blocksize;		/* block size units */

extern int f_accesstime;	/* use time of last access */
extern int f_group;		/* show group ownership of a file */
extern int f_flags;		/* show flags associated with a file */
extern int f_inode;		/* print inode */
extern int f_longform;		/* long listing format */
extern int f_sectime;		/* print the real time for all files */
extern int f_singlecol;		/* use single column output */
extern int f_size;		/* list size in short listing */
extern int f_statustime;	/* use time of last mode change */
extern int f_total;		/* if precede with "total" line */
extern int f_type;		/* add type character for non-regular files */
