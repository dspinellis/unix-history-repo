/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ls.h	5.10 (Berkeley) 4/8/90
 */

typedef struct _lsstruct {
	char *name;			/* file name */
	int len;			/* file name length */
	struct stat lstat;		/* lstat(2) for file */
} LS;

/*
 * overload -- we probably have to save blocks and/or maxlen with the lstat
 * array, so tabdir() stuffs it into unused fields in the first stat structure.
 * If there's ever a type larger than u_long, fix this.  Any calls to qsort
 * must save and restore the values.
 */
#define	st_btotal	st_flags
#define	st_maxlen	st_gen

extern int errno;

extern int f_accesstime;	/* use time of last access */
extern int f_group;		/* show group ownership of a file */
extern int f_inode;		/* print inode */
extern int f_kblocks;		/* print size in kilobytes */
extern int f_longform;		/* long listing format */
extern int f_singlecol;		/* use single column output */
extern int f_size;		/* list size in short listing */
extern int f_statustime;	/* use time of last mode change */
extern int f_total;		/* if precede with "total" line */
extern int f_type;		/* add type character for non-regular files */
