/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ls.h	5.7 (Berkeley) %G%
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
extern int f_longform;		/* long listing format */
extern int f_singlecol;		/* use single column output */
extern int f_size;		/* list size in short listing */
extern int f_statustime;	/* use time of last mode change */
extern int f_total;		/* if precede with "total" line */
extern int f_type;		/* add type character for non-regular files */
