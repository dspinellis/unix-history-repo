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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)hexdump.h	5.1 (Berkeley) %G%
 */

typedef struct _pr {
	struct _pr *nextpr;		/* next print unit */
#define	F_ADDRESS	0x001		/* print offset */
#define	F_C		0x002		/* %_c */
#define	F_CHAR		0x004		/* %c */
#define	F_DBL		0x008		/* %[EefGf] */
#define	F_INT		0x010		/* %[di] */
#define	F_P		0x020		/* %_p */
#define	F_STR		0x040		/* %s */
#define	F_U		0x080		/* %_u */
#define	F_UINT		0x100		/* %[ouXx] */
#define	F_TEXT		0x200		/* no conversions */
	u_int flags;			/* flag values */
	int bcnt;			/* byte count */
	char *cchar;			/* conversion character */
	char *fmt;			/* printf format */
	char *nospace;			/* no whitespace version */
} PR;

typedef struct _fu {
	struct _fu *nextfu;		/* next format unit */
	struct _pr *nextpr;		/* next print unit */
#define	F_IGNORE	0x01		/* %_A */
#define	F_SETREP	0x02		/* rep count set, not default */
	u_int flags;			/* flag values */
	int reps;			/* repetition count */
	int bcnt;			/* byte count */
	char *fmt;			/* format string */
} FU;

typedef struct _fs {			/* format strings */
	struct _fs *nextfs;		/* linked list of format strings */
	struct _fu *nextfu;		/* linked list of format units */
	int bcnt;
} FS;

extern FS *fshead;			/* head of format strings list */
extern int blocksize;			/* data block size */
enum _vflag { ALL, DUP, WAIT };		/* -v values */
char *emalloc();
