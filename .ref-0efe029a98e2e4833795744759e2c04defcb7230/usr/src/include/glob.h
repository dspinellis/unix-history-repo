/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)glob.h	5.2 (Berkeley) %G%
 */

typedef struct {
	int gl_pathc;		/* count of paths matching pattern */
	int gl_offs;		/* reserved at beginning of gl_pathv */
	int gl_flags;		/* copy of flags parameter to glob() */
	int (*gl_errfunc)();	/* copy of errfunc parameter to glob() */
	char **gl_pathv;	/* list of paths matching pattern */
} glob_t;

#define	GLOB_ERR	0x01	/* return on error */
#define	GLOB_MARK	0x02	/* append / to matching directories */
#define	GLOB_NOSORT	0x04	/* don't sort */
#define	GLOB_NOCHECK	0x08	/* return pattern itself if nothing matches */
#define	GLOB_DOOFFS	0x10	/* use gl_offs */
#define	GLOB_APPEND	0x20	/* append to output from previous call */

#ifndef _POSIX_SOURCE
#define	GLOB_QUOTE	0x40	/* quote special chars with \ */
#endif

#define	GLOB_NOSPACE	(-1)	/* malloc call failed */
#define	GLOB_ABEND	(-2)	/* unignored error */

void globfree();
