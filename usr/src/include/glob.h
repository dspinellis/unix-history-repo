/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guido van Rossum.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)glob.h	5.5 (Berkeley) %G%
 */

typedef struct {
	int gl_pathc;		/* count of total paths so far */
	int gl_matchc;		/* count of paths matching pattern */
	int gl_offs;		/* reserved at beginning of gl_pathv */
	int gl_flags;		/* copy of flags parameter to glob() */
	int (*gl_errfunc)();	/* copy of errfunc parameter to glob() */
	char **gl_pathv;	/* list of paths matching pattern */
} glob_t;

#define	GLOB_APPEND	0x01	/* append to output from previous call */
#define	GLOB_DOOFFS	0x02	/* use gl_offs */
#define	GLOB_ERR	0x04	/* return on error */
#ifndef _POSIX_SOURCE
#define	GLOB_MAGCHAR	0x08	/* pattern had globbing characters */
#endif
#define	GLOB_MARK	0x10	/* append / to matching directories */
#define	GLOB_NOCHECK	0x20	/* return pattern itself if nothing matches */
#define	GLOB_NOSORT	0x40	/* don't sort */
#ifndef _POSIX_SOURCE
#define	GLOB_QUOTE	0x80	/* quote special chars with \ */
#endif

#define	GLOB_NOSPACE	(-1)	/* malloc call failed */
#define	GLOB_ABEND	(-2)	/* unignored error */

#include <sys/cdefs.h>

__BEGIN_DECLS
int glob __P((const char *, int, int (*)(char *, int), glob_t *));
void globfree __P((glob_t *));
__END_DECLS
