/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)local.h	8.3 (Berkeley) %G%
 */

/*
 * Information local to this implementation of stdio,
 * in particular, macros and private variables.
 */

extern int	__sflush __P((FILE *));
extern FILE	*__sfp __P((void));
extern int	__srefill __P((FILE *));
extern int	__sread __P((void *, char *, int));
extern int	__swrite __P((void *, char const *, int));
extern fpos_t	__sseek __P((void *, fpos_t, int));
extern int	__sclose __P((void *));
extern void	__sinit __P((void));
extern void	_cleanup __P((void));
extern void	(*__cleanup) __P((void));
extern void	__smakebuf __P((FILE *));
extern int	__swhatbuf __P((FILE *, size_t *, int *));
extern int	_fwalk __P((int (*)(FILE *)));
extern int	__swsetup __P((FILE *));
extern int	__sflags __P((const char *, int *));

extern int	__sdidinit;

/*
 * Return true iff the given FILE cannot be written now.
 */
#define	cantwrite(fp) \
	((((fp)->_flags & __SWR) == 0 || (fp)->_bf._base == NULL) && \
	 __swsetup(fp))

/*
 * Test whether the given stdio file has an active ungetc buffer;
 * release such a buffer, without restoring ordinary unread data.
 */
#define	HASUB(fp) ((fp)->_ub._base != NULL)
#define	FREEUB(fp) { \
	if ((fp)->_ub._base != (fp)->_ubuf) \
		free((char *)(fp)->_ub._base); \
	(fp)->_ub._base = NULL; \
}

/*
 * test for an fgetln() buffer.
 */
#define	HASLB(fp) ((fp)->_lb._base != NULL)
#define	FREELB(fp) { \
	free((char *)(fp)->_lb._base); \
	(fp)->_lb._base = NULL; \
}
