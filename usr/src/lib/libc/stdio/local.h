/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)local.h	5.1 (Berkeley) %G%
 */

/*
 * Information local to this implementation of stdio,
 * in particular, macros and private variables.
 */

#if __STDC__ || c_plusplus
extern int	__sflush(FILE *);
extern FILE	*__sfp(void);
extern int	__srefill(FILE *);
extern int	__sread(void *, char *, int);
extern int	__swrite(void *, char const *, int);
extern fpos_t	__sseek(void *, fpos_t, int);
extern int	__sclose(void *);
extern void	__sinit(void);
extern void	_cleanup(void);
extern void	(*__cleanup)(void);
extern void	__smakebuf(FILE *);
extern int	_fwalk(int (*)(FILE *));
#else /* __STDC__ || c_plusplus */
extern int	__sflush();
extern FILE	*__sfp();
extern int	__srefill();
extern int	__sread();
extern int	__swrite();
extern fpos_t	__sseek();
extern int	__sclose();
extern void	__sinit();
extern void	_cleanup();
extern void	(*__cleanup)();
extern void	__smakebuf();
extern int	__sfwalk();
#endif /* __STDC__ || c_plusplus */

extern int __sdidinit;

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
 * test for an fgetline() buffer.
 */
#define	HASLB(fp) ((fp)->_lb._base != NULL)
#define	FREELB(fp) { \
	free((char *)(fp)->_lb._base); \
	(fp)->_lb._base = NULL; \
}
