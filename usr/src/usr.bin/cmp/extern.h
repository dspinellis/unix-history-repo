/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.3 (Berkeley) %G%
 */

#define OK_EXIT		0
#define DIFF_EXIT	1
#define ERR_EXIT	2	/* error exit code */

void	c_regular __P((int, char *, off_t, off_t, int, char *, off_t, off_t));
void	c_special __P((int, char *, off_t, int, char *, off_t));
void	diffmsg __P((char *, char *, off_t, off_t));
void	eofmsg __P((char *));

extern int lflag, sflag;
