/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

void	c_regular __P((int, char *, off_t, off_t, int, char *, off_t, off_t));
void	c_special __P((int, char *, off_t, int, char *, off_t));
void	diffmsg __P((char *, char *, off_t, off_t));
void	eofmsg __P((char *));
void	err __P((const char *fmt, ...));

extern int lflag, sflag;
