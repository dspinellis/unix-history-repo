/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

int	c_cchars __P((const void *, const void *));
int	c_modes __P((const void *, const void *));
int	csearch __P((char ***, struct info *));
void	checkredirect __P((void));
void	gprint __P((struct termios *, struct winsize *, int));
void	gread __P((struct termios *, char *));
int	ksearch __P((char ***, struct info *));
int	msearch __P((char ***, struct info *));
void	optlist __P((void));
void	print __P((struct termios *, struct winsize *, int, enum FMT));
void	usage __P((void));

extern struct cchar cchars1[], cchars2[];
