/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek and Darren F. Provine.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)input.h	8.1 (Berkeley) %G%
 */

void	eat_input __P((void));
int	rwait __P((struct timeval *));
int	tgetchar __P((void));
void	tsleep __P((void));
