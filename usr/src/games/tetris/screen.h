/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek and Darren F. Provine.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)screen.h	5.2 (Berkeley) %G%
 */

/*
 * Capabilities from TERMCAP (used in the score code).
 */
char *SEstr;			/* end standout mode */
char *SOstr;			/* begin standout mode */

/*
 * putpad() is for padded strings with count=1.
 */
#define	putpad(s)	tputs(s, 1, put)

int	put __P((int));		/* just calls putchar; for tputs */
void	scr_clear __P((void));
void	scr_end __P((void));
void	scr_init __P((void));
void	scr_msg __P((char *, int));
void	scr_set __P((void));
void	scr_update __P((void));
