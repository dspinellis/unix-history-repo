/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)unctrl.h	5.4 (Berkeley) %G%
 */

/*
 * unctrl.h
 */

extern char	*_unctrl[];

# define	unctrl(ch)	(_unctrl[ch & 0177])
