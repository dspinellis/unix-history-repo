/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trap.h	8.1 (Berkeley) %G%
 */

extern int pendingsigs;

#ifdef __STDC__
void clear_traps(void);
int setsignal(int);
void ignoresig(int);
void dotrap(void);
void setinteractive(int);
void exitshell(int);
#else
void clear_traps();
int setsignal();
void ignoresig();
void dotrap();
void setinteractive();
void exitshell();
#endif
