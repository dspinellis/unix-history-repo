/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trap.h	8.3 (Berkeley) %G%
 */

extern int pendingsigs;

int trapcmd __P((int, char **));
void clear_traps __P((void)); 
long setsignal __P((int)); 
void ignoresig __P((int));
void onsig __P((int));
void dotrap __P((void));
void setinteractive __P((int));
void exitshell __P((int)); 
