/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)look.h	5.1 (Berkeley) %G%
 */

#ifdef __STDC__
#define proto(s) s
#else
#define proto(s) ()
#endif

int hash proto((char *name));
ndptr lookup proto((char *name));
ndptr addent proto((char *name));
void remhash proto((char *name, int all));

#undef proto
