/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)init.h	8.1 (Berkeley) %G%
 */

#ifdef __STDC__
void init(void);
void reset(void);
void initshellproc(void);
#else
void init();
void reset();
void initshellproc();
#endif
