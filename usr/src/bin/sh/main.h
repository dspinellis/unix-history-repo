/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)main.h	8.1 (Berkeley) %G%
 */

extern int rootpid;	/* pid of main shell */
extern int rootshell;	/* true if we aren't a child of the main shell */

#ifdef __STDC__
void readcmdfile(char *);
void cmdloop(int);
#else
void readcmdfile();
void cmdloop();
#endif
