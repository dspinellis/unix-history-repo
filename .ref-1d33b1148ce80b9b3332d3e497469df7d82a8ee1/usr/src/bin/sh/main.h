/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)main.h	8.2 (Berkeley) %G%
 */

extern int rootpid;	/* pid of main shell */
extern int rootshell;	/* true if we aren't a child of the main shell */

void readcmdfile __P((char *));
void cmdloop __P((int));
int dotcmd __P((int, char **));
int exitcmd __P((int, char **));
