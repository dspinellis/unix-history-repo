/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)redir.h	8.2 (Berkeley) %G%
 */

/* flags passed to redirect */
#define REDIR_PUSH 01		/* save previous values of file descriptors */
#define REDIR_BACKQ 02		/* save the command output in memory */

union node;
void redirect __P((union node *, int));
void popredir __P((void));
int fd0_redirected_p __P((void));
void clearredir __P((void)); 
int copyfd __P((int, int));

