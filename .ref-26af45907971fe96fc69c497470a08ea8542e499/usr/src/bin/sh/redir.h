/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)redir.h	8.1 (Berkeley) %G%
 */

/* flags passed to redirect */
#define REDIR_PUSH 01		/* save previous values of file descriptors */
#define REDIR_BACKQ 02		/* save the command output in memory */

#ifdef __STDC__
union node;
void redirect(union node *, int);
void popredir(void);
void clearredir(void);
int copyfd(int, int);
int fd0_redirected_p(void);
#else
void redirect();
void popredir();
void clearredir();
int copyfd();
int fd0_redirected_p();
#endif
