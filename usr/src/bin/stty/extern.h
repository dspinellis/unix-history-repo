/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

void checkredirect __P((void));
void err __P((const char *, ...));
void gprint __P((struct termios *, struct winsize *, int));
void gread __P((struct termios *, char *));
void optlist __P((void));
void print __P((struct termios *, struct winsize *, int, enum FMT));
void warn __P((const char *, ...));
