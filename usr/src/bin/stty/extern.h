/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.2 (Berkeley) %G%
 */

__BEGIN_DECLS
int	c_cchars __P((const void *, const void *));
int	c_modes __P((const void *, const void *));
void	checkredirect __P((void));
void	err __P((const char *, ...));
struct key *
	ksearch __P((char *));
void	gprint __P((struct termios *, struct winsize *, int));
void	gread __P((struct termios *, char *));
void	optlist __P((void));
void	print __P((struct termios *, struct winsize *, int, enum FMT));
void	warn __P((const char *, ...));
__END_DECLS

extern struct cchar cchars1[], cchars2[];
extern struct modes cmodes[], imodes[], lmodes[], omodes[];
