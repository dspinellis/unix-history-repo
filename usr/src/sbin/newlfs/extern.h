/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

u_long	cksum __P((void *, size_t));
u_short	dkcksum __P((struct disklabel *));
void	fatal __P((const char *fmt, ...));
u_int	log2 __P((u_int));
int	make_lfs
	    __P((int, struct disklabel *, struct partition *, int, int, int));
int	mkfs __P((struct partition *, char *, int, int));

extern char	*progname;
extern char	*special;
