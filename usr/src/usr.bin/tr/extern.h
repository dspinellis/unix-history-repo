/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.2 (Berkeley) %G%
 */

typedef struct {
	enum { STRING1, STRING2 } which;
	enum { EOS, INFINITE, NORMAL, RANGE, SEQUENCE, SET } state;
	int	 cnt;			/* character count */
	int	 lastch;		/* last character */
	int	equiv[2];		/* equivalence set */
	int	*set;			/* set of characters */
	char	*str;			/* user's string */
} STR;

#include <limits.h>
#define	NCHARS	(UCHAR_MAX + 1)		/* Number of possible characters. */
#define	OOBCH	(UCHAR_MAX + 1)		/* Out of band character value. */

void	 err __P((const char *fmt, ...));
int	 next __P((STR *));
