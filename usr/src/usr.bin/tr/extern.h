/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

typedef struct {
	enum { EOS, INFINITE, NORMAL, RANGE, SEQUENCE, SET, ULSET } state;
	int	 cnt;			/* character count */
	int	 lastch;		/* last character */
	int	*set;			/* set of characters */
	char	*str;			/* user's string */

#define	T_CLASS	0x01			/* class != lower/upper */
#define	T_SEQ	0x02			/* sequence */
#define	T_UL	0x04			/* lower/upper classes */
	u_int	type;			/* Permissible string conventions. */
} STR;

#include <limits.h>
#define	NCHARS	(UCHAR_MAX + 1)		/* Number of possible characters. */
#define	OOBCH	(UCHAR_MAX + 1)		/* Out of band character value. */

void	 err __P((const char *fmt, ...));
int	 next __P((STR *));
