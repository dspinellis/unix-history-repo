/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stoc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "gomoku.h"
#include <ctype.h>

char	*letters	= "<ABCDEFGHJKLMNOPQRST>";

struct mvstr {
	int	m_code;
	char	*m_text;
};
static	struct	mvstr	mv[] = {
	RESIGN,		"resign",
	RESIGN,		"quit",
	SAVE,		"save",
	-1,		0
};

/*
 * Turn the spot number form of a move into the character form.
 */
char *
stoc(s)
	int s;
{
	static char buf[32];
	register int i;

	for (i = 0; mv[i].m_code >= 0; i++)
		if (s == mv[i].m_code)
			return(mv[i].m_text);
	sprintf(buf, "%c%d", letters[s % BSZ1], s / BSZ1);
	return(buf);
}

/*
 * Turn the character form of a move into the spot number form.
 */
ctos(mp)
	char *mp;
{
	register int i;

	for (i = 0; mv[i].m_code >= 0; i++)
		if (strcmp(mp, mv[i].m_text) == 0)
			return(mv[i].m_code);
	if (!isalpha(mp[0]))
		return(ILLEGAL);
	i = atoi(&mp[1]);
	if (i < 1 || i > 19)
		return(ILLEGAL);
	return(PT(lton(mp[0]), i));
}

/*
 * Turn a letter into a number.
 */
lton(c)
	int c;
{
	register int i;

	if (islower(c))
		c = toupper(c);
	for (i = 1; i <= BSZ && letters[i] != c; i++)
		;
	return(i);
}
