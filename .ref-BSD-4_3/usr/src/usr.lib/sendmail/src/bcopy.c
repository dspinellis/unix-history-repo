/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
static char	SccsId[] = "@(#)bcopy.c	5.1 (Berkeley) 6/7/85";
#endif not lint

# include "useful.h"

/*
**  BCOPY -- block copy.
**
**	Parameters:
**		s -- source of bytes.
**		d -- destination of bytes.
**		l -- length of block to move.
**
**	Returns:
**		none.
**
**	Side Effects:
**		copies 's' to 'd' for 'l' bytes.
**
**	Notes:
**		This can be easily written in assembly language on
**		machines like VAXes if performance is needed.
*/

/*VARARGS0*/
bcopy(s, d, l)
	register char *s, *d;
	register int l;
{
	while (l-- > 0)
		*d++ = *s++;
}
/*
**  BZERO -- zero a block of memory
**
**	Parameters:
**		p -- location to clear.
**		l -- number of bytes to clear.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

bzero(p, l)
	register char *p;
	register int l;
{
	while (l-- > 0)
		*p++ = 0;
}
