/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)utility.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
**  ASSORTED UTILITY ROUTINES
*/

/*
**  BLOCK MOVE
**
**	Moves a block of storage of length `l' bytes from the data
**	area pointed to by `a' to the area pointed to by `b'.
**	Returns the address of the byte following the `b' field.
**	Overflow of `b' is not tested.
*/

char *bmove(a, b, l)
char	*a, *b;
int	l;
{
	register int		n;
	register char		*p, *q;

	p = a;
	q = b;
	n = l;
	while (n--)
		*q++ = *p++;
	return (q);
}


/*
**  STRING EQUALITY TEST
**	null-terminated strings `a' and `b' are tested for
**	absolute equality.
**	returns one if equal, zero otherwise.
*/

sequal(a, b)
char	*a, *b;
{
	register char		*p, *q;

	p = a;
	q = b;
	while (*p || *q)
		if (*p++ != *q++)
			return(0);
	return(1);
}


/*
**  STRING CONCATENATE
**
**	The strings `s1' and `s2' are concatenated and stored into
**	`s3'.  It is ok for `s1' to equal `s3', but terrible things
**	will happen if `s2' equals `s3'.  The return value is is a
**	pointer to the end of `s3' field.
*/

char *concat(s1, s2, s3)
char	*s1, *s2, *s3;
{
	register char		*p;
	register char		*q;

	p = s3;
	q = s1;
	while (*q)
		*p++ = *q++;
	q = s2;
	while (*q)
		*p++ = *q++;
	*p = 0;
	return (p);
}


/*
**  FIND STRING LENGTH
**
**	The length of string `s' (excluding the null byte which
**		terminates the string) is returned.
*/

length(s)
char	*s;
{
	register int	l;
	register char	*p;

	l = 0;
	p = s;
	while (*p++)
		l++;
	return(l);
}


/*
**  SYSTEM ERROR
*/

syserr(p0, p1, p2, p3, p4, p5)
{
	extern int	errno;

	printf("\n\07TREK SYSERR: ");
	printf(p0, p1, p2, p3, p4, p5);
	printf("\n");
	if (errno)
		printf("\tsystem error %d\n", errno);
	exit(-1);
}
