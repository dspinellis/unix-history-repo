/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)roll.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 *	This routine rolls ndie nside-sided dice.
 */

# define	reg	register

# if !defined(vax) && !defined(tahoe)
# define	MAXRAND	32767L

roll(ndie, nsides)
int	ndie, nsides; {

	reg long	tot;
	reg unsigned	n, r;

	tot = 0;
	n = ndie;
	while (n--)
		tot += rand();
	return (int) ((tot * (long) nsides) / ((long) MAXRAND + 1)) + ndie;
}

# else

roll(ndie, nsides)
reg int	ndie, nsides; {

	reg int		tot, r;
	reg double	num_sides;

	num_sides = nsides;
	tot = 0;
	while (ndie--)
		tot += (r = rand()) * (num_sides / 017777777777) + 1;
	return tot;
}
# endif
