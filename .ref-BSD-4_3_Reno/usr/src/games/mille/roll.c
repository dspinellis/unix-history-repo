/*
 * Copyright (c) 1982 Regents of the University of California.
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
static char sccsid[] = "@(#)roll.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"mille.h"

/*
 *	This routine rolls ndie nside-sided dice.
 *
 * @(#)roll.c	1.1 (Berkeley) 4/1/82
 *
 */

roll(ndie, nsides)
reg int	ndie, nsides; {

	reg int			tot;
	extern unsigned int	random();

	tot = 0;
	while (ndie--)
		tot += random() % nsides + 1;
	return tot;
}
