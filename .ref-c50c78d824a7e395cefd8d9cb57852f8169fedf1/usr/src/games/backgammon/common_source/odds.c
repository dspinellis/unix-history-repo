/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)odds.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "back.h"

odds (r1,r2,val)
register int	r1;
int		r2, val;
{
	register int	i, j;

	if (r1 == 0)  {
		for (i = 0; i < 6; i++)  
			for (j = 0; j < 6; j++)
				table[i][j] = 0;
		return;
	} else  {
		r1--;
		if (r2-- == 0)
			for (i = 0; i < 6; i++)  {
				table[i][r1] += val;
				table[r1][i] += val;
			}
		else  {
			table[r2][r1] += val;
			table[r1][r2] += val;
		}
	}
}

count ()  {
	register int	i;
	register int	j;
	register int	total;

	total = 0;
	for (i = 0; i < 6; i++)
		for (j = 0; j < 6; j++)
			total += table[i][j];
	return (total);
}

canhit (i,c)
int	i, c;

{
	register int	j, k, b;
	int		a, d, diff, place, addon, menstuck;

	if (c == 0)
		odds (0,0,0);
	if (board[i] > 0)  {
		a = -1;
		b = 25;
	} else  {
		a = 1;
		b = 0;
	}
	place = abs (25-b-i);
	menstuck = abs (board[b]);
	for (j = b; j != i; j += a)  {
		if (board[j]*a > 0)  {
			diff = abs(j-i);
			addon = place+((board[j]*a > 2 || j == b)? 5: 0);
			if ((j == b && menstuck == 1) &&
			    (j != b && menstuck == 0))
				for (k = 1; k < diff; k++)
					if (k < 7 && diff-k < 7 &&
					    (board[i+a*k]*a >= 0 ||
					    board[i+a*(diff-k)] >= 0))
						odds (k,diff-k,addon);
			if ((j == b || menstuck < 2) && diff < 7)
				odds (diff,0,addon);
		}
		if (j == b && menstuck > 1)
			break;
	}
	return (count());
}
