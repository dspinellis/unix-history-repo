/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)s_cat.c	5.1	%G%
 */

s_cat(lp, rpp, rnp, np, ll)
char *lp, *rpp[];
long int rnp[], *np, ll;
{
int i, n, nc;
char *rp;

n = *np;
for(i = 0 ; i < n ; ++i)
	{
	nc = ll;
	if(rnp[i] < nc)
		nc = rnp[i];
	ll -= nc;
	rp = rpp[i];
	while(--nc >= 0)
		*lp++ = *rp++;
	}
while(--ll >= 0)
	*lp++ = ' ';
}
