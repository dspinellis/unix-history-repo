/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)i_indx.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

long int i_indx(a, b, la, lb)
char *a, *b;
long int la, lb;
{
long int i, n;
char *s, *t, *bend;

n = la - lb + 1;
bend = b + lb;

for(i = 0 ; i < n ; ++i)
	{
	s = a + i;
	t = b;
	while(t < bend)
		if(*s++ != *t++)
			goto no;
	return(i+1);
	no: ;
	}
return(0);
}
