/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)pow_hh.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

short pow_hh(ap, bp)
short *ap, *bp;
{
	short int pow, x, n;

	pow = 1;
	x = *ap;
	n = *bp;

	if (n == 0)
		return ( 1L );

	if (x == 0)
	{
		if( n > 0 )
			return ( 0L );
		else
			return ( 1/x );
	}

	if (x == 1)
		return ( 1L );

	if (x == -1)
	{
		if (n < 0)
		{
			if (n < -2)
				n += 2;
			n = -n;
		}
		if (n % 2 == 0)
			return ( 1L );
		else
			return ( -1L );
	}

	if (n > 0)
		for( ; ; )
		{
			if(n & 01)
				pow *= x;
			if(n >>= 1)
				x *= x;
			else
				break;
		}
	else
		pow = 0;

	return(pow);
}
