/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_ii.c	5.2	%G%
 *
 *  Corrections by Robert P. Corbett, 1983 March 2
 *  Revised to restore portability, 1983 March 4
 */


long int pow_ii(ap, bp)
long int *ap, *bp;
{
	long int pow, x, n;
	int zero = 0;

	pow = 1;
	x = *ap;
	n = *bp;

	if (n == 0)
		return ( 1L );

	if (x == 0)
		return ( 0L );

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
