/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)i_abs.c	5.1	%G%
 */

long int i_abs(x)
long int *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
