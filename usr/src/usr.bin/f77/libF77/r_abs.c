/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_abs.c	5.1	%G%
 */

double r_abs(x)
float *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
