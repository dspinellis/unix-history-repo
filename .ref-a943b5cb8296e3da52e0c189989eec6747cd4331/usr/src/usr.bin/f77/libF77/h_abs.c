/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)h_abs.c	5.1	%G%
 */

short h_abs(x)
short *x;
{
if(*x >= 0)
	return(*x);
return(- *x);
}
