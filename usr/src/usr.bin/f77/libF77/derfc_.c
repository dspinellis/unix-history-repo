/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)derfc_.c	5.1	%G%
 */

double derfc_(x)
double *x;
{
double erfc();

return( erfc(*x) );
}
