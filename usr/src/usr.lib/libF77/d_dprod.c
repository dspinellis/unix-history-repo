/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_dprod.c	5.1	8/9/85
 */

double d_dprod(x,y)
double *x, *y;
{
/* dprod with -r8 flag - all in double precision */
return( (*x) * (*y) );
}
