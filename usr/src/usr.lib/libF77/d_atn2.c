/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_atn2.c	5.1	6/7/85
 */

double d_atn2(x,y)
double *x, *y;
{
double atan2();
return( atan2(*x,*y) );
}
