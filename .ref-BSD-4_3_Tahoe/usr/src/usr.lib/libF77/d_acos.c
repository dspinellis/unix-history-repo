/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_acos.c	5.1	6/7/85
 */

double d_acos(x)
double *x;
{
double acos();
return( acos(*x) );
}
