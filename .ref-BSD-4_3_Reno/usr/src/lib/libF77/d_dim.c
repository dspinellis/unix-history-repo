/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_dim.c	5.1	6/7/85
 */

double d_dim(a,b)
double *a, *b;
{
return( *a > *b ? *a - *b : 0);
}
