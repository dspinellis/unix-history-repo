/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)erfc_.c	5.1	6/7/85
 */

float erfc_(x)
float *x;
{
double erfc();

return( erfc(*x) );
}
