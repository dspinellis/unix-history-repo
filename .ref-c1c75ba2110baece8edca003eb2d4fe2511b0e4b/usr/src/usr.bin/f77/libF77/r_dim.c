/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_dim.c	5.3	%G%
 */

float flt_retval;

float r_dim(a,b)
float *a, *b;
{
flt_retval =  *a > *b ? *a - *b : 0;
return(flt_retval);
}
