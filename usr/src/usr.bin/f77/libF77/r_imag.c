/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)r_imag.c	5.1	%G%
 */

#include "complex"

double r_imag(z)
complex *z;
{
return(z->imag);
}
