/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)r_imag.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "complex"

float r_imag(z)
complex *z;
{
return(z->imag);
}
