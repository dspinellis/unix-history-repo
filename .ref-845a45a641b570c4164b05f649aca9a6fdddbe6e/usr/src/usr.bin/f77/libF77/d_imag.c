/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)d_imag.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "complex"

double d_imag(z)
dcomplex *z;
{
return(z->dimag);
}
