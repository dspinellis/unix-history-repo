/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)d_cnjg.c	5.2	11/3/86
 */

#include "complex"
#ifdef tahoe
#include <tahoemath/FP.h>
#endif tahoe

d_cnjg(r, z)
dcomplex *r, *z;
{
r->dreal = z->dreal;
#ifndef tahoe
r->dimag = - z->dimag;
#else tahoe
r->dimag = z->dimag;
if (z->dimag == 0.0)
	return;
else
	*(unsigned long *)&(z->dimag) ^= SIGN_BIT;
#endif tahoe
}
