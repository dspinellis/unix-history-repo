/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Robert Paul Corbett.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)warshall.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include "defs.h"

transitive_closure(R, n)
unsigned *R;
int n;
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rowj;
    register unsigned *rp;
    register unsigned *rend;
    register unsigned *ccol;
    register unsigned *relend;
    register unsigned *cword;
    register unsigned *rowi;

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    cword = R;
    mask = 1;
    rowi = R;
    while (rowi < relend)
    {
	ccol = cword;
	rowj = R;

	while (rowj < relend)
	{
	    if (*ccol & mask)
	    {
		rp = rowi;
		rend = rowj + rowsize;
		while (rowj < rend)
		    *rowj++ |= *rp++;
	    }
	    else
	    {
		rowj += rowsize;
	    }

	    ccol += rowsize;
	}

	mask <<= 1;
	if (mask == 0)
	{
	    mask = 1;
	    cword++;
	}

	rowi += rowsize;
    }
}

reflexive_transitive_closure(R, n)
unsigned *R;
int n;
{
    register int rowsize;
    register unsigned mask;
    register unsigned *rp;
    register unsigned *relend;

    transitive_closure(R, n);

    rowsize = WORDSIZE(n);
    relend = R + n*rowsize;

    mask = 1;
    rp = R;
    while (rp < relend)
    {
	*rp |= mask;
	mask <<= 1;
	if (mask == 0)
	{
	    mask = 1;
	    rp++;
	}

	rp += rowsize;
    }
}
