/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)functab.c	5.2 (Berkeley) %G%";
#endif not lint
/*
 * This file contains the implementation of a table for going
 * from object addresses to the functions in which they belong.
 */

#include "defs.h"
#include "mappings.h"
#include "sym.h"

#define MAXNFUNCS 1001		/* maximum number of functions allowed */

LOCAL SYM *functab[MAXNFUNCS];
LOCAL int nfuncs;

/*
 * Insert a new function into the table.
 * The table is ordered by object address.
 */

newfunc(f)
SYM *f;
{
	register int i, j;
	ADDRESS a;

	if (nfuncs >= MAXNFUNCS) {
		panic("too many procedures/functions");
	}
	a = codeloc(f);
	i = 0;
	while (i < nfuncs && codeloc(functab[i]) < a) {
		i++;
	}
	for (j = nfuncs; j > i; j--) {
		functab[j] = functab[j - 1];
	}
	functab[i] = f;
	nfuncs++;
}

/*
 * Return the function that begins at the given address.
 */

SYM *whatblock(addr)
ADDRESS addr;
{
	register int i, j, k;
	ADDRESS a;

	i = 0;
	j = nfuncs - 1;
	if (addr < codeloc(functab[i])) {
		return program;
	} else if (addr == codeloc(functab[i])) {
		return functab[i];
	} else if (addr >= codeloc(functab[j])) {
		return functab[j];
	}
	while (i <= j) {
		k = (i + j) / 2;
		a = codeloc(functab[k]);
		if (a == addr) {
			return functab[k];
		} else if (addr > a) {
			i = k+1;
		} else {
			j = k-1;
		}
	}
	if (addr > codeloc(functab[i])) {
		return functab[i];
	} else {
		return functab[i-1];
	}
	/* NOTREACHED */
}

/*
 * Clear out the functab, used when re-reading the object information.
 */

clrfunctab()
{
	nfuncs = 0;
}
