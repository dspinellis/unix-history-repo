/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)fetch.c	5.5 (Berkeley) %G%";
#endif not lint

#include "systat.h"

kvm_ckread(a, b, l)
	void *a, *b;
	int l;
{
	extern int verbose;

	if (kvm_read(a, b, l) != l) {
		if (verbose)
			error("error reading kmem at %x\n", a);
		return (0);
	} 
	else
		return (1);
}
