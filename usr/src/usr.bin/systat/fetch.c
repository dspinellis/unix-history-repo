/*-
 * Copyright (c) 1980, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fetch.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include "systat.h"
#include "extern.h"

int
kvm_ckread(a, b, l)
	void *a, *b;
	int l;
{
	if (kvm_read(kd, (u_long)a, b, l) != l) {
		if (verbose)
			error("error reading kmem at %x\n", a);
		return (0);
	} 
	else
		return (1);
}
