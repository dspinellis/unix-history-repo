/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)stalloc.c	1.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>

void *
stalloc(nbytes) 
	int nbytes;
{
      register void *p;

      if ((p = malloc(nbytes)) == NULL)
	    error("Out of space");
      return p;
}
