/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwend.c	3.16 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwend()
{
	xxend();
	(void) wwsettty(0, &wwoldtty);
}
