/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)strlen.s	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Return the length of cp (not counting '\0').
 *
 * strlen(cp)
 *	char *cp;
 */
#include "DEFS.h"

ENTRY(strlen, 0)
	movl	4(ap),r1
1:
	locc	$0,$65535,(r1)	# look for '\0'
	beql	1b
	subl3	4(ap),r1,r0	# len = cp - base
	ret
