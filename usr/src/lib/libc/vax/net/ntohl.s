/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ntohl.s	5.1 (Berkeley) %G%";
#endif not lint

/* hostorder = ntohl(netorder) */

#include "DEFS.h"

ENTRY(ntohl)
	rotl	$-8,4(ap),r0
	insv	r0,$16,$8,r0
	movb	7(ap),r0
	ret
