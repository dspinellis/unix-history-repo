/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bzero.s	5.1 (Berkeley) %G%";
#endif not lint

/* bzero(base, length) */

#include "DEFS.h"

ENTRY(bzero, 0)
	movl	4(ap),r3
	jbr	2f
1:
	subl2	r0,8(ap)
	movc5	$0,(r3),$0,r0,(r3)
2:
	movzwl	$65535,r0
	cmpl	8(ap),r0
	jgtr	1b
	movc5	$0,(r3),$0,8(ap),(r3)
	ret
