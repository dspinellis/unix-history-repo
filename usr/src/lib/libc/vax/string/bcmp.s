/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)bcmp.s	5.1 (Berkeley) %G%";
#endif not lint

/* bcmp(s1, s2, n) */

#include "DEFS.h"

ENTRY(bcmp, 0)
	movl	4(ap),r1
	movl	8(ap),r3
	movl	12(ap),r4
1:
	movzwl	$65535,r0
	cmpl	r4,r0
	jleq	2f
	subl2	r0,r4
	cmpc3	r0,(r1),(r3)
	jeql	1b
	addl2	r4,r0
	ret
2:
	cmpc3	r4,(r1),(r3)
	ret
