/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifdef LIBC_SCCS
_sccsid:.asciz	"@(#)ntohs.s	5.4 (Berkeley) %G%"
#endif LIBC_SCCS

/* hostorder = ntohs(netorder) */

#include "DEFS.h"

ENTRY(ntohs, 0)
	rotl	$8,4(ap),r0
	movb	5(ap),r0
	movzwl	r0,r0
	ret
