/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fixunsdfsi.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

	.data
	.align	2
Lbig:	.long	0x50000000, 0x00000000 # .double 2147483648
	.text

ENTRY(__fixunsdfsi, 0)
	cmpd2	4(fp),Lbig
	jgeq	1f
	ldd	4(fp)
	cvdl	r0
	ret

1:	ldd	4(fp)
	subd	Lbig
	cvdl	r0
	addl2	$2147483648,r0
	ret
