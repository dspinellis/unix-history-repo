/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
_sccsid:.asciz	"@(#)bzero.s	1.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bzero(base, length) */
#include "DEFS.h"

ENTRY(bzero, 0)
	movl	$1f,r0				# r0 = null source string
	movl	4(fp),r1			# r1 = destination address
	movl	8(fp),r2			# r2 = count
	movs3
	ret
1:
	.byte 0
