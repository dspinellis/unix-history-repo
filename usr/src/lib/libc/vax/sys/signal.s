/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
_sccsid:.asciz	"@(#)signal.s	5.3 (Berkeley) %G%"
#endif /* SYSLIBC_SCCS and not lint */

#include "SYS.h"

	.globl	__sigsim
ENTRY(signal)
	blbs	__sigsim,2f
	chmk	$SYS_signal
	bcs	1f
	ret
1:
	jmp	cerror

/*
 * Must simulate signal 
 */
2:
	movq	4(ap),-(sp)
	calls	$2,_signalsim
	ret
