/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)negd.s	1.3 (Berkeley) 6/1/90"
#endif /* SYSLIBC_SCCS and not lint */

#include <tahoemath/fp.h>
#include "DEFS.h"

XENTRY(negd, 0)
	andl3	$EXPMASK,4(fp),r0	/* check for reserved operand,zero. */
	beql	isreserved
	movl	4(fp),r0		/* fetch operand. */
	movl	8(fp),r1
	bbc	$31,r0,seton
	andl2	$(0!SIGNBIT),r0		/* turn it off. */
	ret
seton:	orl2	$SIGNBIT,r0		/* turn it on. */
	ret
isreserved:
	bbc	$31,4(fp),retzero
	callf	$4,fpresop
	ret
retzero:
	clrl	r0
	clrl	r1
	ret
