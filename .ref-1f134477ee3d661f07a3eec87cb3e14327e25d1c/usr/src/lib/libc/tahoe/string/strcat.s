/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strcat.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* 
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * Return s1.
 * 
 * char * strcat(s1, s2)
 * register char *s1, *s2;
*/
#include "DEFS.h"
	
ENTRY(strcat, 0)
	movl	4(fp),r0
	movl	r0,r1
	cmps2			# r0 and r1 point at null at end of s1
	movl	8(fp),r0	# source string
	movs2
	movl	4(fp),r0
	ret 
