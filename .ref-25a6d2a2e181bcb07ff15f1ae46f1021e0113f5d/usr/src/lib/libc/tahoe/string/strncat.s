/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strncat.s	1.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * At most n characters are moved.
 * Return s1.
 * 
 * char *strncat(s1, s2, n)
 * register char *s1, *s2;
 * register n;
 */
#include "DEFS.h"

ENTRY(strncat, 0)
	tstl	12(fp)
	jgtr	n_ok
	movl	4(fp),r0
	ret
n_ok:
	movl	8(fp),r0
	movl	r0,r1
	cmps2			# r0 points at null at end of s2
	subl3	8(fp),r0,r2	# r2 = numberof non-null chars in s2
	cmpl	12(fp),r2
	jgeq	r2_ok
	movl	12(fp),r2	# r2 = min (n, length(s2))
r2_ok:
	movl	4(fp),r0
	movl	r0,r1
	cmps2			# r1 points at null at end of s1
	movl	8(fp),r0	# source for copy
	movs3			# actual copy
	clrb	(r1)		# null terminated string !
	movl	4(fp),r0
	ret
