/*
 * Copyright (c) 1983 Regents of the University of California.
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
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strcat.s	5.5 (Berkeley) 6/27/88"
#endif /* LIBC_SCCS and not lint */

/*
 * Concatenate string s2 to the end of s1
 * and return the base of s1.
 *
 * char *
 * strcat(s1, s2)
 *	char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strcat, R6|R7)
	movq	4(ap), r6	# r6 = s1; r7 = s2
	movl	r6,r1
0:
	locc	$0,$65535,(r1)	# look for '\0'
	beql	0b
	movl	r1,r3		# save end of s1
1:
	locc	$0,$65535,(r7)	# find length of s2
	bneq	2f
	movc3	$65535,(r7),(r3)# copy full block
	movl	r1,r7
	jbr	1b
2:
	subl2	r7,r1		# calculate length
	incl	r1
	movc3	r1,(r7),(r3)	# copy remainder
	movl	r6,r0
	ret
