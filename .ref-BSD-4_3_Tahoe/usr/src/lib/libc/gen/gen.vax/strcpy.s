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
	.asciz "@(#)strcpy.s	5.5 (Berkeley) 6/27/88"
#endif /* LIBC_SCCS and not lint */

/*
 * Copy string s2 over top of s1.
 * Return base of s1.
 *
 * char *
 * strcpy(s1, s2)
 *	char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strcpy, R6)
	movl	4(ap), r3	# r3 = s1
	movl	8(ap), r6	# r6 = s2
1:
	locc	$0,$65535,(r6)	# find length of s2
	bneq	2f
	movc3	$65535,(r6),(r3)# copy full block
	movl	r1,r6
	jbr	1b
2:
	subl2	r6,r1		# calculate length
	incl	r1
	movc3	r1,(r6),(r3)	# copy remainder
	movl	4(ap),r0	# return base of s1
	ret
