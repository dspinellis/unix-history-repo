/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strcmp.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Compare string s1 lexicographically to string s2.
 * Return:
 *	0	s1 == s2
 *	> 0	s1 > s2
 *	< 0	s2 < s2
 *
 * strcmp(s1, s2)
 *	char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strcmp, 0)
	movl	4(ap),r1	# r1 = s1
	movl	8(ap),r3	# r3 = s2
	subb3	(r3),(r1),r0	# quick check for first char different
	beql	1f		# have to keep checking
	cvtbl	r0,r0
	ret
1:
	clrl	r5		# calculate min bytes to next page boundry
	subb3	r1,$255,r5	# r5 = (bytes - 1) to end of page for s1
	subb3	r3,$255,r0	# r0 = (bytes - 1) to end of page for s2
	cmpb	r0,r5		# r5 = min(r0, r5);
	bgtru	2f
	movb	r0,r5
2:
	incl	r5		# r5 = min bytes to next page boundry
	cmpc3	r5,(r1),(r3)	# compare strings
	bneq	3f
	subl2	r5,r1		# check if found null yet
	locc	$0,r5,(r1)
	beql	1b		# not yet done, continue checking
	subl2	r0,r3
	mnegb	(r3),r0		# r0 = '\0' - *s2
	cvtbl	r0,r0
	ret
3:
	subl2	r0,r5		# check for null in matching string
	subl2	r5,r1
	locc	$0,r5,(r1)
	bneq	4f
	subb3	(r3),(r1),r0	# r0 = *s1 - *s2
	cvtbl	r0,r0
	ret
4:
	clrl	r0		# both the same to null
	ret
