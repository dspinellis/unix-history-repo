/*	strncmp.s	4.4	84/12/02	*/

/*
 * Compare at most n characters of string
 * s1 lexicographically to string s2.
 * Return:
 *	0	s1 == s2
 *	> 0	s1 > s2
 *	< 0	s2 < s2
 *
 * strncmp(s1, s2, n)
 *	char *s1, *s2;
 *	int n;
 */
#include "DEFS.h"

ENTRY(strncmp, 0)
	movl	4(ap),r1	# r1 = s1
	movq	8(ap),r3	# r3 = s2; r4 = n
1:
	movl	$32,r5		# r5 = current blocksize
	cmpl	r4,r5		# r5 = min(n, 32);
	bgeq	2f
	movl	r4,r5
2:
	cmpc3	r5,(r1),(r3)	# compare strings
	bneq	3f
	subl2	r5,r4		# check for end of comparison
	beql	4f
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
