/*	strcmp.s	4.3	84/11/14	*/

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
1:
	cmpc3	$32,(r1),(r3)	# compare strings
	bneq	2f
	locc	$0,$32,-32(r1)	# check if contain null
	beql	1b
	mnegb	-32(r0)[r3],r0	# r0 = $0 - *s2
	cvtbl	r0,r0
	ret
2:
	subl3	r0,$32,r0	# check for null in matching string
	subl2	r0,r1
	locc	$0,r0,(r1)
	bneq	3f
	subb3	(r3),-(r1),r0	# r0 = *s1 - *s2
	cvtbl	r0,r0
	ret
3:
	clrl	r0		# both the same to null
	ret
