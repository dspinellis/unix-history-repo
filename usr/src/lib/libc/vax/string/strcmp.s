/*	strcmp.s	4.1	84/11/01	*/

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
	.globl	_strcmp

_strcmp:
	.word	0x0
	movq	4(ap),r3	# r3 = s1; r4 = s2
1:
	locc	$0,$65535,(r4)	# look for '\0' in s2
	bneq	2f
	cmpc3	$65535,(r4),(r3)# compare full block
	bneq	3f
	movl	r1,r4		# advance s2
	jbr	1b		# matched, next block
2:
	subl2	r4,r1		# calculate length
	incl	r1		# +1 for '\0'
	cmpc3	r1,(r4),(r3)	# compare remainder
	bneq	3f
	ret			# r0 = 0 already
3:
	subb3	(r1),(r3),r0	# r0 = *s1 - *s2
	cvtbl	r0,r0
	ret
