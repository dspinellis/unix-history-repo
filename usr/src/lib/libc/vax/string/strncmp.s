/*	strncmp.s	4.1	84/11/01	*/

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
	.globl	_strncmp

_strncmp:
	.word	0x0
	movl	12(ap),r5	# r5 = n
	movq	4(ap),r3	# r3 = s1; r4 = s2
1:
	locc	$0,$65535,(r4)	# look for '\0' in s2
	bneq	2f
	cmpl	r5,$65535	# n > chunk size?
	bgtr	4f		# yes
	movl	r5,r1		# no, compare only n bytes
	jbr	5f
4:
	subl2	$65535,r5	# adjust n
	cmpc3	$65535,(r4),(r3)# compare full block
	bneq	3f
	movl	r1,r4		# advance s2
	jbr	1b		# matched, next block
2:
	subl2	r4,r1		# calculate length
	incl	r1		# +1 for '\0'
	cmpl	r1,r5		# length <= n
	bleq	5f		# yes, compare full string
	movl	r5,r1		# no, compare only n bytes
5:
	cmpc3	r1,(r4),(r3)	# compare remainder
	bneq	3f
	ret			# r0 = 0 already
3:
	subb3	(r1),(r3),r0	# r0 = *s1 - *s2
	cvtbl	r0,r0
	ret
