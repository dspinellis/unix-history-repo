/*	strcpy.s	4.1	84/11/01	*/

/*
 * Copy string s2 over top of s1.
 *
 * char *
 * strcpy(s1, s2)
 *	char *s1, *s2;
 */
	.globl	_strcpy

_strcpy:
	.word	0x80
	movl	4(ap), r3	# r3 = s1
	movl	8(ap), r6	# r6 = s2
1:
	locc	$0,$65535,(r6)	# find length of s2
	bneq	2f
	movc3	$65535,(r6),(r3)# copy full block
	movl	r6,r1
	jbr	1b
2:
	subl2	r6,r1		# calculate length
	incl	r1
	movc3	r1,(r6),(r3)	# copy remainder
	movl	r6,r0
	ret
