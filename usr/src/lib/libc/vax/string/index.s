/*	index.s	4.1	84/11/01	*/

/*
 * Find the first occurence of c in the string cp.
 * Return pointer to match or null pointer.
 *
 * char *
 * index(cp, c)
 *	char *cp, c;
 */
	.globl	_index

_index:
	.word	0x0
	movq	4(ap),r1	# r1 = cp; r2 = c
	tstl	r2		# check for special case c == '\0'
	bneq	2f
1:
	locc	$0,$65535,(r1)	# just find end of string
	beql	1b		# still looking
	movl	r1,r0		# found it
	ret
2:
	movab	tbl[r2],r5	# table entry for c
	incb	(r5)
	movzwl	$65535,r4	# fast access
3:
	scanc	r4,(r1),tbl,$1	# look for c or '\0'
	beql	3b		# still looking
	movl	r1,r0		# return pointer to char
	tstb	(r0)		#    if have found '\0'
	bneq	4f
	clrl	r0		# else return 0
4:
	clrb	(r5)		# clean up table
	ret

	.data
tbl:	.byte	1
	.space	255
	.text
