/*	strlen.s	4.1	84/11/01	*/

/*
 * Return the length of cp (not counting '\0').
 *
 * strlen(cp)
 *	char *cp;
 */
	.globl	_strlen

_strlen:
	.word	0x0
	movl	4(ap),r2
0:
	locc	$0,$65535,(r2)	# look for '\0'
	beql	0b
	subl3	r2,r1,r0	# len = cp - base
	ret
