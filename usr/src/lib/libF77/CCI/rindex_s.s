#	"@(#)rindex_s.s	5.1 (Berkeley) 11/3/86"

.data
.text
LL0:.align	1
.globl	_rindex_
.set MASK__,0x1c0c
.data
.text
_rindex_: .word	MASK__

	movl	16(fp),r12	/* sublen */
	tstl	r12		/* if sublen == 0 */
	jeql	out0		/*   return(0L) */

	movl 	4(fp), r11	/* str */
	movl	8(fp), r10	/* substr */
	addl3	r11, 12(fp), r3	/* p = str + strlen */
	subl2	r12, r3		/* p -= sublen */
loop:
	cmpl	r3, r11		/* while (p >= str) */
	jlss	out0		/* not found - return(0L) */
	movl	r3, r0		/* current p (in str) */
	movl	r10, r1		/* substr */
	movl 	r12, r2		/* sublen */
	cmps3			/* if strigs equal */
	jeql	out		/* return index */

	decl	r3		/* p-- */
	jbr	loop

out:
	incl	r3		/* index = ++p - str */
	subl3	r11, r3, r0
	ret

out0:
	clrl	r0
	ret

