#	"@(#)h_indx_s.s	5.1 (Berkeley) %G%"

.data
.text
LL0:.align	1
.globl	_h_indx
.set MASK__, 0x1c0c
.data
.text
_h_indx:	.word	MASK__

	movl	4(fp), r3		/* s = a */
	movl	8(fp), r10		/* b */
	movl	16(fp), r12		/* lb */
	addl3	r3, 12(fp), r11		/* end = a + la */
	subl2  	r12, r11		/* end -= lb */
loop:
	cmpl	r3, r11			/* while (s <= end) */
	jgtr	out0			/* not found - return(0) */
	movl	r3, r0			/* current s in string */
	movl	r10, r1			/* b */
	movl	r12, r2			/* lb */
	cmps3				/* if strings equal */
	jeql	out			/* return index */

	incl	r3			/* s++ */	
	jbr	loop

out:
	incl	r3			/* index = ++s - a */
	subl3	4(fp), r3, r0
	ret

out0:
	clrl	r0
	ret
