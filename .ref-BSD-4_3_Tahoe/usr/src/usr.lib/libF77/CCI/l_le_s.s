#	"@(#)l_le_s.s	5.1 (Berkeley) 11/3/86"

.data
.text
LL0:.align	1
.globl	_l_le
.set	MASK__,0x4
.data
.text
_l_le:.word	MASK__
	movl	4(fp),r0		/* a */
	movl	8(fp),r1		/* b */
	cmpl	12(fp),16(fp)		/* if (la <= lb) */
	jgtr	LB
	movl	12(fp), r2		/* compare according to la */
	cmps3
	jleq	out1			/* if less or equal return(1) */
	jbr	out0			/* else greater: return(0) */

LB:					/* else */
	movl	16(fp), r2		/* compare according to lb */
	cmps3
	jlss	out1			/* if less return(1) */
	jgtr	out0			/* if greater return(0) */

	addl3	4(fp), 12(fp), r2	/* aend */
LOOP2:
	cmpb	(r0), $32		/* if *a != space */
	jneq	out0			/* then astring > bstring */
	incl	r0			/* else continue */
	cmpl	r0, r2			/* till aend */
	jlssu	LOOP2
out1:
	movl	$1, r0			/* else return(1) */
	ret
out0:
	clrl	r0
	ret
