#	"@(#)l_gt_s.s	5.1 (Berkeley) 11/3/86"

.data
.text
LL0:.align	1
.globl	_l_gt
.set	MASK__,0x4
.data
.text
_l_gt:.word	MASK__
	movl	4(fp),r0		/* a */
	movl	8(fp),r1		/* b */
	cmpl	12(fp),16(fp)		/* if (la <= lb) */
	jgtr	LB
	movl	12(fp), r2		/* compare according to la */
	cmps3
	jleq	out0			/* if less or equal return(0) */
	jbr	out1			/* else greater: return(1) */

LB:					/* else */
	movl	16(fp), r2		/* compare according to lb */
	cmps3
	jlss	out0			/* if less return(0) */
	jgtr	out1			/* if greater return(1) */

	addl3	4(fp), 12(fp), r2	/* aend */
LOOP2:
	cmpb	(r0), $32		/* if *a != space */
	jneq	out1			/* then astring > bstring */
	incl	r0			/* else continue */
	cmpl	r0, r2			/* till aend */
	jlssu	LOOP2
out0:
	clrl	r0
	ret
out1:
	movl	$1, r0			/* else return(1) */
	ret
