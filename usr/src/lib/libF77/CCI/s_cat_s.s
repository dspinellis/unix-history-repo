#	"@(#)s_cat_s.s	5.1 (Berkeley) 11/3/86"

.data
.text
LL0:.align	1
.globl	_s_cat
.set	MASK__,0x1F04
.data
.text
_s_cat:.word	MASK__
movl	8(fp),r12		/* rpp */
movl	20(fp),r10		/* ll */
movl	12(fp),r9		/* pi = &rnp */
shll	$2,*16(fp),r1		/* *np * 4 */
addl3	r1,r9,r8		/* pn = &rnp + *np */
movl	4(fp),r1		/* lp */
jbr	L18			/* for */

L20:
movs3				/* copy nc chars from rp to lp */
addl2	$4,r12			/* rpp++ */
addl2	$4,r9			/* pi++ */

L18:
cmpl	r9,r8			/* pi < pn */
jgequ	L22			/* pn reached - out of loop */
movl	r10,r2			/* nc = ll */
cmpl	(r9),r2			/* if (*pi < nc) */
jgeq	L19
movl	(r9),r2			/* nc = *pi */

L19:
subl2	r2,r10			/* ll -= nc */
movl	(r12),r0		/* rp = *rpp */
jbr	L20

L2000005:			/* pad with spaces */
movb	$32,(r1)
incl	r1

L22:				/* while (--ll >= 0) */
decl	r10
jgeq	L2000005
ret#2

