#	"@(#)system_s.s	5.1 (Berkeley) 11/3/86"

.data
.text
LL0:.align	1
.globl	_system_
.set	MASK__,0x04
.data
.text
_system_:.word	MASK__
subl3	$1056,fp,sp
movl	4(fp),r0		/* s */
subl3	$1056,fp,r1		/* buf */
cmpl	8(fp),$1000		/* if (n < 1000) */
jgeq	L9999
movl	8(fp),r2		/* length = n */
jbr	L1000
L9999:
movl	$1000,r2		/* else length = 1000 */
L1000:
movs3				/* copy string */
clrb	(r1)			/* add end-of-str */
subl3	$1056,fp,-(sp)		/* push &buf */
callf	$8,_system
ret#2

