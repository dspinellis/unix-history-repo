
#include "fp.h"
#include "fp_in_krnl.h"
	.text
	.globl	_Knegd
_Knegd:	.word	0x0000
	andl3	$EXPMASK,4(fp),r0	/* check for reserved operand,zero. */
	beql	retzero
	movl	4(fp),r0		/* fetch operand. */
	movl	8(fp),r1
	bbc	$31,r0,seton
	andl2	$(0!SIGNBIT),r0		/* turn it off. */
	ret
seton:	orl2	$SIGNBIT,r0		/* turn it on. */
	ret
retzero:
	clrl	r0
	clrl	r1
	ret
