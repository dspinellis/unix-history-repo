/*	Knegf.s	1.2	86/01/03	*/

#include "fp.h"
#include "Kfp.h"
#include "SYS.h"

	.text
ENTRY(Knegf, 0)
	clrl	r1
	andl3	$EXPMASK,4(fp),r0	/* check for reserved operand,zero. */
	beql	retzero
	movl	4(fp),r0		/* fetch operand. */
	bbc	$31,r0,seton
	andl2	$(0!SIGNBIT),r0		/* turn it off. */
	ret
seton:	orl2	$SIGNBIT,r0		/* turn it on. */
	ret
retzero:
	clrl	r0
	ret
