/*	abs.s	4.2	84/11/01	*/

/* abs - int absolute value */

#include "DEFS.h"

ENTRY(abs, 0)
	movl	4(ap),r0
	bgeq	1f
	mnegl	r0,r0
1:
	ret
