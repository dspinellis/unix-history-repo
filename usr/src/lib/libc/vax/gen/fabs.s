/*	fabs.s	4.2	84/11/01	*/
/* fabs - floating absolute value */

#include "DEFS.h"

ENTRY(fabs, 0)
	movd	4(ap),r0
	bgeq	1f
	mnegd	r0,r0
1:
	ret
