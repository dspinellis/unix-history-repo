/*	fabs.s	4.1	83/06/27	*/
/* fabs - floating absolute value */

#include "DEFS.h"

ENTRY(fabs)
	movd	4(ap),r0
	bgeq	1f
	mnegd	r0,r0
1:
	ret
