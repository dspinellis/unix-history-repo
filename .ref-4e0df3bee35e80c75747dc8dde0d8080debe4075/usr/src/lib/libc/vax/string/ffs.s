/* @(#)ffs.s	4.1 %G% */
/* bit = ffs(value) */

#include "DEFS.h"

ENTRY(ffs)
	ffs	$0,$32,4(ap),r0
	bneq	1f
	mnegl	$1,r0
1:
	incl	r0
	ret
