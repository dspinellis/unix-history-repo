/* @(#)bcmp.s	4.1 %G% */
/* bcmp(s1, s2, n) */

#include "DEFS.h"

ENTRY(bcmp)
	cmpc3	12(ap), *4(ap), *8(ap)
	ret
