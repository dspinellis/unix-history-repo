/* @(#)bcopy.s	4.1 %G% */
/* bcopy(to, from, size) */

#include "DEFS.h"

ENTRY(bcopy)
	movc3	12(ap), *4(ap), *8(ap)
	ret
