/* @(#)insque.s	4.2 %G% */
/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(ap), *8(ap)
	ret
