/* @(#)insque.s	4.1 %G% */
/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque)
	insque	*4(ap), *8(ap)
	ret
