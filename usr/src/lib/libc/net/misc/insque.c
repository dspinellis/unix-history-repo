/* @(#)insque.c	4.1 12/15/82 */
/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque)
	insque	*4(ap), *8(ap)
	ret
