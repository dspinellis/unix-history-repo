/* @(#)remque.s	4.1 %G% */
/* remque(entry) */

#include "DEFS.h"

ENTRY(remque)
	remque	*4(ap),r0
	ret
