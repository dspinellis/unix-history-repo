/*	abort.s	4.2	84/11/01	*/

/* C library -- abort */

#include "DEFS.h"

ENTRY(abort, 0)
	halt
	clrl	r0
	ret
