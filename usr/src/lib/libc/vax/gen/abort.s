/*	abort.s	4.1	83/06/27	*/

/* C library -- abort */

#include "DEFS.h"

ENTRY(abort)
	halt
	clrl	r0
	ret
