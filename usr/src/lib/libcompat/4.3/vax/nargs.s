/*	nargs.s	4.2	84/11/01	*/
/* C library -- nargs */

#include "DEFS.h"

ENTRY(nargs, 0)
	movzbl	*8(fp),r0	/* 8(fp) is old ap */
	ret
