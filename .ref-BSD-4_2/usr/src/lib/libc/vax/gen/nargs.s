/*	nargs.s	4.1	83/06/27	*/
/* C library -- nargs */

#include "DEFS.h"

ENTRY(nargs)
	movzbl	*8(fp),r0	/* 8(fp) is old ap */
	ret
