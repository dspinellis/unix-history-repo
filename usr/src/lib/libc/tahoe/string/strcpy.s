/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
_sccsid:.asciz	"@(#)strcpy.s	1.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 */
#include "DEFS.h"

ENTRY(strcpy, 0)
	movl	4(fp),r1
	movl	r1,r2
	movl	8(fp),r0
	movs2
	movl	r2,r0
	ret
