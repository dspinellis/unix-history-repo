/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)memchr.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Find the first occurence of c in the memory at cp (length n).
 * Return pointer to match or null pointer.
 *
 * This code optimises the usual case (0 < n < 65535).
 *
 * void *
 * memchr(cp, c, n)
 *	char *cp, c;
 *	size_t n;
 */

#include "DEFS.h"

ENTRY(memchr, 0)
	movq	4(ap),r1	# r1 = cp; r2 = c
	movl	12(ap),r0	# r0 = n
	movzwl	$65535,r4	# handy constant
0:
	cmpl	r0,r4		# check for annoying locc limit
	bgtru	3f

	/* n <= 65535 */
	locc	r2,r0,(r1)	# search n bytes for c
	beql	2f		# done if not found (r0 already 0)
1:	/* found character c at (r1) */
	movl	r1,r0
2:
	ret

3:	/* n > 65535 */
	locc	r2,r4,(r1)	# search 65535 bytes for c
	beql	1b		# done if found
	decw	r0		# from 0 to 65535
	subl2	r0,r4		# adjust n
	brb	0b		# and loop
