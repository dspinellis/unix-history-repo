/*-
 * Copyright (c) 1984 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)machpats.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "inline.h"

/*
 * Pattern table for special instructions.
 */
struct pats machine_ptab[] = {

	{ 3, "_blkcpy\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movblk\n" },

	{ 3, "_bcopy\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movblk\n" },

	{ 2, "_bzero\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movab	1f,r0\n\
	movs3\n\
	.data\n\
1:	.byte	0\n\
	.text\n" },

	{ 2, "_blkclr\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r2\n\
	movab	1f,r0\n\
	movs3\n\
	.data\n\
1:	.byte	0\n\
	.text\n" },

	{ 0, "", "" }
};
