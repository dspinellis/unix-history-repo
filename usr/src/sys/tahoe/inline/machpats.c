/*
 * Copyright (c) 1984 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)machpats.c	1.3 (Berkeley) 2/24/86";
#endif

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
