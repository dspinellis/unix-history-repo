/*-
 * Copyright (c) 1984, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)libcpats.c	7.3 (Berkeley) %G%";
#endif /* not lint */

#include "inline.h"

/*
 * Pattern table for the C library.
 */
struct pats libc_ptab[] = {

#if defined(NOTDEF) && defined(vax)
	{ 1, "_fgetc\n",
"	sobgeq	*(sp),1f\n\
	calls	$1,__filbuf\n\
	jbr     2f\n\
1:\n\
	addl3	$4,(sp)+,r1\n\
	movzbl	*(r1),r0\n\
	incl	(r1)\n\
2:\n" },

	{ 2, "_fputc\n",
"	sobgeq	*4(sp),1f\n\
	calls	$2,__flsbuf\n\
	jbr	2f\n\
1:\n\
	movq	(sp)+,r0\n\
	movb	r0,*4(r1)\n\
	incl	4(r1)\n\
2:\n" },
#endif

#ifdef mc68000
/* someday... */
#endif mc68000

	{ 0, "", "" }
};

struct pats vaxsubset_libc_ptab[] = {

	{ 1, "_strlen\n",
"	movl	(sp)+,r5\n\
	movl	r5,r1\n\
1:\n\
	tstb	(r1)+\n\
	jneq	1b\n\
	decl	r1\n\
	subl3	r5,r1,r0\n" },

	{ 0, "", "" }
};

struct pats vax_libc_ptab[] = {

	{ 1, "_strlen\n",
"	movl	(sp)+,r5\n\
	movl	r5,r1\n\
1:\n\
	locc	$0,$65535,(r1)\n\
	jeql	1b\n\
	subl3	r5,r1,r0\n" },

	{ 0, "", "" }
};
