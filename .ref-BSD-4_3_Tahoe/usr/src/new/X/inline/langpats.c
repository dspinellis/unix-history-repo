/* Copyright (c) 1984 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)langpats.c	2.8 (Berkeley) 2/20/85";
#endif

#include "inline.h"

/*
 * Pattern table for kernel specific routines.
 * These patterns are based on the old asm.sed script.
 */
struct pats language_ptab[] = {

#ifdef vax
	{ "1,_ffs\n",
"	movl	(sp)+,r1$\n\
	ffs	$0,$32,r1,r0\n\
	bneq	1f\n\
	mnegl	$1,r0\n\
1:\n\
	incl	r0\n" },

	{ "1,_htons\n",
"	movl	(sp)+,r5$\n\
	rotl	$8,r5,r0\n\
	rotl	$-8,r5,r1\n\
	movb	r1,r0\n\
	movzwl	r0,r0\n" },

	{ "1,_ntohs\n",
"	movl	(sp)+,r5$\n\
	rotl	$8,r5,r0\n\
	rotl	$-8,r5,r1\n\
	movb	r1,r0\n\
	movzwl	r0,r0\n" },

	{ "1,_htonl\n",
"	movl	(sp)+,r5$\n\
	rotl	$-8,r5,r0\n\
	insv	r0,$16,$8,r0\n\
	rotl	$8,r5,r1\n\
	movb	r1,r0\n" },

	{ "1,_ntohl\n",
"	movl	(sp)+,r5$\n\
	rotl	$-8,r5,r0\n\
	insv	r0,$16,$8,r0\n\
	rotl	$8,r5,r1\n\
	movb	r1,r0\n" },

	{ "2,__insque\n",
"	movl	(sp)+,r4*\n\
	movl	(sp)+,r5*\n\
	insque	(r4),(r5)\n" },

	{ "1,__remque\n",
"	movl	(sp)+,r5*\n\
	remque	(r5),r0\n" },

	{ "2,__queue\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1*\n\
	insque	(r1),*4(r0)\n" },

	{ "1,__dequeue\n",
"	movl	(sp)+,r0\n\
	remque	*(r0),r0\n" },

	{ "2,_imin\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5$\n\
	cmpl	r0,r5\n\
	bleq	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ "2,_imax\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5$\n\
	cmpl	r0,r5\n\
	bgeq	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ "2,_min\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5$\n\
	cmpl	r0,r5\n\
	blequ	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ "2,_max\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5$\n\
	cmpl	r0,r5\n\
	bgequ	1f\n\
	movl	r5,r0\n\
1:\n" },
#endif vax

#ifdef mc68000
/* someday... */
#endif mc68000

	{ "", "" }
};
