/*
 * Copyright (c) 1984, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)langpats.c	7.2 (Berkeley) %G%";
#endif not lint

#include "inline.h"

/*
 * Pattern table for kernel specific routines.
 * These patterns are based on the old asm.sed script.
 */
struct pats language_ptab[] = {

#ifdef vax
	{ 0, "_spl0\n",
"	mfpr	$18,r0\n\
	mtpr	$0,$18\n" },

	{ 0, "_spl1\n",
"	mfpr	$18,r0\n\
	mtpr	$1,$18\n" },

	{ 0, "_splsoftclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x8,$18\n" },

	{ 0, "_splnet\n",
"	mfpr	$18,r0\n\
	mtpr	$0xc,$18\n" },

	{ 0, "_splimp\n",
"	mfpr	$18,r0\n\
	mtpr	$0x16,$18\n" },

	{ 0, "_spl4\n",
"	mfpr	$18,r0\n\
	mtpr	$0x14,$18\n" },

	{ 0, "_splbio\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ 0, "_spltty\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ 0, "_spl5\n",
"	mfpr	$18,r0\n\
	mtpr	$0x15,$18\n" },

	{ 0, "_spl6\n",
"	mfpr	$18,r0\n\
	mtpr	$0x16,$18\n" },

	{ 0, "_spl7\n",
"	mfpr	$18,r0\n\
	mtpr	$0x17,$18\n" },

	{ 0, "_splclock\n",
"	mfpr	$18,r0\n\
	mtpr	$0x18,$18\n" },

	{ 0, "_splhigh\n",
"	mfpr	$18,r0\n\
	mtpr	$0x1f,$18\n" },

	{ 1, "_splx\n",
"	movl	(sp)+,r0\n\
	mtpr	r0,$18\n" },

	{ 1, "_mfpr\n",
"	movl	(sp)+,r5\n\
	mfpr	r5,r0\n" },

	{ 2, "_mtpr\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	mtpr	r5,r4\n" },

	{ 0, "_setsoftclock\n",
"	mtpr	$0x8,$0x14\n" },

	{ 1, "_resume\n",
"	movl	(sp)+,r5\n\
	ashl	$9,r5,r0\n\
	movpsl	-(sp)\n\
	jsb	_Resume\n" },

	{ 3, "_copyin\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r5\n\
	jsb	_Copyin\n" },

	{ 3, "_copyout\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r5\n\
	jsb	_Copyout\n" },

	{ 1, "_fubyte\n",
"	movl	(sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ 1, "_fuibyte\n",
"	movl	(sp)+,r0\n\
	jsb	_Fubyte\n" },

	{ 1, "_fuword\n",
"	movl	(sp)+,r0\n\
	jsb	_Fuword\n" },

	{ 1, "_fuiword\n",
"	movl	(sp)+,r0\n\
	jsb	_Fuword\n" },

	{ 2, "_subyte\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ 2, "_suibyte\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Subyte\n" },

	{ 2, "_suword\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ 2, "_suiword\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	jsb	_Suword\n" },

	{ 1, "_setrq\n",
"	movl	(sp)+,r0\n\
	jsb	_Setrq\n" },

	{ 1, "_remrq\n",
"	movl	(sp)+,r0\n\
	jsb	_Remrq\n" },

	{ 0, "_swtch\n",
"	movpsl	-(sp)\n\
	jsb	_Swtch\n" },

	{ 1, "_ffs\n",
"	movl	(sp)+,r1\n\
	ffs	$0,$32,r1,r0\n\
	bneq	1f\n\
	mnegl	$1,r0\n\
1:\n\
	incl	r0\n" },

	{ 1, "_htons\n",
"	movl	(sp)+,r5\n\
	rotl	$8,r5,r0\n\
	rotl	$-8,r5,r1\n\
	movb	r1,r0\n\
	movzwl	r0,r0\n" },

	{ 1, "_ntohs\n",
"	movl	(sp)+,r5\n\
	rotl	$8,r5,r0\n\
	rotl	$-8,r5,r1\n\
	movb	r1,r0\n\
	movzwl	r0,r0\n" },

	{ 1, "_htonl\n",
"	movl	(sp)+,r5\n\
	rotl	$-8,r5,r0\n\
	insv	r0,$16,$8,r0\n\
	rotl	$8,r5,r1\n\
	movb	r1,r0\n" },

	{ 1, "_ntohl\n",
"	movl	(sp)+,r5\n\
	rotl	$-8,r5,r0\n\
	insv	r0,$16,$8,r0\n\
	rotl	$8,r5,r1\n\
	movb	r1,r0\n" },

	{ 2, "__insque\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	insque	(r4),(r5)\n" },

	{ 1, "__remque\n",
"	movl	(sp)+,r5\n\
	remque	(r5),r0\n" },

	{ 2, "__queue\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	insque	(r1),*4(r0)\n" },

	{ 1, "__dequeue\n",
"	movl	(sp)+,r0\n\
	remque	*(r0),r0\n" },

	{ 2, "_imin\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5\n\
	cmpl	r0,r5\n\
	bleq	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ 2, "_imax\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5\n\
	cmpl	r0,r5\n\
	bgeq	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ 2, "_min\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5\n\
	cmpl	r0,r5\n\
	blequ	1f\n\
	movl	r5,r0\n\
1:\n" },

	{ 2, "_max\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r5\n\
	cmpl	r0,r5\n\
	bgequ	1f\n\
	movl	r5,r0\n\
1:\n" },
#endif vax

#ifdef mc68000
/* someday... */
#endif mc68000

	{ 0, "", "" }
};
