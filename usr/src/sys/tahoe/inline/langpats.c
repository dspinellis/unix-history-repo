/*-
 * Copyright (c) 1984 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)langpats.c	1.8 (Berkeley) 5/8/91";
#endif /* not lint */

#include "inline.h"

/*
 * Pattern table for kernel specific routines.
 * These patterns are based on the old asm.sed script.
 */
struct pats language_ptab[] = {

	{ 0, "_spl0\n",
"	mfpr	$8,r0\n\
	mtpr	$0,$8\n" },

	{ 0, "_spl1\n",
"	mfpr	$8,r0\n\
	mtpr	$0x11,$8\n" },

	{ 0, "_spl3\n",
"	mfpr	$8,r0\n\
	mtpr	$0x13,$8\n" },

	{ 0, "_spl4\n",
"	mfpr	$8,r0\n\
	mtpr	$0x14,$8\n" },

	{ 0, "_spl5\n",
"	mfpr	$8,r0\n\
	mtpr	$0x15,$8\n" },

	{ 0, "_spl7\n",
"	mfpr	$8,r0\n\
	mtpr	$0x17,$8\n" },

	{ 0, "_spl8\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 0, "_splimp\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 0, "_splsoftclock\n",
"	mfpr	$8,r0\n\
	mtpr	$0x8,$8\n" },

	{ 0, "_splnet\n",
"	mfpr	$8,r0\n\
	mtpr	$0xc,$8\n" },

	{ 0, "_splbio\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 0, "_spltty\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 0, "_splclock\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 0, "_splhigh\n",
"	mfpr	$8,r0\n\
	mtpr	$0x18,$8\n" },

	{ 1, "_splx\n",
"	movl	(sp)+,r1\n\
	mfpr	$8,r0\n\
	mtpr	r1,$8\n" },

	{ 1, "_mfpr\n",
"	movl	(sp)+,r1\n\
	mfpr	r1,r0\n" },

	{ 2, "_mtpr\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r0\n\
	mtpr	r0,r1\n" },

#ifdef notdef
	{ 1, "_uncache\n",
"	movl	(sp)+,r1\n\
	mtpr	r1,$0x1c\n" },
#endif

	{ 0, "_setsoftclock\n",
"	mtpr	$0x8,$0x10\n" },

	{ 1, "_fuibyte\n",
"	callf	$8,_fubyte\n" },

	{ 1, "_fuiword\n",
"	callf	$8,_fuword\n" },

	{ 2, "_suibyte\n",
"	callf	$12,_subyte\n" },

	{ 2, "_suiword\n",
"	callf	$12,_suword\n" },

	{ 1, "_ffs\n",
"	movl	(sp)+,r1\n\
	ffs	r1,r0\n\
	bgeq	1f\n\
	mnegl	$1,r0\n\
1:\n\
	incl	r0\n" },

	{ 2, "__insque\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	insque	(r0),(r1)\n" },

	{ 1, "__remque\n",
"	movl	(sp)+,r1\n\
	remque	(r1)\n" },

	{ 2, "_imin\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	cmpl	r0,r1\n\
	bleq	1f\n\
	movl	r1,r0\n\
1:\n" },

	{ 2, "_imax\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	cmpl	r0,r1\n\
	bgeq	1f\n\
	movl	r1,r0\n\
1:\n" },

	{ 2, "_min\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	cmpl	r0,r1\n\
	blequ	1f\n\
	movl	r1,r0\n\
1:\n" },

	{ 2, "_max\n",
"	movl	(sp)+,r0\n\
	movl	(sp)+,r1\n\
	cmpl	r0,r1\n\
	bgequ	1f\n\
	movl	r1,r0\n\
1:\n" },

	{ 2, "__movow\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r0\n\
	movow	r0,(r1)\n" },

	{ 2, "__movob\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r0\n\
	movob	r0,(r1)\n" },

	{ 0, "_movpsl\n",
"	movpsl	r0\n" },

	{ 0, "", "" }
};
