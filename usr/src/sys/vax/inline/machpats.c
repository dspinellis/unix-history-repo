/*-
 * Copyright (c) 1984, 1986 The Regents of the University of California.
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
static char sccsid[] = "@(#)machpats.c	7.4 (Berkeley) 5/8/91";
#endif /* not lint */

#include "inline.h"

/*
 * Pattern table for special VAX instructions.
 */
struct pats machine_ptab[] = {

#ifdef vax
	{ 3, "_blkcpy\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	movc3	r0,(r1),(r3)\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movl	(sp)+,r0\n\
	movc3	r0,(r1),(r3)\n" },

	{ 3, "_bcopy\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r5\n\
	movc3	r5,(r1),(r3)\n" },

	{ 3, "_ovbcopy\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	movc3	r5,(r3),(r4)\n" },

	{ 2, "_blkclr\n",
"	movl	(sp)+,r3\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	movc5	$0,(r3),$0,r0,(r3)\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movl	(sp)+,r0\n\
	movc5	$0,(r3),$0,r0,(r3)\n" },

	{ 2, "_bzero\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r5\n\
	movc5	$0,(r3),$0,r5,(r3)\n" },

	{ 2, "_insque\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	insque	(r4),(r5)\n" },

	{ 1, "_remque\n",
"	movl	(sp)+,r5\n\
	remque	(r5),r0\n" },
#endif vax

#ifdef mc68000
/* someday... */
#endif mc68000

	{ 0, "", "" }
};

#ifdef vax

struct pats vax_ptab[] = {

	{ 3, "_blkcmp\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	jbr	2f\n\
1:\n\
	subl2	r0,(sp)\n\
	cmpc3	r0,(r1),(r3)\n\
	bneq	3f\n\
2:\n\
	movzwl	$65535,r0\n\
	cmpl	(sp),r0\n\
	jgtr	1b\n\
	movl	(sp)+,r0\n\
	cmpc3	r0,(r1),(r3)\n\
3:\n" },

	{ 3, "_bcmp\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r5\n\
	cmpc3	r5,(r1),(r3)\n" },

	{ 3, "_llocc\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	movl	(sp)+,r1\n\
1:\n\
	movzwl	$65535,r0\n\
	cmpl	r5,r0\n\
	jleq	1f\n\
	subl2	r0,r5\n\
	locc	r4,r0,(r1)\n\
	jeql	1b\n\
	addl2	r5,r0\n\
	jbr	2f\n\
1:\n\
	locc	r4,r5,(r1)\n\
2:\n" },

	{ 3, "_locc\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	locc	r3,r4,(r5)\n" },

	{ 4, "_scanc\n",
"	movl	(sp)+,r2\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	scanc	r2,(r3),(r4),r5\n" },

	{ 3, "_skpc\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r4\n\
	movl	(sp)+,r5\n\
	skpc	r3,r4,(r5)\n" },

	{ 0, "", "" }
};

struct pats vaxsubset_ptab[] = {

	{ 3, "_blkcmp\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r0\n\
2:\n\
	cmpb	(r1)+,(r3)+\n\
	jneq	3f\n\
	sobgtr	r0,2b\n\
3:\n" },

	{ 3, "_bcmp\n",
"	movl	(sp)+,r1\n\
	movl	(sp)+,r3\n\
	movl	(sp)+,r0\n\
	tstl	r0\n\
	jeql	1f\n\
2:\n\
	cmpb	(r1)+,(r3)+\n\
	jneq	1f\n\
	sobgtr	r0,2b\n\
1:\n" },

	{ 3, "_llocc\n",
"	movl	(sp)+,r4\n\
	movl	(sp)+,r0\n\
	tstl	r0\n\
	jeql	1f\n\
	movl	(sp)+,r1\n\
2:\n\
	cmpb	r4,(r1)+\n\
	jeql	1f\n\
	sobgtr	r0,2b\n\
1:\n" },

	{ 3, "_locc\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r0\n\
	tstl	r0\n\
	jeql	1f\n\
	movl	(sp)+,r5\n\
2:\n\
	cmpb	r3,(r5)+\n\
	jeql	1f\n\
	sobgtr	r0,2b\n\
1:\n" },

	{ 3, "_skpc\n",
"	movl	(sp)+,r3\n\
	movl	(sp)+,r0\n\
	tstl	r0\n\
	jeql	1f\n\
	movl	(sp)+,r5\n\
2:\n\
	cmpb	r3,(r5)+\n\
	jneq	1f\n\
	sobgtr	r0,2b\n\
1:\n" },

	{ 0, "", "" }
};
#endif
