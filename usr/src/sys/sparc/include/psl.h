/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *
 *	@(#)psl.h	8.2 (Berkeley) 9/27/93
 *
 * from: $Header: psl.h,v 1.13 93/09/27 01:37:25 torek Exp $
 */

#ifndef PSR_IMPL

/*
 * SPARC Process Status Register (in psl.h for hysterical raisins).
 *
 * The picture in the Sun manuals looks like this:
 *	                                     1 1
 *	 31   28 27   24 23   20 19       14 3 2 11    8 7 6 5 4       0
 *	+-------+-------+-------+-----------+-+-+-------+-+-+-+---------+
 *	|  impl |  ver  |  icc  |  reserved |E|E|  pil  |S|P|E|   CWP   |
 *	|       |       |n z v c|           |C|F|       | |S|T|         |
 *	+-------+-------+-------+-----------+-+-+-------+-+-+-+---------+
 */

#define	PSR_IMPL	0xf0000000	/* implementation */
#define	PSR_VER		0x0f000000	/* version */
#define	PSR_ICC		0x00f00000	/* integer condition codes */
#define	PSR_N		0x00800000	/* negative */
#define	PSR_Z		0x00400000	/* zero */
#define	PSR_O		0x00200000	/* overflow */
#define	PSR_C		0x00100000	/* carry */
#define	PSR_EC		0x00002000	/* coprocessor enable */
#define	PSR_EF		0x00001000	/* FP enable */
#define	PSR_PIL		0x00000f00	/* interrupt level */
#define	PSR_S		0x00000080	/* supervisor (kernel) mode */
#define	PSR_PS		0x00000040	/* previous supervisor mode (traps) */
#define	PSR_ET		0x00000020	/* trap enable */
#define	PSR_CWP		0x0000001f	/* current window pointer */

#define	PSR_BITS "\20\16EC\15EF\10S\7PS\6ET"

#define	PIL_CLOCK	10

#if defined(KERNEL) && !defined(LOCORE)
/*
 * GCC pseudo-functions for manipulating PSR (primarily PIL field).
 */
static __inline int getpsr() {
	int psr;

	__asm __volatile("rd %%psr,%0" : "=r" (psr));
	return (psr);
}

static __inline void setpsr(int newpsr) {
	__asm __volatile("wr %0,0,%%psr" : : "r" (newpsr));
	__asm __volatile("nop");
	__asm __volatile("nop");
	__asm __volatile("nop");
}

static __inline int spl0() {
	int psr, oldipl;

	/*
	 * wrpsr xors two values: we choose old psr and old ipl here,
	 * which gives us the same value as the old psr but with all
	 * the old PIL bits turned off.
	 */
	__asm __volatile("rd %%psr,%0" : "=r" (psr));
	oldipl = psr & PSR_PIL;
	__asm __volatile("wr %0,%1,%%psr" : : "r" (psr), "r" (oldipl));

	/*
	 * Three instructions must execute before we can depend
	 * on the bits to be changed.
	 */
	__asm __volatile("nop; nop; nop");
	return (oldipl);
}

/*
 * PIL 1 through 14 can use this macro.
 * (spl0 and splhigh are special since they put all 0s or all 1s
 * into the ipl field.)
 */
#define	SPL(name, newipl) \
static __inline int name() { \
	int psr, oldipl; \
	__asm __volatile("rd %%psr,%0" : "=r" (psr)); \
	oldipl = psr & PSR_PIL; \
	psr &= ~oldipl; \
	__asm __volatile("wr %0,%1,%%psr" : : \
	    "r" (psr), "n" ((newipl) << 8)); \
	__asm __volatile("nop; nop; nop"); \
	return (oldipl); \
}

SPL(splsoftint, 1)
#define	splnet	splsoftint
#define	splsoftclock splsoftint

/* Memory allocation (must be as high as highest network device) */
SPL(splimp, 5)

/* tty input runs at software level 6 */
#define	PIL_TTY	6
SPL(spltty, PIL_TTY)

/* audio software interrupts are at software level 4 */
#define	PIL_AUSOFT	4
SPL(splausoft, PIL_AUSOFT)

SPL(splbio, 9)

SPL(splclock, PIL_CLOCK)

/* zs hardware interrupts are at level 12 */
SPL(splzs, 12)

/* audio hardware interrupts are at level 13 */
SPL(splaudio, 13)

/* second sparc timer interrupts at level 14 */
SPL(splstatclock, 14)

static __inline int splhigh() {
	int psr, oldipl;

	__asm __volatile("rd %%psr,%0" : "=r" (psr));
	__asm __volatile("wr %0,0,%%psr" : : "r" (psr | PSR_PIL));
	__asm __volatile("and %1,%2,%0; nop; nop" : "=r" (oldipl) : \
	    "r" (psr), "n" (PSR_PIL));
	return (oldipl);
}

/* splx does not have a return value */
static __inline void splx(int newipl) {
	int psr;

	__asm __volatile("rd %%psr,%0" : "=r" (psr));
	__asm __volatile("wr %0,%1,%%psr" : : \
	    "r" (psr & ~PSR_PIL), "rn" (newipl));
	__asm __volatile("nop; nop; nop");
}
#endif /* KERNEL && !LOCORE */

#endif /* PSR_IMPL */
