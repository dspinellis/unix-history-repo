/*-
 * Copyright (c) 1986, 1988 The Regents of the University of California.
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
 *
 *	@(#)ka630.h	7.4 (Berkeley) 5/9/91
 */

/*
 * Definitions specific to the ka630 uvax2 cpu card. Includes the tod
 * clock chip and the cpu registers.
 */
#ifdef VAX630
/* Bdr register bits */
#define	KA630BDR_PWROK	0x8000
#define	KA630BDR_HLTENB	0x4000
#define	KA630BDR_CPU	0x0c00
#define	KA630BDR_BDG	0x0300
#define	KA630BDR_DSPL	0x000f

/* Memory system err reg. */
#define	KA630MSER_CD	0x00000300
#define	KA630MSER_NXM	0x00000080
#define	KA630MSER_LPE	0x00000040
#define	KA630MSER_QPE	0x00000020
#define	KA630MSER_MERR	0x000000f0
#define	KA630MSER_CPUER	0x00000060
#define	KA630MSER_DQPE	0x00000010
#define	KA630MSER_LEB	0x00000008
#define	KA630MSER_WRWP	0x00000002
#define	KA630MSER_PAREN	0x00000001

/* Mem. error address regs. */
#define	KA630CEAR_PG	0x00007fff
#define	KA630DEAR_PG	0x00007fff

/* Clock registers and constants */
#define	MINSEC	60
#define	HRSEC	3600

#define	KA630CLK_VRT	0200
#define	KA630CLK_UIP	0200
#define	KA630CLK_RATE	040
#define	KA630CLK_ENABLE	06
#define	KA630CLK_SET	0206
/* cpmbx bits */
#define	KA630CLK_HLTACT	03
/* halt action values */
#define	KA630CLK_RESTRT	01
#define	KA630CLK_REBOOT	02
#define	KA630CLK_HALT	03
/* in progress flags */
#define	KA630CLK_BOOT	04
#define	KA630CLK_RSTRT	010
#define	KA630CLK_LANG	0360

#ifndef LOCORE
struct ka630clock {
	u_short	sec;
	u_short	secalrm;
	u_short	min;
	u_short	minalrm;
	u_short	hr;
	u_short	hralrm;
	u_short	dayofwk;
	u_short	day;
	u_short	mon;
	u_short	yr;
	u_short	csr0;
	u_short	csr1;
	u_short	csr2;
	u_short	csr3;
	u_short	cpmbx;	/* CPMBX is used by the boot rom. see ka630-ug-3.3.3 */
};

struct ka630cpu {
	u_short ka630_bdr;
	u_short ka630_xxx;
	u_long  ka630_mser;
	u_long  ka630_cear;
	u_long  ka630_dear;
};
#endif
#endif
