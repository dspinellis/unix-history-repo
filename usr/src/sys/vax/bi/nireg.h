/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
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
 *	@(#)nireg.h	7.3 (Berkeley) 6/28/90
 */

/*
 * Registers for the DEBNA and DEBNK Ethernet interfaces
 * (DEC calls these Network Interfaces, hence nireg.h)
 */

/*
 * this seems to be intended to be more general, but I have no details,
 * so it goes here for now
 *
 * BI Vax Port (BVP) stuff first:
 */
struct bvpregs {
	u_long	p_pcr;		/* port control register */
	u_long	p_psr;		/* port status register */
	u_long	p_per;		/* port error register */
	u_long	p_pdr;		/* port data register */
};

/*
 * BI node space registers
 */
struct ni_regs {
	struct	biiregs ni_bi;	/* BIIC registers, except GPRs */
	struct	bvpregs ni_tkp;	/* tk50 port control via BIIC GPRs */
	u_long	ni_xxx[64];	/* unused */
	u_long	ni_rxcd;	/* receive console data */
	struct	bvpregs ni_nip;	/* NI port control via BCI3 GPRs */
	u_long	ni_pudr;	/* power-up diagnostic register */
};

/* bits in ni_pudr */
#define	PUDR_TAPE	0x40000000	/* tk50 & assoc logic ok */
#define	PUDR_PATCH	0x20000000	/* patch logic ok */
#define	PUDR_VRAM	0x10000000	/* DEBNx onboard RAM ok */
#define	PUDR_VROM1	0x08000000	/* uVax ROM 1 ok */ /* ? */
#define	PUDR_VROM2	0x04000000	/* uVax ROM 2 ok */
#define	PUDR_VROM3	0x02000000	/* uVax ROM 3 ok */
#define	PUDR_VROM4	0x01000000	/* uVax ROM 4 ok */
#define	PUDR_UVAX	0x00800000	/* uVax passes self test */
#define	PUDR_BI		0x00400000	/* BIIC and BCI3 chips ok */
#define	PUDR_TMR	0x00200000	/* interval timer ok */
#define	PUDR_IRQ	0x00100000	/* no IRQ lines stuck */
#define	PUDR_NI		0x00080000	/* Ethernet ctlr ok */
#define	PUDR_TK50	0x00040000	/* tk50 present */
#define	PUDR_PRES	0x00001000	/* tk50 present (again?!) */
#define	PUDR_UVINT	0x00000800	/* uVax-to-80186 intr logic ok */
#define	PUDR_BUSHD	0x00000400	/* no bus hold errors */
#define	PUDR_II32	0x00000200	/* II32 transceivers ok */
#define	PUDR_MPSC	0x00000100	/* MPSC logic ok */
#define	PUDR_GAP	0x00000080	/* gap-detect logic ok */
#define	PUDR_MISC	0x00000040	/* misc. registers ok */
#define	PUDR_UNEXP	0x00000020	/* unexpected interrupt trapped */
#define	PUDR_80186	0x00000010	/* 80186 ok */
#define	PUDR_PATCH	0x00000008	/* patch logic ok (again) */
#define	PUDR_8RAM	0x00000004	/* 80186 RAM ok */
#define	PUDR_8ROM2	0x00000002	/* 80186 ROM1 ok */
#define	PUDR_8ROM1	0x00000001	/* 80186 ROM2 ok */
