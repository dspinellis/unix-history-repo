/*
 * Copyright (c) 1986 The Regents of the University of California.
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
 *	@(#)ikreg.h	7.3 (Berkeley) 6/28/90
 */

/*
 * IKON DR-11W register definitions.
 */
struct	ikdevice {
	u_short ik_csr;		/* control status register */
	u_short ik_data;	/* data in/out register */
	u_char	ik_mod;		/* address modifier */
	u_char	ik_vec;		/* interrupt vector */
	u_short ik_pulse;	/* pulse commands (w) */
	u_short ik_fill[5];
	u_short ik_balo;	/* low word of dma beginning address (w) */
	u_short ik_wc;		/* dma word count */
	u_short ik_calo;	/* low word of dma current address (r) */
	u_short ik_fill1;
	u_short ik_bahi;	/* high word of dma beginning address (w) */
	u_short ik_fill2;
	u_short ik_cahi;	/* high word of dma current address (r) */
};

/*
 * CSR control definitions (write-only).
 */
#define IKCSR_GO	0x0001		/* start dma */
#define IKCSR_FNC1	0x0002		/* function bit 1 */
#define IKCSR_FNC2	0x0004		/* function bit 2 */
#define IKCSR_FNC3	0x0008		/* function bit 3 */
/* bits 4-5 are unused */
#define IKCSR_IENA	0x0040		/* enable/disable interrupts */
/* bit 7 is unused */
#define IKCSR_CYCLE	0x0100		/* force dma to cycle */
/* bits 9-11 are unused */
#define IKCSR_MCLR	0x1000		/* master clear board */
#define IKCSR_RPERR	0x2000		/* reset parity error */
#define IKCSR_RATTF	0x4000		/* reset attention */
#define IKCSR_RDMAF	0x8000		/* reset dma completion */

/*
 * CSR status definitions (read-only).
 */
#define IKCSR_DEV	0x0001		/* device flag (0 = 10083, 1 = 10077) */
/* bits 1-3 reflect the function latch state */
#define IKCSR_TIMO	0x0010		/* bus timeout during dma */
#define IKCSR_BERR	0x0020		/* bus error during dma */
/* bit 6 reflects interrupt enable state */
#define IKCSR_READY	0x0080		/* device ready for next command */
/* bit 8 should be 0 */
#define IKCSR_STATC	0x0200		/* status bit C */
#define IKCSR_STATB	0x0400		/* status bit B */
#define IKCSR_STATA	0x0800		/* status bit A */
#define IKCSR_PERR	0x1000		/* parity error during pi/o or dma */
#define IKCSR_ATTN	0x2000		/* current state of attention bit */
#define IKCSR_ATTF	0x4000		/* latched attention t-f transition */
#define IKCSR_DMAF	0x8000		/* dma completed or terminated */

#define IKCSR_BITS \
"\020\1DEV\2FNC1\3FNC2\4FNC3\5TIMO\6BERR\7IENA\10READY\12STATC\13STATB\14STATA\
\15PERR\16ATTN\17ATTF\20DMAF"

/*
 * Pulse command register definitions (write-only).
 */
#define IKPULSE_GO	0x0001		/* enable dma */
#define IKPULSE_FNC2	0x0004		/* pulse function bit 1 */
#define IKPULSE_RIENA	0x0020		/* reset IKCSR_IENA */
#define IKPULSE_SIENA	0x0040		/* set IKCSR_IENA */
#define IKPULSE_CYCL	0x0100		/* force dma to cycle */
#define IKPULSE_MCLR	0x1000		/* initialize interface */
#define IKPULSE_RPERR	0x2000		/* reset IKCSR_PERR */
#define IKPULSE_RATTF	0x4000		/* reset IKCSR_ATTF */
#define IKPULSE_RDMAF	0x8000		/* reset IKCSR_DMAF */
