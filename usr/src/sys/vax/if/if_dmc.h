/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *	@(#)if_dmc.h	7.5 (Berkeley) 6/28/90
 */

/*
 * DMC-11 Interface
 */

struct dmcdevice {
        char    bsel0;
	char 	bsel1;
	char	bsel2;
	char 	bsel3;
	short	sel4;
	short	sel6;
};

/*
 * dmc software packet encapsulation.  This allows the dmc
 * link to be multiplexed among several protocols.
 * The first eight bytes of the dmc header are garbage,
 * since on a vax the uba has been known to mung these
 * bytes.  The next two bytes encapsulate packet type.
 */
struct dmc_header {
	char	dmc_buf[8];	/* space for uba on vax */
	short	dmc_type;	/* encapsulate packet type */
};

/* packet types */
#define	DMC_IPTYPE	1
#define	DMC_TRAILER	2
#define	DMC_NTRAILER	16

/*
 * DMCMTU includes space for data (1024) + 
 * protocol header (256) + trailer descriptor (4).
 * The software link encapsulation header (dmc_header)
 * is handled separately.
	#define DMCMTU  1284
 */
#define DMCMTU  1500

#define	RDYSCAN	16	/* loop delay for RDYI after RQI */

/* defines for bsel0 */
#define	DMC_BACCI	0
#define	DMC_CNTLI	1
#define	DMC_PERR	2
#define	DMC_BASEI	3
#define	DMC_WRITE	0		/* transmit block */
#define	DMC_READ	4		/* read block */
#define	DMC_RQI		0040		/* port request bit */
#define	DMC_IEI		0100		/* enable input interrupts */
#define	DMC_RDYI	0200		/* port ready */
#define	DMC0BITS	"\10\10RDI\7IEI\6RQI"

/* defines for bsel1 */
#define	DMC_MCLR	0100		/* DMC11 Master Clear */
#define	DMC_RUN		0200		/* clock running */
#define	DMC1BITS	"\10\10RUN\7MCLR"

/* defines for bsel2 */
#define	DMC_BACCO	0
#define	DMC_CNTLO	1
#define	DMC_OUX		0		/* transmit block */
#define	DMC_OUR		4		/* read block */
#define	DMC_IEO		0100		/* enable output interrupts */
#define	DMC_RDYO	0200		/* port available */
#define	DMC2BITS	"\10\10RDO\7IEO"

/* defines for CNTLI mode */
#define	DMC_HDPLX	02000		/* half duplex DDCMP operation */
#define	DMC_SEC		04000		/* half duplex secondary station */
#define	DMC_MAINT	00400		/* enter maintenance mode */

/* defines for BACCI/O and BASEI mode */
#define	DMC_XMEM	0140000		/* xmem bit position */
#define	DMC_CCOUNT	0037777		/* character count mask */
#define	DMC_RESUME	0002000		/* resume (BASEI only) */

/* defines for CNTLO */
#define	DMC_CNTMASK	01777

#define	DMC_DATACK	01
#define	DMC_TIMEOUT	02
#define	DMC_NOBUFS	04
#define	DMC_MAINTREC	010
#define	DMC_LOSTDATA	020
#define	DMC_DISCONN	0100
#define	DMC_START	0200
#define	DMC_NEXMEM	0400
#define	DMC_ERROR	01000

#define	DMC_FATAL (DMC_ERROR|DMC_NEXMEM|DMC_START|DMC_LOSTDATA|DMC_MAINTREC)
#define	CNTLO_BITS	\
   "\10\12ERROR\11NEXMEM\10START\7DISC\5LSTDATA\4MAINT\3NOBUF\2TIMEO\1DATACK"
