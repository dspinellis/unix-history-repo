/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Advanced Computer Communications.
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
 *	@(#)if_hdhreg.h	7.3 (Berkeley) 6/28/90
 */

/*
 * ACC IF-11/HDH interface
 */

struct hdhregs {			/* device registers */
	u_short	csr;			/* control and status register */
	u_char	iochn;			/* logical channel */
	u_char	ioadx;			/* address extension (A16,A17) */
	u_short	ioadl;			/* buffer address (A0-A15) */
	u_short	iocnt;			/* byte count */
	u_char	iofcn;			/* UMC funciton code */
	u_char	iosbf;			/* UMC subfunction code */
	u_char	ioini;			/* comm regs valid flag */
	u_char	staack;			/* interrupt acknowledge flag */
	u_char	ionmi;			/* NMI routine active flag */
	u_char	ioxfrg;			/* UMR transfer grant flag */
	u_char	stachn;			/* interrupt channel number */
	u_char	statyp;			/* interrupt type code */
	u_char	stacc;			/* completion function code */
	u_char	stacs;			/* completion subfunction code */
	u_short	stacnt;			/* completion byte count */
};

/* defines for CSR */

#define HDH_UER		0100000		/* UMC error condition */
#define HDH_NXM		0040000		/* non-existent memory error */
#define HDH_PER		0020000		/* UNIBUS parity error */
#define HDH_ZRUN	0010000		/* Z80 running */
#define HDH_ZGO		0004000		/* Z80 not in wait state */
#define HDH_MBLK	0000200		/* memory swap state (0=main, 1=srv) */
#define	HDH_SRV		0000100		/* select UMC service memory */
#define HDH_MAIN	0000040		/* select UMC main memory */
#define HDH_DMA		0000020		/* DMA enable */
#define HDH_WRT		0000010		/* DMA write enable */
#define HDH_IEN		0000004		/* interrupt enable */
#define HDH_RST		0000002		/* reset */
#define	HDH_NMI		0000001		/* cause NMI */

#define HDH_BITS \
"\10\20UER\17NXM\16PER\15ZRUN\14ZGO\10MBLK\7SRV\6MAIN\5DMA\4WRT\3IEN\2RST\1NMI"

/* start i/o function code definitions */

#define HDHWRT		0	/* write to if-11 */
#define HDHRDB		1	/* read from if-11 */
#define HDHSTR		2	/* stream flag */
#define HDHEOS		6	/* end of stream flag */
#define HDHABT		8	/* abort flag */
#define HDHUMR		16	/* UMR protocol flag */

/* interrupt type definitions */

#define HDHSACK		0	/* start i/o ack */
#define HDHDONE		1	/* i/o completion */
#define HDHXREQ		2	/* UMR protocol transfer request */

/* i/o completion codes */

#define HDHIOCOK	0001	/* successful completion */
#define HDHIOCOKP 	0002	/* successful completion, more data pending */
#define HDHIOCABT 	0361	/* i/o aborted */
#define HDHIOCERR 	0321	/* program error */
#define HDHIOCOVR 	0363	/* overrun error */
#define HDHIOCUBE 	0374	/* non-existant memory or unibus error */

/* UMR protocol transfer grant code definitions */

#define HDHXEVN		1	/* start with even address */
#define HDHXODD		2	/* start with odd address */
#define HDHNUMR		4	/* non-UMR transfer */
#define HDHXABT		8	/* abort transfer */

/* HDH supervisor request code definitions */
#define HDHINIT		0x42	/* SYSINIT opcode */

#define HDHSUP		0xf0	/* supervisor HDH status/line control prefix */
#define HDHIMP		0x400	/* IMP line up modifier */
#define HDHREFL		0x800	/* reflect mode modifier */
#define HDHINLB		0x1000	/* internal loopback modifier */
#define HDHEXLP		0x2000	/* external loopback modifier */
#define HDHRQST		(HDHSUP+0x0000)	/* line status request */
#define HDHRQUP		(HDHSUP+0x0100)	/* line up request */
#define HDHRQDN		(HDHSUP+0x0200)	/* line down request */

/* HDH supervisor reply code definitions */

#define HDHIACK		(HDHSUP+0x4200)	/* line init ack */
#define HDHLNUP		(HDHSUP+0x0100)	/* line up reply */
#define HDHLNDN		(HDHSUP+0x0200)	/* line down reply */
#define HDHLNACK	(HDHSUP+0x0300)	/* ack line up request (but line is down now) */
#define HDHTIMO		(HDHSUP+0x0400)	/* line timeout */
#define HDHLOOP		(HDHSUP+0x0500)	/* loopback message */
#define HDHDTERR	(HDHSUP+0x0600)	/* host data error detected */
#define HDHSQRCV	(HDHSUP+0x0700)	/* HDLC sequence error detected by IMP */
#define HDHSQERR	(HDHSUP+0x0800)	/* HDLC sequence error detected by if-11 */
