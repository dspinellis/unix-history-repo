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
 *	@(#)ka820.h	7.3 (Berkeley) 6/28/90
 */

/*
 * Definitions specific to the ka820 cpu.
 */

#if VAX8200

/*
 * Device addresses.
 */
#define	KA820_PORTADDR		0x20088000	/* port controller */
#define	KA820_BRAMADDR		0x20090000	/* boot ram */
#define	KA820_EEPROMADDR	0x20098000	/* eeprom */
#define	KA820_RX50ADDR		0x200b0000	/* rcx50 */
#define	KA820_CLOCKADDR		0x200b8000	/* watch chip */

/*
 * Sizes.  The port controller, RCX50, and watch chip are all one page.
 */
#define	KA820_BRPAGES		16		/* 8K */
#define	KA820_EEPAGES		64		/* 32K */

/* port controller CSR bit values */
#define	KA820PORT_RSTHALT	0x80000000	/* restart halt */
#define	KA820PORT_LCONS		0x40000000	/* logical console */
#define	KA820PORT_LCONSEN	0x20000000	/* logical console enable */
#define	KA820PORT_BIRESET	0x10000000	/* BI reset */
#define	KA820PORT_BISTF		0x08000000	/* ??? */
#define	KA820PORT_ENBAPT	0x04000000	/* ??? */
#define	KA820PORT_STPASS	0x02000000	/* self test pass */
#define	KA820PORT_RUN		0x01000000	/* run */
#define	KA820PORT_WWPE		0x00800000	/* ??? parity even? */
#define	KA820PORT_EVLCK		0x00400000	/* event lock */
#define	KA820PORT_WMEM		0x00200000	/* write mem */
#define	KA820PORT_EV4		0x00100000	/* event 4 */
#define	KA820PORT_EV3		0x00080000	/* event 3 */
#define	KA820PORT_EV2		0x00040000	/* event 2 */
#define	KA820PORT_EV1		0x00020000	/* event 1 */
#define	KA820PORT_EV0		0x00010000	/* event 0 */
#define	KA820PORT_WWPO		0x00008000	/* ??? parity odd? */
#define	KA820PORT_PERH		0x00004000	/* parity error H */
#define	KA820PORT_ENBPIPE	0x00002000	/* enable? pipe */
#define	KA820PORT_TIMEOUT	0x00001000	/* timeout */
#define	KA820PORT_RSVD		0x00000800	/* reserved */
#define	KA820PORT_CONSEN	0x00000400	/* console interrupt enable */
#define	KA820PORT_CONSCLR	0x00000200	/* clear console interrupt */
#define	KA820PORT_CONSINTR	0x00000100	/* console interrupt req */
#define	KA820PORT_RXIE		0x00000080	/* RX50 interrupt enable */
#define	KA820PORT_RXCLR		0x00000040	/* clear RX50 interrupt */
#define	KA820PORT_RXIRQ		0x00000020	/* RX50 interrupt request */
#define	KA820PORT_IPCLR		0x00000010	/* clear IP interrupt */
#define	KA820PORT_IPINTR	0x00000008	/* IP interrupt request */
#define	KA820PORT_CRDEN		0x00000004	/* enable CRD interrupts */
#define	KA820PORT_CRDCLR	0x00000002	/* clear CRD interrupt */
#define	KA820PORT_CRDINTR	0x00000001	/* CRD interrupt request */

/* what the heck */
#define	KA820PORT_BITS \
"\20\40RSTHALT\37LCONS\36LCONSEN\35BIRESET\34BISTF\33ENBAPT\32STPASS\31RUN\
\30WWPE\27EVLCK\26WMEM\25EV4\24EV3\23EV2\22EV1\21EV\20WWPO\17PERH\16ENBPIPE\
\15TIMEOUT\13CONSEN\12CONSCLR\11CONSINTR\10RXIE\7RXCLR\6RXIRQ\5IPCLR\4IPINTR\
\3CRDEN\2CLRCLR\1CRDINTR"

/* clock CSR bit values, per csr */
#define	KA820CLK_0_BUSY		0x01		/* busy (time changing) */
#define	KA820CLK_1_GO		0x0c		/* run */
#define	KA820CLK_1_SET		0x0d		/* set the time */
#define	KA820CLK_3_VALID	0x01		/* clock is valid */

#ifndef LOCORE
struct ka820port {
	u_long	csr;
	/* that seems to be all.... */
};

struct ka820clock {
	u_char	sec;
	u_char	pad0;
	u_char	secalrm;
	u_char	pad1;
	u_char	min;
	u_char	pad2;
	u_char	minalrm;
	u_char	pad3;
	u_char	hr;
	u_char	pad4;
	u_char	hralrm;
	u_char	pad5;
	u_char	dayofwk;
	u_char	pad6;
	u_char	day;
	u_char	pad7;
	u_char	mon;
	u_char	pad8;
	u_char	yr;
	u_char	pad9;
	u_short	csr0;
	u_short	csr1;
	u_short	csr2;
	u_short	csr3;
};
#endif
#endif
