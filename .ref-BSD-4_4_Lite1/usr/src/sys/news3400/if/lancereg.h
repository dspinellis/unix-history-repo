/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: lancereg.h,v 4.300 91/06/09 06:26:05 root Rel41 $ SONY
 *
 *	@(#)lancereg.h	8.1 (Berkeley) 6/11/93
 */

struct lance {
	u_short	rdp;			/* data port			*/
	u_short rap;			/* address port			*/
};

/*
 *	Control and status registers
 */
#define	CSR0		0
#define	CSR1		1
#define	CSR2		2
#define	CSR3		3

/*
 *	CSR0
 */
#define	CSR_ERR		0x8000		/* BABL|CERR|MISS|MERR		*/
#define	CSR_BABL	0x4000		/* transmitter timeout error	*/
#define	CSR_CERR	0x2000		/* collision error		*/
#define	CSR_MISS	0x1000		/* missed packet		*/
#define	CSR_MERR	0x0800		/* memory error			*/
#define	CSR_RINT	0x0400		/* receiver interrupt		*/
#define	CSR_TINT	0x0200		/* transmitter interrupt	*/
#define	CSR_IDON	0x0100		/* initailization done		*/
#define	CSR_INTR	0x0080		/* BABL|MISS|MERR|RINT|TINT|IDON */
#define	CSR_INEA	0x0040		/* interrupt enable		*/
#define	CSR_RXON	0x0020		/* receiver on			*/
#define	CSR_TXON	0x0010		/* transmitter on		*/
#define	CSR_TDMD	0x0008		/* transmit demand		*/
#define	CSR_STOP	0x0004		/* disable chip			*/
#define	CSR_STRT	0x0002		/* enable chip			*/
#define	CSR_INIT	0x0001		/* initialize			*/

/*
 *	CSR3
 */
#define	CSR_BSWP	0x0004		/* byte swap			*/
#define	CSR_ACON	0x0002		/* ALE control			*/
#define	CSR_BCON	0x0001		/* byte control			*/

/*
 *	Initialization block
 */
struct init_block {
	u_short	ib_mode;
	u_char  ib_padr[6];
	u_char  ib_ladrf[8];
	u_short ib_rdra;
	u_short ib_rlen_rdra;
	u_short ib_tdra;
	u_short ib_tlen_tdra;
};

#define	IB_PROM		0x8000		/* promiscuous mode		*/
#define	IB_INTL		0x0040		/* internal loopback		*/
#define	IB_DRTY		0x0020		/* disable retry		*/
#define	IB_COLL		0x0010		/* force collision		*/
#define	IB_DTCR		0x0008		/* disable transmit CRC		*/
#define	IB_LOOP		0x0004		/* loopback			*/
#define	IB_DTX		0x0002		/* disable the transmitter	*/
#define	IB_DRX		0x0001		/* disable the receiver		*/

/*
 *	Descriptor rings
 */

/*
 *	Receive message descriptor
 */
struct recv_msg_desc {
	u_short rmd_ladr;
	u_short rmd_stat;
	u_short rmd_bcnt;
	u_short rmd_mcnt;
};

#define	RMD_OWN		0x8000		/* owned by lance		*/
#define	RMD_ERR		0x4000		/* FRAM|OFLO|CRC|BUFF		*/
#define	RMD_FRAM	0x2000		/* framing error		*/
#define	RMD_OFLO	0x1000		/* overflow			*/
#define	RMD_CRC		0x0800		/* CRC error			*/
#define	RMD_BUFF	0x0400		/* buffer error			*/
#define	RMD_STP		0x0200		/* start op packet		*/
#define	RMD_ENP		0x0100		/* end of packet		*/
#define	RMD_HADR	0x00ff		/* high order 8 bit of buffer address */

/*
 *	Transmit message descriptor
 */
struct xmit_msg_desc {
	u_short tmd_ladr;
	u_short tmd_stat;
	u_short tmd_bcnt;
	u_short tmd_error;
};

#define	TMD_OWN		0x8000		/* owned by lance		*/
#define	TMD_ERR		0x4000		/* LCOL|LCAR|UFLO|RTRY		*/
#define	TMD_MORE	0x1000		/* more than one retry		*/
#define	TMD_ONE		0x0800		/* one retry			*/
#define	TMD_DEF		0x0400		/* deferred			*/
#define	TMD_STP		0x0200		/* start of packet		*/
#define	TMD_ENP		0x0100		/* end of packet		*/
#define	TMD_HADR	0x00ff		/* high order 8 bit of buffer address */

#define	TMD_BUFF	0x8000		/* buffer error			*/
#define	TMD_UFLO	0x4000		/* underflow error		*/
#define	TMD_LCOL	0x1000		/* late collision		*/
#define	TMD_LCAR	0x0800		/* loss of carrier		*/
#define	TMD_RTRY	0x0400		/* retry error			*/
#define	TMD_TDR		0x00ff		/* time domain refrectometry	*/
