/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Micom-Interlan Inc.
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
 *	@(#)if_ix.h	7.3 (Berkeley) 6/28/90
 */

union ix_stats {
	struct {				/* General statistics below */
		u_char	macg_physaddr[6];
		u_short macg_pad;
		u_long	dlag_rcvmac;	/* packets received by DLA from MAC */
		u_long	dlag_rcvpass;	/* packets passed to users by DLA */
		u_long	dlag_txreq;	/* packets sent by users to DLA */
		u_long	dlag_txsnt;	/* packets sent by DLA to MAC */
		u_short	dlag_chaopn;	/* channels open */
		u_short	dlag_maxopn;	/* max channels opened concurrently */
		u_long	macg_frmtos;	/* packets discarded by MAC */
		u_long	macg_frmpas;	/* packets sent to DLA by MAC */
		u_long	macg_x2x;	/* packets put on wire by MAC */
		u_long	macg_x2r;	/* packets looped by MAC */
		u_long	macg_xrty;	/* transmitter retries */
		u_short	macg_noap;	/* open MAC paths */
		u_short	macg_nprom;	/* open promiscuous paths */
		u_short	macg_conopn;	/* max concurrent MAC paths */
		u_short	sysg_crce;	/* CRC errors */
		u_short	sysg_alne;	/* alignment errors */
		u_short	sysg_rsce;	/* resource errors */
		u_short	sysg_ovre;	/* overrun errors */
	} ixg;
	struct {			/* Channel statistics below */
		u_long	dabc_rcvacc;	/* packets received */
		u_long	dabc_rcvtoss;	/* packets discarded, queue full */
		u_long	dabc_rcvpass;	/* packets passed to user */
		u_long	dabc_txreq;	/* packets sent by  user */
		u_long	dabc_txsent;	/* packets sent to MAC */
		u_long	macc_rcvcnt;	/* packets received by MAC */
		u_long	macc_txtcnt;	/* packets sent by MAC to wire */
		u_long	macc_lowmem;	/* packets discarded, no mem  */
	} ixc;
};
#define IXC_MAP(a)	(((a) << 6) | 0100077)

#define IXC_OPEN	IXC_MAP(1)		/* Open Channel */
#define IXC_CLOSE	IXC_MAP(2)		/* Close Channel */
#define IXC_MCAST	IXC_MAP(3)		/* Set Multicast Addresses */
#define IXC_RECV	IXC_MAP(4)		/* Receive Frame */
#define IXC_RECVF	IXC_MAP(5)		/* Receive Fragment */
#define IXC_XMIT	IXC_MAP(6)		/* Send Frame */
#define IXC_GSTAT	IXC_MAP(7)		/* Get General Statistics */
#define IXC_CSTAT	IXC_MAP(8)		/* Get Channel Statistics */
#define IXC_GSCLR	IXC_MAP(9)		/* Clear General Statistics */
#define IXC_CSCLR	IXC_MAP(10)		/* Clear Channel Statistics */
#define IXC_RESET	IXC_MAP(11)		/* Reset DLA module */
#define IXC_LDPA	IXC_MAP(12)		/* Load Physical Address */
