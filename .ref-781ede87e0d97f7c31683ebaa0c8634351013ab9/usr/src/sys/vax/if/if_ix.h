/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Micom-Interlan Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ix.h	7.3 (Berkeley) %G%
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
