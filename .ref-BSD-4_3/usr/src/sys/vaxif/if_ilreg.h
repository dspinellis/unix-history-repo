/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_ilreg.h	7.1 (Berkeley) 6/5/86
 */

/*
 * Interlan Ethernet Communications Controller interface
 */
struct ildevice {
	short	il_csr;		/* Command and Status Register */
	short	il_bar;		/* Buffer Address Register */
	short	il_bcr;		/* Byte Count Register */
};

/*
 * Command and status bits
 */
#define	IL_EUA		0xc000		/* Extended Unibus Address */
#define	IL_CMD		0x3f00		/* Command Function Code */
#define	IL_CDONE	0x0080		/* Command Done */
#define	IL_CIE		0x0040		/* Command Interrupt Enable */
#define	IL_RDONE	0x0020		/* Receive DMA Done */
#define	IL_RIE		0x0010		/* Receive Interrupt Enable */
#define	IL_STATUS	0x000f		/* Command Status Code */

#define	IL_BITS		"\20\10CDONE\7CIE\6RDONE\5RIE"

/* command definitions */
#define	ILC_MLPBAK	0x0100		/* Set Module Interface Loopback Mode */
#define	ILC_ILPBAK	0x0200		/* Set Internal Loopback Mode */
#define	ILC_CLPBAK	0x0300		/* Clear Loopback Mode */
#define	ILC_PRMSC	0x0400		/* Set Promiscuous Receive Mode */
#define	ILC_CLPRMSC	0x0500		/* Clear Promiscuous Receive Mode */
#define	ILC_RCVERR	0x0600		/* Set Receive-On-Error Bit */
#define	ILC_CRCVERR	0x0700		/* Clear Receive-On-Error Bit */
#define	ILC_OFFLINE	0x0800		/* Go Offline */
#define	ILC_ONLINE	0x0900		/* Go Online */
#define	ILC_DIAG	0x0a00		/* Run On-board Diagnostics */
#define	ILC_ISA		0x0d00		/* Set Insert Source Address Mode */
#define	ILC_CISA	0x0e00		/* Clear Insert Source Address Mode */
#define	ILC_DEFPA	0x0f00		/* Set Physical Address to Default */
#define	ILC_ALLMC	0x1000		/* Set Receive All Multicast Packets */
#define	ILC_CALLMC	0x1100		/* Clear Receive All Multicast */
#define	ILC_STAT	0x1800		/* Report and Reset Statistics */
#define	ILC_DELAYS	0x1900		/* Report Collision Delay Times */
#define	ILC_RCV		0x2000		/* Supply Receive Buffer */
#define	ILC_LDXMIT	0x2800		/* Load Transmit Data */
#define	ILC_XMIT	0x2900		/* Load Transmit Data and Send */
#define	ILC_LDGRPS	0x2a00		/* Load Group Addresses */
#define	ILC_RMGRPS	0x2b00		/* Delete Group Addresses */
#define	ILC_LDPA	0x2c00		/* Load Physical Address */
#define	ILC_FLUSH	0x3000		/* Flush Receive BAR/BCR Queue */
#define	ILC_RESET	0x3f00		/* Reset */

/*
 * Error codes found in the status bits of the csr.
 */
#define	ILERR_SUCCESS		0	/* command successful */
#define	ILERR_RETRIES		1	/* " " with retries */
#define	ILERR_BADCMD		2	/* illegal command */
#define	ILERR_INVCMD		3	/* invalid command */
#define	ILERR_RECVERR		4	/* receiver error */
#define	ILERR_BUFSIZ		5	/* buffer size too big */
#define	ILERR_FRAMESIZ		6	/* frame size too small */
#define	ILERR_COLLISIONS	8	/* excessive collisions */
#define	ILERR_BUFALIGNMENT	10	/* buffer not word aligned */
#define	ILERR_NXM		15	/* non-existent memory */

#define	NILERRS			16
#ifdef ILERRS
char *ilerrs[NILERRS] = {
	"success",			/*  0 */
	"success with retries", 	/*  1 */
	"illegal command",		/*  2 */
	"inappropriate command",	/*  3 */
	"failure",			/*  4 */
	"buffer size exceeded",		/*  5 */
	"frame too small",		/*  6 */
	0,				/*  7 */
	"excessive collisions",		/*  8 */
	0,				/*  9 */
	"buffer alignment error",	/* 10 */
	0,				/* 11 */
	0,				/* 12 */
	0,				/* 13 */
	0,				/* 14 */
	"non-existent memory"		/* 15 */
};
#endif

/*
 * Diagnostics codes.
 */
#define	ILDIAG_SUCCESS		0	/* no problems */
#define	ILDIAG_CHKSUMERR	1	/* ROM/RAM checksum error */
#define	ILDIAG_DMAERR		2	/* DMA not working */
#define	ILDIAG_XMITERR		3	/* xmit circuitry failure */
#define	ILDIAG_RECVERR		4	/* rcvr circuitry failure */
#define	ILDIAG_LOOPBACK		5	/* loopback test failed */

#define	NILDIAGS		6
#ifdef ILDIAGS
char *ildiag[NILDIAGS] = {
	"success",			/* 0 */
	"checksum error",		/* 1 */
	"NM10 dma error",		/* 2 */
	"transmitter error",		/* 3 */
	"receiver error",		/* 4 */
	"loopback failure",		/* 5 */
};
#endif

/*
 * Frame status bits, returned in frame status byte
 * at the top of each received packet.
 */
#define	ILFSTAT_C	0x1		/* CRC error */
#define	ILFSTAT_A	0x2		/* alignment error */
#define	ILFSTAT_L	0x4		/* 1+ frames lost just before */
