/*	if_ilreg.h	4.1	82/05/21	*/

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
#define	IL_EUA		0xC000		/* Extended Unibus Address */
#define	IL_CMD		0x3f00		/* Command Function Code */
#define	IL_CDONE	0x0080		/* Command Done */
#define	IL_CIE		0x0040		/* Command Interrupt Enable */
#define	IL_RDONE	0x0020		/* Receive DMA Done */
#define	IL_RIE		0x0010		/* Receive Interrupt Enable */
#define	IL_STATUS	0x000f		/* Command Status Code */

#define	IL_BITS		"\10\10CDONE\7CIE\6RDONE\5RIE"

/* Command definitions */

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
#define	ILC_STAT	0x1800		/* Report and Reset Statistics */
#define	ILC_DELAYS	0x1900		/* Report Collision Delay Times */
#define	ILC_RCV		0x2000		/* Supply Receive Buffer */
#define	ILC_LDXMIT	0x2800		/* Load Transmit Data */
#define	ILC_XMIT	0x2900		/* Load Transmit Data and Send */
#define	ILC_LDGRPS	0x2a00		/* Load Group Addresses */
#define	ILC_RMGRPS	0x2b00		/* Delete Group Addresses */
#define	ILC_FLUSH	0x3000		/* Flush Receive BAR/BCR Queue */
#define	ILC_RESET	0x3f00		/* Reset */

/*
 * Error codes
 */
char *ilerrs[] = {
			"success",			/* 0 */
			"success with retries", 	/* 01 */
			"illegal command",		/* 02 */
			"inappropriate command",	/* 03 */
			"failure",			/* 04 */
			"buffer size exceeded",		/* 05 */
			"frame too small",		/* 06 */
			0,				/* 07 */
			"excessive collisions",		/* 010 */
			0,				/* 011 */
			"buffer alignment error",	/* 012 */
			0,				/* 013 */
			0,				/* 014 */
			0,				/* 015 */
			0,				/* 016 */
			"non-existent memory"		/* 017 */
};

char *ildiag[] = {
			"success",			/* 0 */
			"checksum error",		/* 1 */
			"NM10 dma error",		/* 2 */
			"transmitter error",		/* 3 */
			"receiver error",		/* 4 */
			"loopback failure",		/* 5 */
};
