/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_vv.h	4.8 (Berkeley) %G%
 */

/*
 * ECO 176-748 changed the braodcast address from 0 to 0xff, at
 * CTL (p1002) serial number around 150.
 * It was implemented in August, 1982. This is a field-installable ECO,
 * which improves net reliability. If the broadcast address has not been
 * changed, comment out the following line.
 */
#define	NEW_BROADCAST		/* new chip for broadcast problem */

/*
 * Local network header for proNET Ring
 * This is arbitrated by "jas@proteon"
 * (aka John Shriver, 617-655-3340)
 */

struct vv_header {
	 /* the first two fields are required by the hardware */
	u_char	vh_dhost;	/* destination address */
	u_char	vh_shost;	/* source address */
	/* the next three fields are the local network header */
	u_char	vh_version;	/* header version */
	u_char	vh_type;	/* packet type => protocol number */
	short	vh_info;	/* protocol-specific information */
};

#define	RING_VERSION	2	/* current version of v2lni header */

/*
 * Packet types (protocol numbers) in proNET protocol header
 * Other types are defined, but are proprietary.
 */
#define	RING_IP		1
#define	RING_IPTrailer	2	/* really, 3 = 512 bytes */
				/*         4 = 1024 bytes */
				/*         5 = 1536 bytes */
				/* it's really very messed-up! */
#define	RING_IPNTrailer	4	/* not a number, but a range */
#define RING_ARP	3	/* the next three conflict with trailers */
#define RING_HDLC	4
#define RING_VAXDB	5
#define RING_RINGWAY	6
#define RING_RINGWAYM	8
#define	RING_NOVELL	10
#define RING_PUP	12
#define RING_XNS	14
#define	RING_DIAGNOSTICS 15	/* protocol type for testing */
#define	RING_ECHO	16

#ifdef NEW_BROADCAST
#define	VV_BROADCAST	0xff	/* hardware-defined broadcast address */
#else
#define	VV_BROADCAST	0x00	/* hardware-defined broadcast address */
#endif

/*
 * Proteon proNET Hardware definitions
 * register bit definitions
 */
#define	VV_ENB	01		/* Enable Operation */
#define	VV_DEN	02		/* Enable DMA */
#define	VV_HEN	04		/* Host Relay Enable (Rcv) */
#define	VV_CPB	04		/* Clear Packet Buffer (Xmit) */
#define	VV_STE	010		/* Self Test Enable (Rcv) */
#define	VV_UT1	010		/* Unused (Xmit) */
#define	VV_LPB	020		/* Modem Disable (Rcv) */
#define	VV_INR	020		/* Initialize Ring (Xmit) */
#define	VV_RST	040		/* Reset */
#define	VV_IEN	0100		/* Interrupt Enable */
#define	VV_RDY	0200		/* Done */
#define	VV_DPR	0400		/* Data Present (Rcv) */
#define	VV_RFS	0400		/* Refused (Xmit) */
#define	VV_NXM	01000		/* Non Existent Memory */
#define	VV_OVR	02000		/* Overrun */
#define	VV_ODB	04000		/* Odd Byte (Rcv) */
#define	VV_UT2	04000		/* Unused (Xmit) */
#define	VV_LDE	010000		/* Parity on 10 megabit (Rcv), */
				/* Link Data Error on 80 megabit (Rcv) */
#define	VV_OPT	010000		/* Output Timeout (Xmit) */
#define	VV_NOK	020000		/* Ring Not OK */
#define	VV_BDF	040000		/* Bad Format in Operation */
#define	VV_NIR	0100000		/* Not in Ring */

#define	VVXERR	(VV_NXM|VV_OVR|VV_OPT|VV_BDF)	/* Xmit errs */
#define	VVRERR	(VV_NXM|VV_OVR|VV_ODB|VV_BDF)	/* Rcv errs */
#define	VVFE	(VV_NXM|VV_OVR)			/* Fatal errors */

#define VV_IBITS \
"\10\20NIR\17BDF\16NOK\15LDE\14ODB\13OVR\12NXM\11DPR\10RDY\7IEN\6RST\5LPB\4STE\3HEN\2DEN\1ENB"

#define VV_OBITS \
"\10\20NIR\17BDF\16NOK\15OPT\13OVR\12NXM\11RFS\10RDY\7IEN\6RST\5INR\3HEN\2DEN\1ENB"

/* device registers */
struct vvreg {
	short	vvicsr;		/* input csr */
	u_short	vviwc;		/* input word count */
	u_short	vviba;		/* input addr lo */
	u_short	vviea;		/* input addr hi */
	short	vvocsr;		/* output csr */
	u_short	vvowc;		/* output word count */
	u_short	vvoba;		/* output addr lo */
	u_short	vvoea;		/* output addr hi */
};

#define	VVRETRY	7		/* output retry limit */
#define VVIDENTSUCC 5		/* number of successes required in self-test */
#define VVIDENTRETRY 10		/* identify loop attempt limit */
#define VVTIMEOUT 60		/* seconds before a transmit timeout */
