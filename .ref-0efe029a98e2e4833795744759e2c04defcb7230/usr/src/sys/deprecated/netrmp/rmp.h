/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: rmp.h 1.3 89/06/07$
 *
 *	@(#)rmp.h	7.1 (Berkeley) %G%
 */

/*
 *  Define MIN/MAX sizes of RMP (ethernet) packet.  For ease of computation,
 *  the 4 octet CRC field is not included.
 */

#define	RMP_MAX_PACKET	1514
#define	RMP_MIN_PACKET	60


/*
 *  Define IEEE802.2 (Logical Link Control) information.
 */

#define	ETHERTYPE_IEEE	0	/* hack hack hack */

#define	IEEE802LEN_MIN	40
#define IEEE802LEN_MAX	1500

#define	IEEE_DSAP_HP	0xF8	/* Destination Service Access Point */
#define	IEEE_SSAP_HP	0xF8	/* Source Service Access Point */
#define	IEEE_CNTL_HP	0x0300	/* Type 1 / I format control information */

#define	HPEXT_DXSAP	0x608	/* HP Destination Service Access Point */
#define	HPEXT_SXSAP	0x609	/* HP Source Service Access Point */

/*
 * HP uses 802.2 LLC with their own local extensions.  This struct makes
 * sence out of this data (encapsulated in the 802.3 packet).
 */

struct hp_llc {
	u_char	dsap;		/* 802.2 DSAP */
	u_char	ssap;		/* 802.2 SSAP */
	u_short	cntrl;		/* 802.2 control field */
	u_short	filler;		/* HP filler (must be zero) */
	u_short	dxsap;		/* HP extended DSAP */
	u_short	sxsap;		/* HP extended SSAP */
};


/*
 * Protocol(s)
 */

#define RMPPROTO_BOOT	1		/* RMP boot protocol */

#if	defined(KERNEL) & defined(RMP)
extern	struct	domain rmpdomain;
extern	struct	protosw rmpsw[];
#endif
