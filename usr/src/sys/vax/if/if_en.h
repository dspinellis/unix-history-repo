/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_en.h	6.3 (Berkeley) %G%
 */

/*
 * Structure of a Ethernet header.
 */
struct	en_header {
	u_char	en_shost;
	u_char	en_dhost;
	u_short	en_type;
};

#define	ENTYPE_PUP	0x0200		/* PUP protocol */
#define	ENTYPE_IP	0x0201		/* IP protocol */

/*
 * The ENTYPE_NTRAILER packet types starting at
 * ENTYPE_TRAIL have (type-ENTYPE_TRAIL)*512 bytes
 * of data followed by an Ethernet type (as given above)
 * and then the (variable-length) header.
 */
#define	ENTYPE_TRAIL	0x1000		/* Trailer type */
#define	ENTYPE_NTRAILER	16

#define EN_BROADCAST	0		/* Hardware broadcast address */
