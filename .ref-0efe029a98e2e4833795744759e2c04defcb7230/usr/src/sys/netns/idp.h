/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)idp.h	7.4 (Berkeley) %G%
 */

/*
 * Definitions for NS(tm) Internet Datagram Protocol
 */
struct idp {
	u_short	idp_sum;	/* Checksum */
	u_short	idp_len;	/* Length, in bytes, including header */
	u_char	idp_tc;		/* Transport Crontrol (i.e. hop count) */
	u_char	idp_pt;		/* Packet Type (i.e. level 2 protocol) */
	struct ns_addr	idp_dna;	/* Destination Network Address */
	struct ns_addr	idp_sna;	/* Source Network Address */
};
