/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cltp_var.h	7.3 (Berkeley) %G%
 */

#define UD_TPDU_type	0x40	/* packet type */

#define CLTPOVAL_SRC	0xc1	/* Source TSAP -- required */
#define CLTPOVAL_DST	0xc2	/* Destination TSAP -- required */
#define CLTPOVAL_CSM	0xc3	/* Checksum parameter -- optional */

struct	cltpstat {
	int	cltps_hdrops;
	int	cltps_badsum;
	int	cltps_badlen;
	int	cltps_noport;
	int	cltps_ipackets;
	int	cltps_opackets;
};

#ifdef KERNEL
struct	isopcb cltb;
struct	cltpstat cltpstat;
#endif
