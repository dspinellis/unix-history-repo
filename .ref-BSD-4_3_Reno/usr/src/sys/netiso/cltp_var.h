/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)cltp_var.h	7.3 (Berkeley) 6/28/90
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
