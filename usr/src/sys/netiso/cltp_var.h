/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)cltp_var.h	7.2 (Berkeley) %G%
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
