/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: if_enreg.h,v 4.300 91/06/09 06:25:57 root Rel41 $ SONY
 *
 *	@(#)if_enreg.h	7.1 (Berkeley) %G%
 */

/*
 * if_enreg.h for news800, news900
 */

#ifndef __IF_ENREG__
#define __IF_ENREG__ 1

/* command definitions */
#define	ENC_ADDR	0x00		/* Get Ethernet address */
#define	ENC_DIAG	0x01		/* Run On-board Diagnostics */
#define	ENC_START	0x02		/* Go Online */
#define	ENC_RESET	0x03		/* Reset */
#define	ENC_STAT	0x04		/* Report and Reset Statistics */
#define	ENC_XMIT	0x05		/* Load Transmit Data and Send */
#define	ENC_RECV	0x06		/* Receive Data */
#define	ENC_PROM	0x07		/* Set prom mode */
#define	ENC_SADDR	0x08		/* Set Ethernet address */
#define	ENC_NEWIF	0x09		/* Switch driver interface */

#endif /* !__IF_ENREG__ */
