/*
 * Copyright (c) 1982, 1989 Regents of the University of California.
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
 *	@(#)if_types.h	7.1 (Berkeley) %G%
 */


/* interface types for benefit of parsing media address headers */
#define	IFT_LOOP	0x1		/* loopback */
#define	IFT_XETHER	0x2		/* old 3MBaud experimental ethernet */
#define	IFT_ETHER	0x3		/* 802.3 */
#define	IFT_IMP		0x4		/* Arpanet imp interface */
#define	IFT_P8010	0x5		/* Pronet 10/Pronet 80 */
#define IFT_EON		0x6		/* ISO over IP */
#define IFT_HY		0x7		/* Hyperchannel */
