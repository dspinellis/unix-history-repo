/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *	@(#)if_ecreg.h	7.2 (Berkeley) 8/4/88
 */

/*
 * 3Com Ethernet controller registers.
 */
struct ecdevice {
	short	ec_rcr;		/* Receive Control Register */
	short	ec_xcr;		/* Transmit Control Register */
};

/*
 * Control and status bits -- rcr
 */
#define	EC_SPIE		0x8000		/* set parity interrupt enable */
#define	EC_ASTEP	0x4000		/* increment address counter */
#define	EC_AROM		0x2000		/* 1: Use address ROM, 0: use RAM */
#define	EC_PE		0x2000		/* Parity error */
#define	EC_AWCLK	0x1000		/* address write clock bit */
#define	EC_PIE		0x1000		/* Parity interrupt enable (read) */
#define	EC_ADATA	0x0f00		/* address/filtering */
#define	EC_RDONE	0x0080		/* receive done */
#define	EC_MDISAB	0x0080		/* memory disable */
#define	EC_RINTEN	0x0040		/* receive interrupt enable */
#define	EC_RCLR		0x0020		/* clear RDONE bit */
#define	EC_RWBN		0x0010		/* submit buffer for receive */
#define	EC_RBN		0x000f		/* buffer number */

#define	EC_RBITS	"\10\16PE\15PIE\10RDONE\7RINTEN"

/*
 * Control and status bits -- xcr
 */
#define	EC_JAM		0x8000		/* collision dectected */
#define	EC_JINTEN	0x4000		/* collision interrupt enable */
#define	EC_JCLR		0x2000		/* clear collision detect */
#define	EC_UECLR	0x0100		/* reset controller */
#define	EC_XDONE	0x0080		/* transmit done */
#define	EC_XINTEN	0x0040		/* transmit interrupt enable */
#define	EC_XCLR		0x0020		/* clear XDONE bit */
#define	EC_XWBN		0x0010		/* submit buffer for transmit */
#define	EC_XBN		0x000f		/* buffer number */

#define	EC_XBITS	"\10\20JAM\17JINTEN\10XDONE\7XINTEN"

/*
 * Useful combinations
 */
#define	EC_READ		(0x600|EC_RINTEN|EC_RWBN)
#define	EC_WRITE	(EC_JINTEN|EC_XINTEN|EC_XWBN)
#define	EC_CLEAR	(EC_JINTEN|EC_XINTEN|EC_JCLR)

/*
 * Buffer number definitions
 */
#define	ECTBF		0		/* Buffer for transmit */
#define	ECRLBF		1		/* First buffer for receive */
#define	ECRHBF		15		/* Last buffer for receive */

#define	ECRDOFF		528		/* Packet offset in read buffer */
