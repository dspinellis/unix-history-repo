/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: rsreg.h,v 4.300 91/06/09 06:43:04 root Rel41 $ SONY
 *
 *	@(#)rsreg.h	7.1 (Berkeley) %G%
 */

#ifndef __RSREG__
#define __RSREG__ 1

/* bits in dm lsr, copied from dh.c */
#define	DML_USR		0001000		/* usr modem sig, not a real DM bit */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

#endif /* !__RSREG__ */
