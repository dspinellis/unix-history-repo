/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: sccparam.h,v 4.300 91/06/09 06:44:57 root Rel41 $ SONY
 *
 *	@(#)sccparam.h	7.1 (Berkeley) %G%
 */

/*
 *	SCC channel parameter
 */

#define	BAUD_RATE		0x0000000f
#define	RXE			0x00000010
#define	TXE			0x00000020
#define	CHAR_SIZE		0x000000c0
#define		C5BIT		0x00000000
#define		C7BIT		0x00000040
#define		C6BIT		0x00000080
#define		C8BIT		0x000000c0
#define	PARITY			0x00000100
#define	EVEN			0x00000200
#define	ODD			0x00000000
#define	STOPBIT			0x00000c00
#define		STOP1		0x00000400
#define		STOP1_5		0x00000800
#define		STOP2		0x00000c00
#define	RTS			0x00001000
#define	DTR			0x00002000
#define	XBREAK			0x00004000
#define	NOCHECK			0x00008000

#define	DCD			0x00010000
#define	CTS			0x00020000
#define	RI			0x00040000
#define	DSR			0x00080000
#define	RBREAK			0x00100000
#define	SCC_PARITY_ERROR	0x00200000
#define	OVERRUN_ERROR		0x00400000
#define	FRAMING_ERROR		0x00800000

#define	AUTO_ENABLE		0x01000000
#define	EXTCLK_ENABLE		0x02000000

#define TERM_MODE		0x70000000
#define		CJIS		0x10000000
#define		CSJIS		0x20000000
#define		CEUC		0x40000000
