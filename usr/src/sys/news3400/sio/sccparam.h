/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: $Hdr: sccparam.h,v 4.300 91/06/09 06:44:57 root Rel41 $ SONY
 *
 *	@(#)sccparam.h	8.1 (Berkeley) 6/11/93
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
