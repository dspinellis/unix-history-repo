/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)fhpibreg.h	8.1 (Berkeley) 6/10/93
 */

#include <hp/dev/iotypes.h>	/* XXX */

struct	fhpibdevice {
	u_char	hpib_pad0;
	vu_char	hpib_cid;
	u_char	hpib_pad1;
#define	hpib_ie		hpib_ids
	vu_char	hpib_ids;
	u_char	hpib_pad2;
	vu_char	hpib_ctrl2;
	u_char	hpib_pad3;
	vu_char	hpib_latch;
	u_char	hpib_pad4[9];
	vu_char	hpib_intr;
	u_char	hpib_pad5;
	vu_char	hpib_imask;
	u_char	hpib_pad6;
	vu_char	hpib_data;
	u_char	hpib_pad7;
	vu_char	hpib_stat;
	u_char	hpib_pad8;
	vu_char	hpib_cmd;
	u_char	hpib_pad9;
	vu_char	hpib_ar;
	u_char	hpib_pad10;
	vu_char	hpib_pmask;
	u_char	hpib_pad11;
	vu_char	hpib_psense;
};

/* status bits */
#define	ST_READ0        0xC0
#define	ST_READ1	0x80
#define	ST_IENAB	0x80
#define	ST_EOI		0x80
#define	ST_ATN		0x40
#define	ST_WRITE	0x00

/* control bits */
#define	CT_8BIT		0x80
#define	CT_REN		0x20
#define	CT_IFC		0x10
#define	CT_FIFOSEL	0x02
#define	CT_INITFIFO	0x01

#define	IM_PABORT	0x40
#define	IM_PPRESP	0x20
#define	IM_ROOM		0x08
#define	IM_BYTE		0x04
#define	IM_IDLE		0x02

#define	AR_ARONC	0x80
