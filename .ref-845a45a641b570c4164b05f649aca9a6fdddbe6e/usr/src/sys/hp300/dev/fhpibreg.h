/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fhpibreg.h	7.3 (Berkeley) %G%
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
