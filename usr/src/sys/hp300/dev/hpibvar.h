/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hpibvar.h	7.2 (Berkeley) %G%
 */

#define	HPIB_IPL(x)	((((x) >> 4) & 0x3) + 3)

#define	HPIBA		32
#define	HPIBB		1
#define	HPIBC		8
#define	HPIBA_BA	21
#define	HPIBC_BA	30
#define	HPIBA_IPL	3

#define	CSA_BA		0x1F

#define	IDS_WDMA	0x04
#define	IDS_WRITE	0x08
#define	IDS_IR		0x40
#define	IDS_IE		0x80
#define	IDS_DMA(x)	(1 << (x))

#define	C_DCL		0x14
#define	C_LAG		0x20
#define	C_UNL		0x3f
#define	C_TAG		0x40
#define	C_UNA		0x5e
#define	C_UNT		0x5f
#define	C_SCG		0x60

struct	hpib_softc {
	struct	hp_ctlr *sc_hc;
	int	sc_flags;
	struct	devqueue sc_dq;
	struct	devqueue sc_sq;
	int	sc_ba;
	int	sc_type;
	char	*sc_addr;
	int	sc_count;
	int	sc_curcnt;
};

/* sc_flags */
#define	HPIBF_IO	0x1
#define	HPIBF_DONE	0x2
#define	HPIBF_PPOLL	0x4
#define	HPIBF_READ	0x8
#define	HPIBF_DMA16	0x8000

#ifdef KERNEL
extern	struct hpib_softc hpib_softc[];
extern	int internalhpib;
extern	int hpibtimeout;
extern	int hpibdmathresh;
#endif
