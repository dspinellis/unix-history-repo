/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hpibvar.h	7.1 (Berkeley) %G%
 */

#define	HPIBA		32
#define	HPIBB		1
#define	HPIBC		8
#define	HPIBA_BA	21
#define	HPIBC_BA	30

#define	CSA_BA		0x1F

#define	C_DCL		20
#define	C_LAG		32
#define	C_UNL		63
#define	C_TAG		64
#define	C_UNA		94
#define	C_UNT		95
#define	C_SCG		96

struct	hpib_softc {
	char	sc_alive;
	char	sc_type;
	int	sc_ba;
	char	*sc_addr;
};

extern	struct hpib_softc hpib_softc[];
