/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)hpibvar.h	7.1 (Berkeley) 5/8/90
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
