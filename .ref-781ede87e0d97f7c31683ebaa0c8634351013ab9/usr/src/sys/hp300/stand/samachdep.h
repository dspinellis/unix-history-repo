/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)samachdep.h	7.1 (Berkeley) %G%
 */

#define	NHPIB		4
#define NITE		4
#define	NSCSI		2
#define NRD		(NHPIB * 8)
#define NCT		(NHPIB * 8)
#define NSD		(NSCSI * 8)

#define IOV(x)		(x)

extern	int howto, devtype;

/* bogon grfinfo structure to keep grf_softc happy */
struct grfinfo {
	int	grf_foo;
};
