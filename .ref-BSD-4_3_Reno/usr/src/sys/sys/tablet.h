/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tablet.h	7.3 (Berkeley) 12/18/87
 */

#ifndef _TABLET_
/*
 * Tablet line discipline.
 */
#ifdef KERNEL
#include "../h/ioctl.h"
#else
#include <sys/ioctl.h>
#endif

/*
 * Reads on the tablet return one of the following
 * structures, depending on the underlying tablet type.
 * The first two are defined such that a read of
 * sizeof (gtcopos) on a non-gtco tablet will return
 * meaningful info.  The in-proximity bit is simulated
 * where the tablet does not directly provide the information.
 */
struct	tbpos {
	int	xpos, ypos;	/* raw x-y coordinates */
	short	status;		/* buttons/pen down */
#define	TBINPROX	0100000		/* pen in proximity of tablet */
	short	scount;		/* sample count */
};

struct	gtcopos {
	int	xpos, ypos;	/* raw x-y coordinates */
	short	status;		/* as above */
	short	scount;		/* sample count */
	short	xtilt, ytilt;	/* raw tilt */
	short	pressure;
	short	pad;		/* pad to longword boundary */
};

struct	polpos {
	short	p_x, p_y, p_z;	/* raw 3-space coordinates */
	short	p_azi, p_pit, p_rol;	/* azimuth, pitch, and roll */
	short	p_stat;		/* status, as above */
	char	p_key;		/* calculator input keyboard */
};

#define BIOSMODE	_IOW('b', 1, int)	/* set mode bit(s) */
#define BIOGMODE	_IOR('b', 2, int)	/* get mode bit(s) */
#define	TBMODE		0xfff0		/* mode bits: */
#define		TBPOINT		0x0010		/* single point */
#define		TBRUN		0x0000		/* runs contin. */
#define		TBSTOP		0x0020		/* shut-up */
#define		TBGO		0x0000		/* ~TBSTOP */
#define	TBTYPE		0x000f		/* tablet type: */
#define		TBUNUSED	0x0
#define		TBHITACHI	0x1		/* hitachi tablet */
#define		TBTIGER		0x2		/* hitachi tiger */
#define		TBGTCO		0x3		/* gtco */
#define		TBPOL		0x4		/* polhemus 3space */
#define		TBHDG		0x5		/* hdg-1111b, low res */
#define		TBHDGHIRES	0x6		/* hdg-1111b, high res */
#define		TBDIGI		0x7		/* gtco digi-pad, low res */
#define		TBDIGIHIRES	0x8		/* gtco digi-pad, high res */
#define BIOSTYPE	_IOW('b', 3, int)	/* set tablet type */
#define BIOGTYPE	_IOR('b', 4, int)	/* get tablet type*/
#endif
