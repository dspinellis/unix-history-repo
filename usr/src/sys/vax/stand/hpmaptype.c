/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hpmaptype.c	7.4 (Berkeley) %G%
 */

/*
 * RP??/RM?? drive type mapping routine.
 * Used for compatibility with unlabeled disks.
 */
#ifdef COMPAT_42
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/disklabel.h"

#include "../vaxmba/hpreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

short	rp06_off[8] =	{ 0, 38, 0, -1, -1, -1, 118, -1 };
short	rm03_off[8] =	{ 0, 100, 0, -1, -1, -1, 309, -1 };
short	rm05_off[8] =	{ 0, 27, 0, 562, 589, 681, 562, 82 };
short	rm80_off[8] =	{ 0, 37, 0, -1, -1, -1, 115, -1 };
short	rp07_off[8] = 	{ 0, 10, 0, 235, 245, 437, 235, 52 };
short	ml_off[8] =	{ 0, -1, -1, -1, -1, -1, -1, -1 };
/*short	cdc9775_off[8] = { 0, 13, 0, -1, -1, -1, 294, 66 };*/
short	cdc9730_off[8] = { 0, 50, 0, -1, -1, -1, 155, -1 };
short	capricorn_off[8] = { 0, 32, 0, 668, 723, 778, 668, 98 };
short	eagle_off[8] =	{ 0, 17, 0, 391, 408, 728, 391, 87 };
short	fj2361_off[8] = { 0, 13, 0, 294, 307, 547, 294, 66 };

/*
 * hptypes is used to translate Massbus drive type and other information
 * into an index in hpst.  The indices of hptypes and hpst must therefore agree.
 */
short	hptypes[] = {
	MBDT_RM03,
	MBDT_RM05,
	MBDT_RP06,
	MBDT_RM80,
	MBDT_RP05,
	MBDT_RP07,
	MBDT_ML11A,
	MBDT_ML11B,
	-1,		/* 9755 */
	-1,		/* 9730 */
	-1,		/* Capricorn */
	-1,		/* Eagle */
	MBDT_RM02,	/* actually something else */
	-1,		/* 9300 */
	-1,		/* 9766 */
	-1,		/* 2361 */
	0
};

struct st hpst[] = {
#define	HPDT_RM03	0
	32,	5,	32*5,	823,	rm03_off,	/* RM03 */
#define	HPDT_RM05	1
	32,	19,	32*19,	823,	rm05_off,	/* RM05 */
#define	HPDT_RP06	2
	22,	19,	22*19,	815,	rp06_off,	/* RP06 */
#define	HPDT_RM80	3
	31,	14, 	31*14,	559,	rm80_off,	/* RM80 */
#define	HPDT_RP05	4
	22,	19,	22*19,	411,	rp06_off,	/* RP05 */
#define	HPDT_RP07	5
	50,	32,	50*32,	630,	rp07_off,	/* RP07 */
#define	HPDT_ML11A	6
	1,	1,	1,	1,	ml_off,		/* ML11A */
#define	HPDT_ML11B	7
	1,	1,	1,	1,	ml_off,		/* ML11B */
#define	HPDT_9775	8
	32,	40,	32*40,	843,	fj2361_off,	/* 9775 */
#define	HPDT_9730	9
	32,	10,	32*10,	823,	cdc9730_off,	/* 9730 */
#define	HPDT_CAP	10
	32,	16,	32*16,	1024,	capricorn_off,	/* Ampex capricorn */
#define	HPDT_EAGLE	11
	48,	20,	48*20,	842,	eagle_off,	/* Fuji Eagle */
#define	HPDT_RM02	12
	32,	5,	32*5,	823,	rm03_off,	/* rm02 - not used */
#define	HPDT_9300	13
	32,	19,	32*19,	815,	rm05_off,	/* Ampex 9300 */
#define	HPDT_9766	14
	32,	19,	32*19,	823,	rm05_off,	/* CDC 9766 */
#define HPDT_2361	15
	64,	20,	64*20,	842,	fj2361_off,	/* Fuji 2361 */
};
#define	NTYPES	(sizeof(hpst) / sizeof(hpst[0]))

#define	MASKREG(reg)	((reg)&0xffff)

hpmaptype(hpaddr, type, unit, lp)
	register struct hpdevice *hpaddr;
	register unsigned type;
	int unit;
	register struct disklabel *lp;
{
	register i;
	register struct st *st;
	int hpsn;

	for (i = 0; hptypes[i]; i++)
		if (hptypes[i] == type)
			goto found;
	_stop("unknown drive type");

found:
	type = i;
	/*
	 * Handle SI model byte stuff when
	 * we think it's an RM03 or RM05.
	 */
	if (type == HPDT_RM03 || type == HPDT_RM05) {
		hpsn = hpaddr->hpsn;
		if ((hpsn & SIMB_LU) == unit)
		switch ((hpsn & SIMB_MB) &~ (SIMB_S6|SIRM03|SIRM05)) {

		case SI9775D:
			type = HPDT_9775;
			break;

		case SI9730D:
			type = HPDT_9730;
			break;

		case SI9766:
			type = HPDT_9766;
			break;

		case SI9762:
			type = HPDT_RM03;
			break;

		case SICAPD:
			type = HPDT_CAP;
			break;

		case SI9751D:
			type = HPDT_EAGLE;
			break;
		}
	}
	/*
	 * RM02: EMULEX controller.  Map to correct
	 * drive type by checking the holding
	 * register for the disk geometry.
	 */
	if (type == HPDT_RM02) {
		int nsectors, ntracks, ncyl;

		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXTRAK;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		DELAY(100);
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXSECT;
		nsectors = MASKREG(hpaddr->hphr) + 1;
		DELAY(100);
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXCYL;
		ncyl = MASKREG(hpaddr->hphr) + 1;
		for (type = 0; type < NTYPES; type++)
			if (hpst[type].nsect == nsectors &&
			    hpst[type].ntrak == ntracks &&
			    hpst[type].ncyl == ncyl)
				break;

		if (type >= NTYPES) {
			printf("%d sectors, %d tracks, %d cyl?\n",
				nsectors, ntracks, ncyl);
			type = HPDT_RM02;
		}
	done:
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
	}
	
	/*
	 * set up minimal disk label.
	 */
	st = &hpst[type];
	lp->d_nsectors = st->nsect;
	lp->d_ntracks = st->ntrak;
	lp->d_secpercyl = st->nspc;
	lp->d_ncylinders = st->ncyl;
	lp->d_secperunit = st->nspc * st->ncyl;
	lp->d_npartitions = 8;
	for (i = 0; i < 8; i++) {
		if (st->off[i] == -1)
			lp->d_partitions[i].p_size = 0;
		else {
			lp->d_partitions[i].p_offset = st->off[i] *
			    lp->d_secpercyl;
			lp->d_partitions[i].p_size = lp->d_secperunit;
		}
	}
}
#endif COMPAT_42
