/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hpmaptype.c	6.2 (Berkeley) %G%
 */

/*
 * RP??/RM?? drive type mapping routine.
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

#include "../vaxmba/hpreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

/* THIS SHOULD BE READ IN OFF THE PACK, PER DRIVE */
short	rp06_off[8] =	{ 0, 38, 0, -1, -1, -1, 118, -1 };
short	rm03_off[8] =	{ 0, 100, 0, -1, -1, -1, 309, -1 };
short	rm05_off[8] =	{ 0, 27, 0, 562, 589, 681, 562, 82 };
short	rm80_off[8] =	{ 0, 37, 0, -1, -1, -1, 115, -1 };
short	rp07_off[8] = 	{ 0, 10, 0, 235, 245, 437, 235, 52 };
short	ml_off[8] =	{ 0, -1, -1, -1, -1, -1, -1, -1 };
short	cdc9775_off[8] = { 0, 13, 0, -1, -1, -1, 294, 66 };
short	cdc9730_off[8] = { 0, 50, 0, -1, -1, -1, 155, -1 };
short	capricorn_off[8] = { 0, 32, 0, 668, 723, 778, 668, 98 };
short	eagle_off[8] =	{ 0, 17, 0, 391, 408, 728, 391, 87 };
/* END SHOULD BE READ IN */

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
	32,	40,	32*40,	843,	cdc9775_off,	/* 9775 */
#define	HPDT_9730	9
	32,	10,	32*10,	823,	cdc9730_off,	/* 9730 */
#define	HPDT_CAP	10
	32,	16,	32*16,	1024,	capricorn_off,	/* Ampex capricorn */
#define	HPDT_EAGLE	11
	48,	20,	48*20,	842,	eagle_off,	/* Fuji Eagle */
#define	HPDT_RM02	12
	1,	1,	1,	1,	0,		/* rm02 - not used */
#define	HPDT_9300	13
	32,	19,	32*19,	815,	rm05_off,	/* Ampex 9300 */
};

#define	MASKREG(reg)	((reg)&0xffff)

hpmaptype(hpaddr, type, unit)
	register struct hpdevice *hpaddr;
	unsigned type;
	int unit;
{
	int ntracks, hpsn;

	/*
	 * Handle SI model byte stuff when
	 * we think it's an RM03 or RM05.
	 */
	if (type == HPDT_RM03 || type == HPDT_RM05) {
		hpsn = hpaddr->hpsn;
		if ((hpsn & SIMB_LU) != unit)
			return (type);
		switch ((hpsn & SIMB_MB) &~ (SIMB_S6|SIRM03|SIRM05)) {

		case SI9775D:
			return (HPDT_9775);

		case SI9730D:
			return (HPDT_9730);

		/*
		 * Beware, since the only have SI controller we
		 * have has a 9300 instead of a 9766, we map the
		 * drive type into the 9300.  This means that
		 * on a 9766 you lose the last 8 cylinders (argh).
		 */
		case SI9766:
			return (HPDT_9300);

		case SI9762:
			return (HPDT_RM03);

		case SICAPD:
			return (HPDT_CAP);

		case SI9751D:
			return (HPDT_EAGLE);
		}
		return (type);
	}
	/*
	 * RM03: EMULEX controller.  Map to correct
	 * drive type by checking the holding
	 * register for the disk geometry.
	 */
	if (type == HPDT_RM02) {
		int newtype = type;

		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXTRAK;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		if (ntracks == 16) {
			newtype = HPDT_CAP;	/* AMPEX capricorn */
			goto done;
		}
		if (ntracks == 19) {
			newtype = HPDT_9300;	/* AMPEX 9300 */
			goto done;
		}
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXSECT;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		if (ntracks == 48) {
			newtype = HPDT_EAGLE;	/* 48 sector Eagle */
			goto done;
		}
		printf("RM02 with %d sectors/track?\n", ntracks);
	done:
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		return (newtype);
	}
	/*
	 * ML11's all map to the same type.
	 */
	if (type == HPDT_ML11A || type == HPDT_ML11B)
		return (HPDT_ML11A);
	return (type);
}
