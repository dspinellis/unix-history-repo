/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)upmaptype.c	7.6 (Berkeley) %G%
 */

/*
 * UNIBUS peripheral standalone
 * driver: drive type mapping routine.
 */
#ifdef COMPAT_42
#include "sys/param.h" 
#include "sys/dkbad.h"
#include "sys/disklabel.h"
#include "sys/vmmac.h"

#include "../include/pte.h"
#include "../uba/upreg.h"
#include "../uba/ubareg.h"

#include "stand/saio.h"
#include "savax.h"

static short	up9300_off[] = { 0,  27,  0,  -1,  -1,  -1, 562, 82 };
static short	fj_off[]     = { 0,  50,  0,  -1,  -1,  -1, 155, -1 };
static short	upam_off[]   = { 0,  32,  0, 668, 723, 778, 668, 98 };
static short	up980_off[]  = { 0, 100,  0,  -1,  -1,  -1, 309, -1 };
static short	eagle_off[]  = { 0,  17,  0, 391, 408, 728, 391, 87 };

struct st upst[] = {
	32,	19,	32*19,	815,	up9300_off,	/* 9300 */
	32,	19,	32*19,	823,	up9300_off,	/* 9766 */
	32,	10,	32*10,	823,	fj_off,		/* Fuji 160 */
	32,	16,	32*16,	1024,	upam_off,	/* Capricorn */
	32,	5,	32*5,	823,	up980_off,	/* DM980 */
	48,	20,	48*20,	842,	eagle_off,	/* Fuji Eagle */
	0,	0,	0,	0,	0,
};

upmaptype(unit, upaddr, lp)
	int unit;
	register struct updevice *upaddr;
	register struct disklabel *lp;
{
	register struct st *st;
	register int i;
	int type = -1;

	upaddr->upcs1 = 0;
	upaddr->upcs2 = unit % 8;
	upaddr->uphr = UPHR_MAXTRAK;
	for (st = upst;; ++st) {
		if (!st->ntrak)
			return(0);
		if (upaddr->uphr == st->ntrak - 1) {
			type = st - upst;
			break;
		}
	}
	if (type == 0) {
		upaddr->uphr = UPHR_MAXCYL;
		if (upaddr->uphr == 822)	/* CDC 9766 */
			++type;
	}
	upaddr->upcs2 = UPCS2_CLR;
	st = &upst[type];

	/* set up a minimal disk label */
	lp->d_nsectors = st->nsect;
	lp->d_ntracks = st->ntrak;
	lp->d_secpercyl = st->nspc;
	lp->d_ncylinders = st->ncyl;
	lp->d_secperunit = st->nspc * st->ncyl;
	lp->d_npartitions = 8;
	for (i = 0; i < 8; i++)
		if (st->off[i] == -1)
			lp->d_partitions[i].p_size = 0;
		else {
			lp->d_partitions[i].p_offset = st->off[i] *
			    lp->d_secpercyl;
			lp->d_partitions[i].p_size = lp->d_secperunit;
		}
	return(1);
}
#endif /* COMPAT_42 */
