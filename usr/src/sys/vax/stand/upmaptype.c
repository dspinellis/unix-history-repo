/*	upmaptype.c	4.1	83/02/18	*/

/*
 * UNIBUS peripheral standalone
 * driver: drive type mapping routine.
 */

#include "../h/param.h" 
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dkbad.h"
#include "../h/vmmac.h"

#include "../vax/pte.h"
#include "../vaxuba/upreg.h"
#include "../vaxuba/ubareg.h"

#include "saio.h"
#include "savax.h"

short	up9300_off[] = { 0, 27, 68, -1, -1, -1, -1, 82 };
short	up9766_off[] = { 0, 27, 68, -1, -1, -1, -1, 82 };
short	fj_off[] = { 0, 50, 0, -1, -1, -1, -1, 155 };
/* this is called upam instead of am because hp.c has a similar array */
short	upam_off[] = { 0, 32, 0, 668, 723, 778, 668, 98 };

struct st upst[] = {
	32,	19,	32*19,	815,	up9300_off,	/* 9300 */
	32,	19,	32*19,	823,	up9766_off,	/* 9766 */
	32,	10,	32*10,	823,	fj_off,		/* Fuji 160 */
	32,	16,	32*16,	1024,	upam_off,	/* Capricorn */
	0,	0,	0,	0,	0,
};

upmaptype(unit, upaddr)
	int unit;
	register struct updevice *upaddr;
{
	register struct st *st;
	int type = -1;

	upaddr->upcs1 = 0;
	upaddr->upcs2 = unit % 8;
	upaddr->uphr = UPHR_MAXTRAK;
	for (st = upst; st->ntrak != 0; st++)
		if (upaddr->uphr == st->ntrak - 1) {
			type = st - st;
			break;
		}
	if (st->ntrak == 0)
		printf("up%d: uphr=%x\n", unit, upaddr->uphr);
	if (type == 0) {
		upaddr->uphr = UPHR_MAXCYL;
		if (upaddr->uphr == 822)	/* CDC 9766 */
			type++;
	}
	upaddr->upcs2 = UPCS2_CLR;
	return (type);
}
