#ifndef lint
static	char *sccsid = "@(#)parties.c	1.2 83/07/20";
#endif

#include "externs.h"

meleeing(from, to)
struct ship *from;
register struct ship *to;
{
	register struct BP *p = from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent && p->toship == to)
			return 1;
	return 0;
}

boarding(from, isdefense)
register struct ship *from;
char isdefense;
{
	register struct BP *p = isdefense ? from->file->DBP : from->file->OBP;
	register struct BP *q = p + NBP;

	for (; p < q; p++)
		if (p->turnsent)
			return 1;
	return 0;
}

Snagged(ship, isgrap)
struct ship *ship;
char isgrap;
{
	register int Snags = 0;
	register struct snag *sp, *sq;

	sp = isgrap ? ship->file->grapples : ship->file->fouls;
	sq = sp + NSHIP;
	for (; sp < sq; sp++)
		if (sp->turnfoul)
			Snags++;
	return Snags;
}

Snagged2(ship, to, isgrap, isX)
struct ship *ship, *to;
char isgrap, isX;
{
	register Snags = 0;
	register struct snag *sp, *sq;

	sp = isgrap ? ship->file->grapples : ship->file->fouls;
	sq = sp + NSHIP;
	for (; sp < sq; sp++) {
		if (sp->turnfoul && sp->toship == to
		    && (!isX || sp->turnfoul < turn - 1
				&& ship->file->loadwith==L_GRAPE))
			Snags++;
	}
	return Snags;
}

unboard(ship, to, isdefense)
register struct ship *ship, *to;
register char isdefense;
{
	register struct BP *p = isdefense ? ship->file->DBP : ship->file->OBP;
	register n;

	for (n = 0; n < NBP; p++, n++)
		if (p->turnsent && (p->toship == to || isdefense || ship == to))
			Write(isdefense ? W_DBP : W_OBP, ship, 0, n, 0, 0, 0);
}
