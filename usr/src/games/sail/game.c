/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)game.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "externs.h"

maxturns(ship, af)
register struct ship *ship;
char *af;
{
	register int turns;

	turns = ship->specs->ta;
	if (*af = (ship->file->drift > 1 && turns)) {
		turns--;
		if (ship->file->FS == 1)
			turns = 0;
	}
	return turns;
}

maxmove(ship, dir, fs)
register struct ship *ship;
int dir, fs;
{
	register int riggone = 0, Move, flank = 0;

	Move = ship->specs->bs;
	if (!ship->specs->rig1)
		riggone++;
	if (!ship->specs->rig2)
		riggone++;
	if (!ship->specs->rig3)
		riggone++;
	if (!ship->specs->rig4)
		riggone++;
	if ((ship->file->FS || fs) && fs != -1) {
		flank = 1;
		Move = ship->specs->fs;
	}
	if (dir == winddir)
		Move -= 1 + WET[windspeed][ship->specs->class-1].B;
	else if (dir == winddir + 2 || dir == winddir - 2 || dir == winddir - 6 || dir == winddir + 6)
		Move -= 1 + WET[windspeed][ship->specs->class-1].C;
	else if (dir == winddir + 3 || dir == winddir - 3 || dir == winddir - 5 || dir == winddir + 5)
		Move = (flank ? 2 : 1) - WET[windspeed][ship->specs->class-1].D;
	else if (dir == winddir + 4 || dir == winddir - 4)
		Move = 0;
	else 
		Move -= WET[windspeed][ship->specs->class-1].A;
	Move -= riggone;
	Move = Move < 0 ? 0 : Move;
	return(Move);
}
