/*
 * Copyright (c) 1980 Regents of the University of California.
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
static char sccsid[] = "@(#)lose.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"

/*
**  PRINT OUT LOSER MESSAGES
**
**	The messages are printed out, the score is computed and
**	printed, and the game is restarted.  Oh yeh, any special
**	actions which need be taken are taken.
*/

char	*Losemsg[] =
{
	"You ran out of time",
	"You ran out of energy",
	"You have been destroyed",
	"You ran into the negative energy barrier",
	"You destroyed yourself by nova'ing that star",
	"You have been caught in a supernova",
	"You just suffocated in outer space",
	"You could not be rematerialized",
	"\n\032\014 ***\07 Ship's hull has imploded\07 ***",
	"You have burned up in a star",
	"Well, you destroyed yourself, but it didn't do any good",
	"You have been captured by Klingons and mercilessly tortured",
	"Your last crew member died",
};

lose(why)
int	why;
{
	Game.killed = 1;
	sleep(1);
	printf("\n%s\n", Losemsg[why - 1]);
	switch (why)
	{

	  case L_NOTIME:
		Game.killed = 0;
		break;
	}
	Move.endgame = -1;
	score();
	skiptonl(0);
	reset();
}
