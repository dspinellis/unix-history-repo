/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)win.c	5.5 (Berkeley) 6/26/90";
#endif /* not lint */

# include	"trek.h"
# include	"getpar.h"
# include	<setjmp.h>

/*
**  Signal game won
**
**	This routine prints out the win message, arranges to print out
**	your score, tells you if you have a promotion coming to you,
**	cleans up the current input line, and arranges to have you
**	asked whether or not you want another game (via the longjmp()
**	call).
**
**	Pretty straightforward, although the promotion algorithm is
**	pretty off the wall.
*/

win()
{
	long			s;
	extern jmp_buf		env;
	extern long		score();
	extern struct cvntab	Skitab[];
	register struct cvntab	*p;

	sleep(1);
	printf("\nCongratulations, you have saved the Federation\n");
	Move.endgame = 1;

	/* print and return the score */
	s = score();

	/* decide if she gets a promotion */
	if (Game.helps == 0 && Game.killb == 0 && Game.killinhab == 0 && 5 * Game.kills + Game.deaths < 100 &&
			s >= 1000 && Ship.ship == ENTERPRISE)
	{
		printf("In fact, you are promoted one step in rank,\n");
		if (Game.skill >= 6)
			printf("to the exalted rank of Commodore Emeritus\n");
		else
		{
			p = &Skitab[Game.skill - 1];
			printf("from %s%s ", p->abrev, p->full);
			p++;
			printf("to %s%s\n", p->abrev, p->full);
		}
	}

	/* clean out input, and request new game */
	skiptonl(0);
	longjmp(env, 1);
}
