/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)jail.c	5.2 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"monop.ext"

/*
 *	This routine uses a get-out-of-jail-free card to get the
 * player out of jail.
 */
card() {

	if (cur_p->loc != JAIL) {
		printf("But you're not IN Jail\n");
		return;
	}
	if (cur_p->num_gojf == 0) {
		printf("But you don't HAVE a get out of jail free card\n");
		return;
	}
	ret_card(cur_p);
	cur_p->loc = 10;			/* just visiting	*/
	cur_p->in_jail = 0;
}
/*
 *	This routine returns the players get-out-of-jail-free card
 * to a deck.
 */
ret_card(plr)
reg PLAY	*plr; {

	plr->num_gojf--;
	if (CC_D.gojf_used)
		CC_D.gojf_used = FALSE;
	else
		CH_D.gojf_used = FALSE;
}
/*
 *	This routine deals with paying your way out of jail.
 */
pay() {

	if (cur_p->loc != JAIL) {
		printf("But you're not IN Jail\n");
		return;
	}
	cur_p->loc = 10;
	cur_p->money -= 50;
	cur_p->in_jail = 0;
	printf("That cost you $50\n");
}
/*
 *	This routine deals with a move in jail
 */
move_jail(r1, r2)
reg int	r1, r2; {

	if (r1 != r2) {
		printf("Sorry, that doesn't get you out\n");
		if (++(cur_p->in_jail) == 3) {
			printf("It's your third turn and you didn't roll doubles.  You have to pay $50\n");
			cur_p->money -= 50;
moveit:
			cur_p->loc = 10;
			cur_p->in_jail = 0;
			move(r1+r2);
			r1 = r2 - 1;	/* kludge: stop new roll w/doub	*/
			return TRUE;
		}
		return FALSE;
	}
	else {
		printf("Double roll gets you out.\n");
		goto moveit;
	}
}
printturn() {

	if (cur_p->loc != JAIL)
		return;
	printf("(This is your ");
	switch (cur_p->in_jail) {
	  case 0:
		printf("1st");
		break;
	  case 1:
		printf("2nd");
		break;
	  case 2:
		printf("3rd (and final)");
		break;
	}
	printf(" turn in JAIL)\n");
}
