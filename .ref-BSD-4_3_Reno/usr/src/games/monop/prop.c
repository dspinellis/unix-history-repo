/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)prop.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"monop.ext"

extern char *calloc();

/*
 *	This routine deals with buying property, setting all the
 * appropriate flags.
 */
buy(player, sqrp)
reg int		player;
reg SQUARE	*sqrp; {

	trading = FALSE;
	sqrp->owner = player;
	add_list(player, &(play[player].own_list), cur_p->loc);
}
/*
 *	This routine adds an item to the list.
 */
add_list(plr, head, op_sqr)
int	plr;
OWN	**head;
int	op_sqr; {

	reg int	val;
	reg OWN	*tp, *last_tp;
	MON	*mp;
	OWN	*op;

	op = (OWN *)calloc(1, sizeof (OWN));
	op->sqr = &board[op_sqr];
	val = value(op->sqr);
	last_tp = NULL;
	for (tp = *head; tp && value(tp->sqr) < val; tp = tp->next)
		if (val == value(tp->sqr)) {
			cfree(op);
			return;
		}
		else
			last_tp = tp;
	op->next = tp;
	if (last_tp != NULL)
		last_tp->next = op;
	else
		*head = op;
	if (!trading)
		set_ownlist(plr);
}
/*
 *	This routine deletes property from the list.
 */
del_list(plr, head, op_sqr)
int	plr;
OWN	**head;
shrt	op_sqr; {

	reg int	i;
	reg OWN	*op, *last_op;

	switch (board[op_sqr].type) {
	  case PRPTY:
		board[op_sqr].desc->mon_desc->num_own--;
		break;
	  case RR:
		play[plr].num_rr--;
		break;
	  case UTIL:
		play[plr].num_util--;
		break;
	}
	last_op = NULL;
	for (op = *head; op; op = op->next)
		if (op->sqr == &board[op_sqr])
			break;
		else
			last_op = op;
	if (last_op == NULL)
		*head = op->next;
	else {
		last_op->next = op->next;
		cfree(op);
	}
}
/*
 *	This routine calculates the value for sorting of the
 * given square.
 */
value(sqp)
reg SQUARE	*sqp; {

	reg int	sqr;

	sqr = sqnum(sqp);
	switch (sqp->type) {
	  case SAFE:
		return 0;
	  default:		/* Specials, etc */
		return 1;
	  case UTIL:
		if (sqr == 12)
			return 2;
		else
			return 3;
	  case RR:
		return 4 + sqr/10;
	  case PRPTY:
		return 8 + (sqp->desc) - prop;
	}
}
/*
 *	This routine accepts bids for the current peice
 * of property.
 */
bid() {

	static bool	in[MAX_PL];
	reg int		i, num_in, cur_max;
	char		buf[80];
	int		cur_bid;

	printf("\nSo it goes up for auction.  Type your bid after your name\n");
	for (i = 0; i < num_play; i++)
		in[i] = TRUE;
	i = -1;
	cur_max = 0;
	num_in = num_play;
	while (num_in > 1 || (cur_max == 0 && num_in > 0)) {
		i = ++i % num_play;
		if (in[i]) {
			do {
				(void)sprintf(buf, "%s: ", name_list[i]);
				cur_bid = get_int(buf);
				if (cur_bid == 0) {
					in[i] = FALSE;
					if (--num_in == 0)
						break;
				}
				else if (cur_bid <= cur_max) {
					printf("You must bid higher than %d to stay in\n", cur_max);
					printf("(bid of 0 drops you out)\n");
				}
			} while (cur_bid != 0 && cur_bid <= cur_max);
			cur_max = (cur_bid ? cur_bid : cur_max);
		}
	}
	if (cur_max != 0) {
		while (!in[i])
			i = ++i % num_play;
		printf("It goes to %s (%d) for $%d\n",play[i].name,i+1,cur_max);
		buy(i, &board[cur_p->loc]);
		play[i].money -= cur_max;
	}
	else
		printf("Nobody seems to want it, so we'll leave it for later\n");
}
/*
 *	This routine calculates the value of the property
 * of given player.
 */
prop_worth(plp)
reg PLAY	*plp; {

	reg OWN	*op;
	reg int	worth;

	worth = 0;
	for (op = plp->own_list; op; op = op->next) {
		if (op->sqr->type == PRPTY && op->sqr->desc->monop)
			worth += op->sqr->desc->mon_desc->h_cost * 50 *
			    op->sqr->desc->houses;
		worth += op->sqr->cost;
	}
	return worth;
}
