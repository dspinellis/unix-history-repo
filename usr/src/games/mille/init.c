#ifndef lint
static char sccsid[] = "@(#)init.c	4.1 12/24/82";
#endif

# include	"mille.h"

init() {

	reg PLAY	*pp;
	reg int		i, j;
	reg CARD	card;

	for (j = 0; j < C_RIGHT_WAY; j++)
		Numseen[j] = 0;
	Numgos = 0;

	for (i = 0; i < 2; i++) {
		pp = &Player[i];
		pp->hand[0] = C_INIT;
		for (j = 0; j < NUM_SAFE; j++) {
			pp->safety[j] = S_UNKNOWN;
			pp->coups[j] = FALSE;
		}
		for (j = 1; j < HAND_SZ; j++) {
			pp->hand[j] = *--Topcard;
			if (i == COMP) {
				account(card = *Topcard);
				if (issafety(card))
					pp->safety[card - S_CONV] = S_IN_HAND;
			}
		}
		pp->mileage = 0;
		pp->hand_tot = 0;
		pp->safescore = 0;
		pp->coupscore = 0;
		pp->can_go = FALSE;
		pp->speed = C_INIT;
		pp->battle = C_INIT;
		pp->new_speed = FALSE;
		pp->new_battle = FALSE;
		for (j = 0; j < NUM_MILES; j++)
			pp->nummiles[j] = 0;
	}
	if (Order)
		sort(Player[PLAYER].hand);
	Discard = C_INIT;
	Finished = FALSE;
	End = 700;
}

shuffle() {

	reg int		i, r;
	reg CARD	temp;

	for (i = 0; i < DECK_SZ; i++) {
		r = roll(1, DECK_SZ) - 1;
		if (r < 0 || r > DECK_SZ - 1) {
			fprintf(stderr, "shuffle: card no. error: %d\n", r);
			die();
		}
		temp = Deck[r];
		Deck[r] = Deck[i];
		Deck[i] = temp;
	}
	Topcard = &Deck[DECK_SZ];
}

newboard() {

	werase(Board);
	werase(Score);
	mvaddstr(5, 0, "--HAND--");
	mvaddch(6, 0, 'P');
	mvaddch(7, 0, '1');
	mvaddch(8, 0, '2');
	mvaddch(9, 0, '3');
	mvaddch(10, 0, '4');
	mvaddch(11, 0, '5');
	mvaddch(12, 0, '6');
	mvaddstr(13, 0, "--BATTLE--");
	mvaddstr(15, 0, "--SPEED--");
	mvaddstr(5, 20, "--DECK--");
	mvaddstr(7, 20, "--DISCARD--");
	mvaddstr(13, 20, "--BATTLE--");
	mvaddstr(15, 20, "--SPEED--");
	wmove(Miles, 0, 0);
	if (winch(Miles) != '-') {
		werase(Miles);
		mvwaddstr(Miles, 0, 0, "--MILEAGE--");
		mvwaddstr(Miles, 0, 41, "--MILEAGE--");
	}
	else {
		wmove(Miles, 1, 0);
		wclrtobot(Miles);
	}
	newscore();
	stdscr = Board;
}

newscore() {

	reg int	i;

	stdscr = Score;
	move(0, 22);
	if (inch() != 'Y') {
		erase();
		mvaddstr(0, 22,  "You   Comp   Value");
		mvaddstr(1, 2, "Milestones Played");
		mvaddstr(2, 8, "Each Safety");
		mvaddstr(3, 5, "All 4 Safeties");
		mvaddstr(4, 3, "Each Coup Fourre");
		mvaddstr(2, 37, "100");
		mvaddstr(3, 37, "300");
		mvaddstr(4, 37, "300");
	}
	else {
		move(5, 1);
		clrtobot();
	}
	for (i = 0; i < SCORE_Y; i++)
		mvaddch(i, 0, '|');
	move(SCORE_Y - 1, 1);
	while (addch('_') != ERR)
		continue;
	if (Window == W_FULL || Finished) {
		mvaddstr(5, 5, "Trip Completed");
		mvaddstr(6, 10, "Safe Trip");
		mvaddstr(7, 5, "Delayed Action");
		mvaddstr(8, 10, "Extension");
		mvaddstr(9, 11, "Shut-Out");
		mvaddstr(10, 21, "----   ----   -----");
		mvaddstr(11, 9, "Hand Total");
		mvaddstr(12, 20, "-----  -----");
		mvaddstr(13, 6, "Overall Total");
		mvaddstr(14, 15, "Games");
		mvaddstr(5, 37, "400");
		mvaddstr(6, 37, "300");
		mvaddstr(7, 37, "300");
		mvaddstr(8, 37, "200");
		mvaddstr(9, 37, "500");
	}
	else {
		mvaddstr(5, 21, "----   ----   -----");
		mvaddstr(6, 9, "Hand Total");
		mvaddstr(7, 20, "-----  -----");
		mvaddstr(8, 6, "Overall Total");
		mvaddstr(9, 15, "Games");
		mvaddstr(11, 2, "p: pick");
		mvaddstr(12, 2, "u: use #");
		mvaddstr(13, 2, "d: discard #");
		mvaddstr(14, 2, "w: toggle window");
		mvaddstr(11, 21, "q: quit");
		mvaddstr(12, 21, "o: order hand");
		mvaddstr(13, 21, "s: save");
		mvaddstr(14, 21, "r: reprint");
	}
	stdscr = Board;
}
