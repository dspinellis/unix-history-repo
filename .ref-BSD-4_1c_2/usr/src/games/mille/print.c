#ifndef lint
static char sccsid[] = "@(#)print.c	4.1 12/24/82";
#endif

# include	"mille.h"

# define	COMP_STRT	20
# define	CARD_STRT	2

prboard() {

	reg PLAY	*pp;
	reg int		i, j, k, temp;

	for (k = 0; k < 2; k++) {
		pp = &Player[k];
		temp = k * COMP_STRT + CARD_STRT;
		for (i = 0; i < NUM_SAFE; i++)
			if (pp->safety[i] == S_PLAYED) {
				mvaddstr(i, temp, C_name[i + S_CONV]);
				if (pp->coups[i])
					mvaddch(i, temp - CARD_STRT, '*');
			}
		mvprintw(14, temp, C_fmt, C_name[pp->battle]);
		mvprintw(16, temp, C_fmt, C_name[pp->speed]);
		for (i = C_25; i <= C_200; ) {
			reg char	*name;
			reg int		end;

			name = C_name[i];
			temp = k * 40;
			end = pp->nummiles[i++];
			for (j = 0; j < end; j++)
				mvwaddstr(Miles, i, (j << 2) + temp, name);
		}
	}
	prscore(TRUE);
	temp = CARD_STRT;
	pp = &Player[PLAYER];
	for (i = 0; i < HAND_SZ; i++)
		mvprintw(i + 6, temp, C_fmt, C_name[pp->hand[i]]);
	mvprintw(6, COMP_STRT + CARD_STRT, "%2d", Topcard - Deck);
	mvprintw(8, COMP_STRT + CARD_STRT, C_fmt, C_name[Discard]);
	if (End == 1000) {
		static char	ext[] = "Extension";

		stand(EXT_Y, EXT_X, ext);
	}
	wrefresh(Board);
	wrefresh(Miles);
	wrefresh(Score);
}

/*
 *	Put str at (y,x) in standout mode
 */
stand(y, x, str)
reg int		y, x;
reg char	*str; {

	standout();
	mvaddstr(y, x, str);
	standend();
	return TRUE;
}

prscore(for_real)
reg bool	for_real; {

	reg PLAY	*pp;
	reg int		x;
	reg char	*Score_fmt = "%4d";

	stdscr = Score;
	for (pp = Player; pp < &Player[2]; pp++) {
		x = (pp - Player) * 6 + 21;
		mvprintw(1, x, Score_fmt, pp->mileage);
		mvprintw(2, x, Score_fmt, pp->safescore);
		if (pp->safescore == 400)
			mvaddstr(3, x + 1, "300");
		else
			mvaddch(3, x + 3, '0');
		mvprintw(4, x, Score_fmt, pp->coupscore);
		if (Window == W_FULL || Finished) {
#ifdef EXTRAP
			if (for_real)
				finalscore(pp);
			else
				extrapolate(pp);
#else
			finalscore(pp);
#endif
			mvprintw(11, x, Score_fmt, pp->hand_tot);
			mvprintw(13, x, Score_fmt, pp->total);
			mvprintw(14, x, Score_fmt, pp->games);
		}
		else {
			mvprintw(6, x, Score_fmt, pp->hand_tot);
			mvprintw(8, x, Score_fmt, pp->total);
			mvprintw(9, x, Score_fmt, pp->games);
		}
	}
	stdscr = Board;
}
