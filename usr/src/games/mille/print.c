# include	"mille.h"

/*
 * @(#)print.c	1.1 (Berkeley) 4/1/82
 */

# define	COMP_STRT	20
# define	CARD_STRT	2

prboard() {

	reg PLAY	*pp;
	reg int		i, j, k, temp;

	for (k = 0; k < 2; k++) {
		pp = &Player[k];
		temp = k * COMP_STRT + CARD_STRT;
		for (i = 0; i < NUM_SAFE; i++)
			if (pp->safety[i] == S_PLAYED && !pp->sh_safety[i]) {
				mvaddstr(i, temp, C_name[i + S_CONV]);
				if (pp->coups[i])
					mvaddch(i, temp - CARD_STRT, '*');
				pp->sh_safety[i] = TRUE;
			}
		show_card(14, temp, pp->battle, &pp->sh_battle);
		show_card(16, temp, pp->speed, &pp->sh_speed);
		for (i = C_25; i <= C_200; i++) {
			reg char	*name;
			reg int		end;

			if (pp->nummiles[i] == pp->sh_nummiles[i])
				continue;

			name = C_name[i];
			temp = k * 40;
			end = pp->nummiles[i];
			for (j = pp->sh_nummiles[i]; j < end; j++)
				mvwaddstr(Miles, i + 1, (j << 2) + temp, name);
			pp->sh_nummiles[i] = end;
		}
	}
	prscore(TRUE);
	temp = CARD_STRT;
	pp = &Player[PLAYER];
	for (i = 0; i < HAND_SZ; i++)
		show_card(i + 6, temp, pp->hand[i], &pp->sh_hand[i]);
	mvprintw(6, COMP_STRT + CARD_STRT, "%2d", Topcard - Deck);
	show_card(8, COMP_STRT + CARD_STRT, Discard, &Sh_discard);
	if (End == 1000) {
		move(EXT_Y, EXT_X);
		standout();
		addstr("Extension");
		standend();
	}
	wrefresh(Board);
	wrefresh(Miles);
	wrefresh(Score);
}

/*
 * show_card:
 *	Show the given card if it is different from the last one shown
 */
show_card(y, x, c, lc)
int		y, x;
register CARD	c, *lc;
{
	if (c == *lc)
		return;

	mvprintw(y, x, C_fmt, C_name[c]);
	*lc = c;
}

static char	Score_fmt[] = "%4d";

prscore(for_real)
reg bool	for_real; {

	reg PLAY	*pp;
	reg int		x;

	stdscr = Score;
	for (pp = Player; pp < &Player[2]; pp++) {
		x = (pp - Player) * 6 + 21;
		show_score(1, x, pp->mileage, &pp->sh_mileage);
		if (pp->safescore != pp->sh_safescore) {
			mvprintw(2, x, Score_fmt, pp->safescore);
			if (pp->safescore == 400)
				mvaddstr(3, x + 1, "300");
			else
				mvaddstr(3, x + 1, "  0");
			mvprintw(4, x, Score_fmt, pp->coupscore);
			pp->sh_safescore = pp->safescore;
		}
		if (Window == W_FULL || Finished) {
#ifdef EXTRAP
			if (for_real)
				finalscore(pp);
			else
				extrapolate(pp);
#else
			finalscore(pp);
#endif
			show_score(11, x, pp->hand_tot, &pp->sh_hand_tot);
			show_score(13, x, pp->total, &pp->sh_total);
			show_score(14, x, pp->games, &pp->sh_games);
		}
		else {
			show_score(6, x, pp->hand_tot, &pp->sh_hand_tot);
			show_score(8, x, pp->total, &pp->sh_total);
			show_score(9, x, pp->games, &pp->sh_games);
		}
	}
	stdscr = Board;
}

/*
 * show_score:
 *	Show a score value if it is different from the last time we
 *	showed it.
 */
show_score(y, x, s, ls)
int		y, x;
register int	s, *ls;
{
	if (s == *ls)
		return;

	mvprintw(y, x, Score_fmt, s);
	*ls = s;
}
