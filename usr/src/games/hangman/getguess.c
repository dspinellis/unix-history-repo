# include	"hangman.h"

/*
 * getguess:
 *	Get another guess
 */
getguess()
{
	register int	i;
	register int	ch;
	register bool	correct;

	leaveok(stdscr, FALSE);
	for (;;) {
		move(PROMPTY, PROMPTX + sizeof "Guess: ");
		refresh();
		ch = readch();
		if (isalpha(ch)) {
			if (isupper(ch))
				ch = tolower(ch);
			if (Guessed[ch - 'a'])
				mvprintw(MESGY, MESGX, "Already guessed '%c'", ch);
			else
				break;
		}
		else if (ch == CTRL(D))
			die();
		else
			mvprintw(MESGY, MESGX, "Not a valid guess: '%s'",
				unctrl(ch));
	}
	leaveok(stdscr, TRUE);
	move(MESGY, MESGX);
	clrtoeol();

	Guessed[ch - 'a'] = TRUE;
	correct = FALSE;
	for (i = 0; Word[i] != '\0'; i++)
		if (Word[i] == ch) {
			Known[i] = ch;
			correct = TRUE;
		}
	if (!correct)
		Errors++;
}

/*
 * readch;
 *	Read a character from the input
 */
readch()
{
	register int	cnt, r;
	auto char	ch;

	cnt = 0;
	for (;;) {
		if (read(0, &ch, sizeof ch) <= 0)
		{
			if (++cnt > 100)
				die();
		}
		else if (ch == CTRL(L)) {
			wrefresh(curscr);
			mvcur(0, 0, curscr->_cury, curscr->_curx);
		}
		else
			return ch;
	}
}
