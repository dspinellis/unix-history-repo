#ifndef lint
static char sccsid[] = "@(#)misc.c	4.2 2/26/83";
#endif

#include	"mille.h"
#include	"unctrl.h"
#define	NUMSAFE	4

/* VARARGS1 */
error(str, arg)
char	*str;
{
	stdscr = Score;
	mvprintw(ERR_Y, ERR_X, str, arg);
	clrtoeol();
	putchar('\007');
	refresh();
	stdscr = Board;
	return FALSE;
}

CARD
getcard()
{
	reg char	c, c1;

	for (;;) {
		while ((c = getch()) == '\n' || c == '\r' || c == ' ')
			continue;
		if (islower(c))
			c = toupper(c);
		if (c == _tty.sg_kill || c == _tty.sg_erase)
			return -1;
		addstr(unctrl(c));
		clrtoeol();
		switch (c) {
		  case '1':	case '2':	case '3':
		  case '4':	case '5':	case '6':
			c -= '0';
			break;
		  case '0':	case 'P':	case 'p':
			c = 0;
			break;
		  default:
			putchar('\007');
			addch('\b');
			if (!isprint(c))
				addch('\b');
			c = -1;
			break;
		}
		refresh();
		if (c >= 0) {
			while ((c1=getch()) != '\r' && c1 != '\n' && c1 != ' ')
				if (c1 == _tty.sg_kill)
					return -1;
				else if (c1 == _tty.sg_erase) {
					addch('\b');
					clrtoeol();
					refresh();
					goto cont;
				}
				else
					write(0, "\007", 1);
			return c;
		}
cont:		;
	}
}

check_ext(forcomp)
reg bool	forcomp; {


	if (End == 700)
		if (Play == PLAYER) {
			if (getyn("Extension? ")) {
extend:
				if (!forcomp)
					End = 1000;
				return TRUE;
			}
			else {
done:
				if (!forcomp)
					Finished = TRUE;
				return FALSE;
			}
		}
		else {
			reg PLAY	*pp, *op;
			reg int		i, safe, miles;

			pp = &Player[COMP];
			op = &Player[PLAYER];
			for (safe = 0, i = 0; i < NUMSAFE; i++)
				if (pp->safety[i] != S_UNKNOWN)
					safe++;
			if (safe < 2)
				goto done;
			if (op->mileage == 0 || onecard(op)
			    || (op->can_go && op->mileage >= 500))
				goto done;
			for (miles = 0, i = 0; i < NUMSAFE; i++)
				if (op->safety[i] != S_PLAYED
				    && pp->safety[i] == S_UNKNOWN)
					miles++;
			if (miles + safe == NUMSAFE)
				goto extend;
			for (miles = 0, i = 0; i < HAND_SZ; i++)
				if ((safe = pp->hand[i]) <= C_200)
					miles += Value[safe]; 
			if (miles + (Topcard - Deck) * 3 > 1000)
				goto extend;
			goto done;
		}
	else
		goto done;
}

/*
 *	Get a yes or no answer to the given question.  Saves are
 * also allowed.  Return TRUE if the answer was yes, FALSE if no.
 */
getyn(prompt)
reg char	*prompt; {

	reg char	c;

	Saved = FALSE;
	for (;;) {
		leaveok(Board, FALSE);
		mvaddstr(MOVE_Y, MOVE_X, prompt);
		clrtoeol();
		refresh();
		switch (c = getch()) {
		  case 'n':	case 'N':
			addch('N');
			refresh();
			leaveok(Board, TRUE);
			return FALSE;
		  case 'y':	case 'Y':
			addch('Y');
			refresh();
			leaveok(Board, TRUE);
			return TRUE;
		  case 's':	case 'S':
			addch('S');
			refresh();
			Saved = save();
			continue;
		  default:
			addstr(unctrl(c));
			refresh();
			putchar('');
			break;
		}
	}
}

/*
 *	Check to see if more games are desired.  If not, and game
 * came from a saved file, make sure that they don't want to restore
 * it.  Exit appropriately.
 */
check_more() {

	raw();	/* Flush input */
	noraw();

	On_exit = TRUE;
	if (Player[PLAYER].total >= 5000 || Player[COMP].total >= 5000)
		if (getyn("Another game? "))
			return;
		else {
			/*
			 * must do accounting normally done in main()
			 */
			if (Player[PLAYER].total > Player[COMP].total)
				Player[PLAYER].games++;
			else if (Player[PLAYER].total < Player[COMP].total)
				Player[COMP].games++;
			Player[COMP].total = 0;
			Player[PLAYER].total = 0;
		}
	else
		if (getyn("Another hand? "))
			return;
	if (!Saved && getyn("Save game? "))
		if (!save())
			return;
	die();
}
