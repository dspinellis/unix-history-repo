static char	*sccsid = "@(#)crib.c	5.1 (Berkeley) %G%";

# include	<curses.h>
# include	<signal.h>
# include	"deck.h"
# include	"cribbage.h"
# include	"cribcur.h"


# define	LOGFILE		"/usr/games/lib/criblog"
# define	INSTRCMD	"ul /usr/games/lib/crib.instr | more -f"


main(argc, argv)
int	argc;
char	*argv[];
{
	register  char		*p;
	BOOLEAN			playing;
	char			*s;		/* for reading arguments */
	char			bust;		/* flag for arg reader */
	FILE			*f;
	FILE			*fopen();
	char			*getline();
	int			bye();

	while (--argc > 0) {
	    if ((*++argv)[0] != '-') {
		fprintf(stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n");
		exit(1);
	    }
	    bust = FALSE;
	    for (s = argv[0] + 1; *s != NULL; s++) {
		switch (*s) {
		    case 'e':
			explain = TRUE;
			break;
		    case 'q':
			quiet = TRUE;
			break;
		    case 'r':
			rflag = TRUE;
			break;
		    default:
			fprintf(stderr, "\n\ncribbage: usage is 'cribbage [-eqr]'\n");
			exit(2);
			break;
		}
		if (bust)
		    break;
	    }
	}

	initscr();
	signal(SIGINT, bye);
	crmode();
	noecho();
	Playwin = subwin(stdscr, PLAY_Y, PLAY_X, 0, 0);
	Tablewin = subwin(stdscr, TABLE_Y, TABLE_X, 0, PLAY_X);
	Compwin = subwin(stdscr, COMP_Y, COMP_X, 0, TABLE_X + PLAY_X);
	Msgwin = subwin(stdscr, MSG_Y, MSG_X, Y_MSG_START, SCORE_X + 1);
	leaveok(Playwin, TRUE);
	leaveok(Tablewin, TRUE);
	leaveok(Compwin, TRUE);
	clearok(stdscr, FALSE);

	if (!quiet) {
	    msg("Do you need instructions for cribbage? ");
	    if (getuchar() == 'Y') {
		endwin();
		fflush(stdout);
		system(INSTRCMD);
		crmode();
		noecho();
		clear();
		refresh();
		msg("For the rules of this program, do \"man cribbage\"");
	    }
	}
	playing = TRUE;
	do {
	    wclrtobot(Msgwin);
	    msg(quiet ? "L or S? " : "Long (to 121) or Short (to 61)? ");
	    if (glimit == SGAME)
		glimit = (getuchar() == 'L' ? LGAME : SGAME);
	    else
		glimit = (getuchar() == 'S' ? SGAME : LGAME);
	    game();
	    msg("Another game? ");
	    playing = (getuchar() == 'Y');
	} while (playing);

	if ((f = fopen(LOGFILE, "a")) != NULL) {
	    fprintf(f, "Won %5.5d, Lost %5.5d\n",  cgames, pgames);
	    fclose(f);
	}

	bye();
}

/*
 * makeboard:
 *	Print out the initial board on the screen
 */
makeboard()
{
    mvaddstr(SCORE_Y + 0, SCORE_X, "+---------------------------------------+");
    mvaddstr(SCORE_Y + 1, SCORE_X, "|  Score:   0     YOU                   |");
    mvaddstr(SCORE_Y + 2, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 3, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 4, SCORE_X, "|                                       |");
    mvaddstr(SCORE_Y + 5, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 6, SCORE_X, "| *.....:.....:.....:.....:.....:.....  |");
    mvaddstr(SCORE_Y + 7, SCORE_X, "|  Score:   0      ME                   |");
    mvaddstr(SCORE_Y + 8, SCORE_X, "+---------------------------------------+");
    gamescore();
}

/*
 * gamescore:
 *	Print out the current game score
 */
gamescore()
{
    extern int	Lastscore[];

    if (pgames || cgames) {
	    mvprintw(SCORE_Y + 1, SCORE_X + 28, "Games: %3d", pgames);
	    mvprintw(SCORE_Y + 7, SCORE_X + 28, "Games: %3d", cgames);
    }
    Lastscore[0] = -1;
    Lastscore[1] = -1;
}

/*
 * game:
 *	Play one game up to glimit points.  Actually, we only ASK the
 *	player what card to turn.  We do a random one, anyway.
 */
game()
{
	register int		i, j;
	BOOLEAN			flag;
	BOOLEAN			compcrib;

	makeboard();
	refresh();
	makedeck(deck);
	shuffle(deck);
	if (gamecount == 0) {
	    flag = TRUE;
	    do {
		if (!rflag) {				/* player cuts deck */
		    msg(quiet ? "Cut for crib? " :
			"Cut to see whose crib it is -- low card wins? ");
		    getline();
		}
		i = (rand() >> 4) % CARDS;		/* random cut */
		do {					/* comp cuts deck */
		    j = (rand() >> 4) % CARDS;
		} while (j == i);
		addmsg(quiet ? "You cut " : "You cut the ");
		msgcard(deck[i], FALSE);
		endmsg();
		addmsg(quiet ? "I cut " : "I cut the ");
		msgcard(deck[j], FALSE);
		endmsg();
		flag = (deck[i].rank == deck[j].rank);
		if (flag) {
		    msg(quiet ? "We tied..." :
			"We tied and have to try again...");
		    shuffle(deck);
		    continue;
		}
		else
		    compcrib = (deck[i].rank > deck[j].rank);
	    } while (flag);
	}
	else {
	    werase(Tablewin);
	    wrefresh(Tablewin);
	    werase(Compwin);
	    wrefresh(Compwin);
	    msg("Loser (%s) gets first crib",  (iwon ? "you" : "me"));
	    compcrib = !iwon;
	}

	pscore = cscore = 0;
	flag = TRUE;
	do {
	    shuffle(deck);
	    flag = !playhand(compcrib);
	    compcrib = !compcrib;
	} while (flag);
	++gamecount;
	if (cscore < pscore) {
	    if (glimit - cscore > 60) {
		msg("YOU DOUBLE SKUNKED ME!");
		pgames += 4;
	    }
	    else if (glimit - cscore > 30) {
		msg("YOU SKUNKED ME!");
		pgames += 2;
	    }
	    else {
		msg("YOU WON!");
		++pgames;
	    }
	    iwon = FALSE;
	}
	else {
	    if (glimit - pscore > 60) {
		msg("I DOUBLE SKUNKED YOU!");
		cgames += 4;
	    }
	    else if (glimit - pscore > 30) {
		msg("I SKUNKED YOU!");
		cgames += 2;
	    }
	    else {
		msg("I WON!");
		++cgames;
	    }
	    iwon = TRUE;
	}
	gamescore();
}

/*
 * playhand:
 *	Do up one hand of the game
 */
playhand(mycrib)
BOOLEAN		mycrib;
{
	register int		deckpos;
	extern char		Msgbuf[];

	werase(Compwin);

	knownum = 0;
	deckpos = deal(mycrib);
	sorthand(chand, FULLHAND);
	sorthand(phand, FULLHAND);
	makeknown(chand, FULLHAND);
	prhand(phand, FULLHAND, Playwin, FALSE);
	discard(mycrib);
	if (cut(mycrib, deckpos))
	    return TRUE;
	if (peg(mycrib))
	    return TRUE;
	werase(Tablewin);
	wrefresh(Tablewin);
	if (score(mycrib))
	    return TRUE;
	return FALSE;
}



/*
 * deal cards to both players from deck
 */

deal( mycrib )
{
	register  int		i, j;

	j = 0;
	for( i = 0; i < FULLHAND; i++ )  {
	    if( mycrib )  {
		phand[i] = deck[j++];
		chand[i] = deck[j++];
	    }
	    else  {
		chand[i] = deck[j++];
		phand[i] = deck[j++];
	    }
	}
	return( j );
}

/*
 * discard:
 *	Handle players discarding into the crib...
 * Note: we call cdiscard() after prining first message so player doesn't wait
 */
discard(mycrib)
BOOLEAN		mycrib;
{
	register char	*prompt;
	CARD		crd;

	prcrib(mycrib, TRUE);
	prompt = (quiet ? "Discard --> " : "Discard a card --> ");
	cdiscard(mycrib);			/* puts best discard at end */
	crd = phand[infrom(phand, FULLHAND, prompt)];
	remove(crd, phand, FULLHAND);
	prhand(phand, FULLHAND, Playwin, FALSE);
	crib[0] = crd;
/* next four lines same as last four except for cdiscard() */
	crd = phand[infrom(phand, FULLHAND - 1, prompt)];
	remove(crd, phand, FULLHAND - 1);
	prhand(phand, FULLHAND, Playwin, FALSE);
	crib[1] = crd;
	crib[2] = chand[4];
	crib[3] = chand[5];
	chand[4].rank = chand[4].suit = chand[5].rank = chand[5].suit = EMPTY;
}

/*
 * cut:
 *	Cut the deck and set turnover.  Actually, we only ASK the
 *	player what card to turn.  We do a random one, anyway.
 */
cut(mycrib, pos)
BOOLEAN		mycrib;
int		pos;
{
	register int		i, cardx;
	BOOLEAN			win = FALSE;

	if (mycrib) {
	    if (!rflag) {			/* random cut */
		msg(quiet ? "Cut the deck? " :
			"How many cards down do you wish to cut the deck? ");
		getline();
	    }
	    i = (rand() >> 4) % (CARDS - pos);
	    turnover = deck[i + pos];
	    addmsg(quiet ? "You cut " : "You cut the ");
	    msgcard(turnover, FALSE);
	    endmsg();
	    if (turnover.rank == JACK) {
		msg("I get two for his heels");
		win = chkscr(&cscore,2 );
	    }
	}
	else {
	    i = (rand() >> 4) % (CARDS - pos) + pos;
	    turnover = deck[i];
	    addmsg(quiet ? "I cut " : "I cut the ");
	    msgcard(turnover, FALSE);
	    endmsg();
	    if (turnover.rank == JACK) {
		msg("You get two for his heels");
		win = chkscr(&pscore, 2);
	    }
	}
	makeknown(&turnover, 1);
	prcrib(mycrib, FALSE);
	return win;
}

/*
 * prcrib:
 *	Print out the turnover card with crib indicator
 */
prcrib(mycrib, blank)
BOOLEAN		mycrib, blank;
{
	register int	y, cardx;

	if (mycrib)
	    cardx = CRIB_X;
	else
	    cardx = 0;

	mvaddstr(CRIB_Y, cardx + 1, "CRIB");
	prcard(stdscr, CRIB_Y + 1, cardx, turnover, blank);

	if (mycrib)
	    cardx = 0;
	else
	    cardx = CRIB_X;

	for (y = CRIB_Y; y <= CRIB_Y + 5; y++)
	    mvaddstr(y, cardx, "       ");
}

/*
 * peg:
 *	Handle all the pegging...
 */

static CARD		Table[14];

static int		Tcnt;

peg(mycrib)
BOOLEAN		mycrib;
{
	static CARD		ch[CINHAND], ph[CINHAND];
	CARD			crd;
	register int		i, j, k;
	register int		l;
	register int		cnum, pnum, sum;
	register BOOLEAN	myturn, mego, ugo, last, played;

	cnum = pnum = CINHAND;
	for (i = 0; i < CINHAND; i++) {		/* make copies of hands */
	    ch[i] = chand[i];
	    ph[i] = phand[i];
	}
	Tcnt = 0;			/* index to table of cards played */
	sum = 0;			/* sum of cards played */
	mego = ugo = FALSE;
	myturn = !mycrib;
	for (;;) {
	    last = TRUE;				/* enable last flag */
	    prhand(ph, pnum, Playwin, FALSE);
	    prhand(ch, cnum, Compwin, TRUE);
	    prtable(sum);
	    if (myturn) {				/* my tyrn to play */
		if (!anymove(ch, cnum, sum)) {		/* if no card to play */
		    if (!mego && cnum) {		/* go for comp? */
			msg("GO");
			mego = TRUE;
		    }
		    if (anymove(ph, pnum, sum))		/* can player move? */
			myturn = !myturn;
		    else {				/* give him his point */
			msg(quiet ? "You get one" : "You get one point");
			if (chkscr(&pscore, 1))
			    return TRUE;
			sum = 0;
			mego = ugo = FALSE;
			Tcnt = 0;
		    }
		}
		else {
		    played = TRUE;
		    j = -1;
		    k = 0;
		    for (i = 0; i < cnum; i++) {	/* maximize score */
			l = pegscore(ch[i], Table, Tcnt, sum);
			if (l > k) {
			    k = l;
			    j = i;
			}
		    }
		    if (j < 0)				/* if nothing scores */
			j = cchose(ch, cnum, sum);
		    crd = ch[j];
		    remove(crd, ch, cnum--);
		    sum += VAL(crd.rank);
		    Table[Tcnt++] = crd;
		    if (k > 0) {
			addmsg(quiet ? "I get %d playing " :
			    "I get %d points playing ", k);
			msgcard(crd, FALSE);
			endmsg();
			if (chkscr(&cscore, k))
			    return TRUE;
		    }
		    myturn = !myturn;
		}
	    }
	    else {
		if (!anymove(ph, pnum, sum)) {		/* can player move? */
		    if (!ugo && pnum) {			/* go for player */
			msg("You have a GO");
			ugo = TRUE;
		    }
		    if (anymove(ch, cnum, sum))		/* can computer play? */
			myturn = !myturn;
		    else {
			msg(quiet ? "I get one" : "I get one point");
			do_wait();
			if (chkscr(&cscore, 1))
			    return TRUE;
			sum = 0;
			mego = ugo = FALSE;
			Tcnt = 0;
		    }
		}
		else {					/* player plays */
		    played = FALSE;
		    if (pnum == 1) {
			crd = ph[0];
			msg("You play your last card");
		    }
		    else
			for (;;) {
			    prhand(ph, pnum, Playwin, FALSE);
			    crd = ph[infrom(ph, pnum, "Your play: ")];
			    if (sum + VAL(crd.rank) <= 31)
				break;
			    else
				msg("Total > 31 -- try again");
			}
		    makeknown(&crd, 1);
		    remove(crd, ph, pnum--);
		    i = pegscore(crd, Table, Tcnt, sum);
		    sum += VAL(crd.rank);
		    Table[Tcnt++] = crd;
		    if (i > 0) {
			msg(quiet ? "You got %d" : "You got %d points", i);
			if (chkscr(&pscore, i))
			    return TRUE;
		    }
		    myturn = !myturn;
		}
	    }
	    if (sum >= 31) {
		if (!myturn)
		    do_wait();
		sum = 0;
		mego = ugo = FALSE;
		Tcnt = 0;
		last = FALSE;				/* disable last flag */
	    }
	    if (!pnum && !cnum)
		break;					/* both done */
	}
	prhand(ph, pnum, Playwin, FALSE);
	prhand(ch, cnum, Compwin, TRUE);
	prtable(sum);
	if (last)
	    if (played) {
		msg(quiet ? "I get one for last" : "I get one point for last");
		do_wait();
		if (chkscr(&cscore, 1))
		    return TRUE;
	    }
	    else {
		msg(quiet ? "You get one for last" :
			    "You get one point for last");
		if (chkscr(&pscore, 1))
		    return TRUE;
	    }
	return FALSE;
}

/*
 * prtable:
 *	Print out the table with the current score
 */
prtable(score)
int	score;
{
	prhand(Table, Tcnt, Tablewin, FALSE);
	mvwprintw(Tablewin, (Tcnt + 2) * 2, Tcnt + 1, "%2d", score);
	wrefresh(Tablewin);
}

/*
 * score:
 *	Handle the scoring of the hands
 */
score(mycrib)
BOOLEAN		mycrib;
{
	sorthand(crib, CINHAND);
	if (mycrib) {
	    if (plyrhand(phand, "hand"))
		return TRUE;
	    if (comphand(chand, "hand"))
		return TRUE;
	    do_wait();
	    if (comphand(crib, "crib"))
		return TRUE;
	}
	else {
	    if (comphand(chand, "hand"))
		return TRUE;
	    if (plyrhand(phand, "hand"))
		return TRUE;
	    if (plyrhand(crib, "crib"))
		return TRUE;
	}
	return FALSE;
}
