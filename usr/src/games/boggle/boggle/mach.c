/* vi: set tabstop=4 : */

/*
 * Terminal interface
 *
 * Input is raw and unechoed
 */

#ifdef unix
#include <sgtty.h>
#endif
#include <signal.h>
#include <curses.h>
#include <ctype.h>
#include <stdio.h>

#include "bog.h"

#ifdef ATARI
#include <osbind.h>
#endif

static int ccol, crow, maxw;
static int colstarts[MAXCOLS], ncolstarts;
static int lastline;
int ncols, nlines;

static int cont_catcher(), prwidth(), prword(), stop_catcher(), tty_cleanup();
static int tty_setup(), tty_showboard();
static int winch_catcher();

extern int ngames, nmwords, npwords, tnmwords, tnpwords;
extern char *pword[], *mword[];

/*
 * Do system dependent initialization
 * This is called once, when the program starts
 */
setup(sflag, seed)
int sflag;
long seed;
{
	extern int debug;

	if (tty_setup() < 0)
		return(-1);

	if (!sflag)
#ifdef ATARI
		seed = Random();
#else
		time(&seed);
#endif
	srandom(seed);
	if (debug)
		(void) printf("seed = %ld\n", seed);
	return(0);
}

/*
 * Do system dependent clean up
 * This is called once, just before the program terminates
 */
cleanup()
{

	tty_cleanup();
}

/*
 * Display the player's word list, the list of words not found, and the running
 * stats
 */
results()
{
	int col, row;
    int denom1, denom2;

    move(LIST_LINE, LIST_COL);
	clrtobot();
    printw("Words you found (%d):", npwords);
	refresh();
    move(LIST_LINE + 1, LIST_COL);
    prtable(pword, npwords, 0, ncols, prword, prwidth);

	getyx(stdscr, row, col);
    move(row + 1, col);
    printw("Words you missed (%d):", nmwords);
	refresh();
    move(row + 2, col);
    prtable(mword, nmwords, 0, ncols, prword, prwidth);

    denom1 = npwords + nmwords;
    denom2 = tnpwords + tnmwords;
 
    move(SCORE_LINE, SCORE_COL);
    printw("Percentage: %0.2f%% (%0.2f%% over %d game%s)\n",
        denom1 ? (100.0 * npwords) / (double) (npwords + nmwords) : 0.0,
        denom2 ? (100.0 * tnpwords) / (double) (tnpwords + tnmwords) : 0.0,
        ngames, ngames > 1 ? "s" : "");
}

static
prword(base, index)
char **base;
int index;
{
 
    printw("%s", base[index]);
}

static
prwidth(base, index)
char **base;
int index;
{

    return(strlen(base[index]));
}

/*
 * Main input routine
 *
 * - doesn't accept words longer than MAXWORDLEN or containing caps
 */
char *
getline(q)
char *q;
{
	register int ch, done;
	register char *p;
	int row, col;

	p = q;
	done = 0;
	while (!done) {
		ch = rawch();
		switch (ch) {
		case '\n':
		case '\r':
		case ' ':
			done = 1;
			break;
		case '\033':
			findword();
			break;
		case '\177':			/* <del> */
		case '\010':			/* <bs> */
			if (p == q)
				break;
			p--;
			getyx(stdscr, row, col);
			move(row, col - 1);
			clrtoeol();
			refresh();
			break;
		case '\025':			/* <^u> */
		case '\027':			/* <^w> */
			if (p == q)
				break;
			getyx(stdscr, row, col);
			move(row, col - (int) (p - q));
			p = q;
			clrtoeol();
			refresh();
			break;
#ifdef SIGTSTP
		case '\032':			/* <^z> */
			stop_catcher();
			break;
#endif
		case '\023':			/* <^s> */
			stoptime();
			printw("<PAUSE>");
			refresh();
			while ((ch = inputch()) != '\021' && ch != '\023')
				;
			move(crow, ccol);
			clrtoeol();
			refresh();
			starttime();
			break;
		case '\003':			/* <^c> */
			cleanup();
			exit(0);
			/*NOTREACHED*/
		case '\004':			/* <^d> */
			done = 1;
			ch = EOF;
			break;
		case '\014':			/* <^l> */
		case '\022':			/* <^r> */
			redraw();
			break;
		case '?':
			stoptime();
			if (help() < 0)
				showstr("Can't open help file", 1);
			starttime();
			break;
		default:
			if (!islower(ch))
				break;
			if ((int) (p - q) == MAXWORDLEN) {
				p = q;
				badword();
				break;
			}
			*p++ = ch;
			addch(ch);
			refresh();
			break;
		}
	}
	*p = '\0';
	if (ch == EOF)
		return((char *) NULL);
	return(q);
}

inputch()
{

	return(getch() & 0177);
}

redraw()
{

	clearok(stdscr, 1);
	refresh();
}

#ifdef XXX
/*
 * Flush all pending input
 */
flushin(fp)
FILE *fp;
{

	flushinp();
}
#endif XXX

#ifdef TIOCFLUSH
#include <sys/file.h>

flushin(fp)
FILE *fp;
{
	int arg;

	arg = FREAD;
	(void) ioctl(fileno(fp), TIOCFLUSH, &arg);
}
#endif TIOCFLUSH

#ifdef ATARI
#include <osbind.h>

/*ARGSUSED*/
flushin(fp)
FILE *fp;
{

	while (Cconis() == -1)
		getch();
}
#endif ATARI

static int gone;

/*
 * Stop the game timer
 */
stoptime()
{
	long t;
	extern long start_t;

	time(&t);
	gone = (int) (t - start_t);
}

/*
 * Restart the game timer
 */
starttime()
{
	long t;
	extern long start_t;

	time(&t);
	start_t = t - (long) gone;
}

/*
 * Initialize for the display of the player's words as they are typed
 * This display starts at (LIST_LINE, LIST_COL) and goes "down" until the last
 * line.  After the last line a new column is started at LIST_LINE
 * Keep track of each column position for showword()
 * There is no check for exceeding COLS
 */
startwords()
{

	crow = LIST_LINE;
	ccol = LIST_COL;
	maxw = 0;
	ncolstarts = 1;
	colstarts[0] = LIST_COL;
	move(LIST_LINE, LIST_COL);
	refresh();
}

/*
 * Add a word to the list and start a new column if necessary
 * The maximum width of the current column is maintained so we know where
 * to start the next column
 */
addword(w)
char *w;
{
	int n;

	if (crow == lastline) {
		crow = LIST_LINE;
		ccol += (maxw + 5);
		colstarts[ncolstarts++] = ccol;
		maxw = 0;
		move(crow, ccol);
	}
	else {
		move(++crow, ccol);
		if ((n = strlen(w)) > maxw)
			maxw = n;
	}
	refresh();
}

/*
 * The current word is unacceptable so erase it
 */
badword()
{

	move(crow, ccol);
	clrtoeol();
	refresh();
}

/*
 * Highlight the nth word in the list (starting with word 0)
 * No check for wild arg
 */
showword(n)
int n;
{
	int col, row;

	row = LIST_LINE + n % (lastline - LIST_LINE + 1);
	col = colstarts[n / (lastline - LIST_LINE + 1)];
	move(row, col);
	standout();
	printw("%s", pword[n]);
	standend();
	move(crow, ccol);
	refresh();
	delay(15);
	move(row, col);
	printw("%s", pword[n]);
	move(crow, ccol);
	refresh();
}

/*
 * Get a word from the user and check if it is in either of the two
 * word lists
 * If it's found, show the word on the board for a short time and then
 * erase the word
 *
 * Note: this function knows about the format of the board
 */
findword()
{
	int c, col, found, i, r, row;
	char buf[MAXWORDLEN + 1];
	extern char board[];
	extern int usedbits, wordpath[];
	extern char *mword[], *pword[];
	extern int nmwords, npwords;

	getyx(stdscr, r, c);
	getword(buf);
	found = 0;
	for (i = 0; i < npwords; i++) {
		if (strcmp(buf, pword[i]) == 0) {
			found = 1;
			break;
		}
	}
	if (!found) {
		for (i = 0; i < nmwords; i++) {
			if (strcmp(buf, mword[i]) == 0) {
				found = 1;
				break;
			}
		}
	}
	for (i = 0; i < MAXWORDLEN; i++)
		wordpath[i] = -1;
	usedbits = 0;
	if (!found || checkword(buf, -1, wordpath) == -1) {
		move(r, c);
		clrtoeol();
		addstr("[???]");
		refresh();
		delay(10);
		move(r, c);
		clrtoeol();
		refresh();
		return;
	}

	standout();
	for (i = 0; wordpath[i] != -1; i++) {
		row = BOARD_LINE + (wordpath[i] / 4) * 2 + 1;
		col = BOARD_COL + (wordpath[i] % 4) * 4 + 2;
		move(row, col);
		if (board[wordpath[i]] == 'q')
			printw("Qu");
		else
			printw("%c", toupper(board[wordpath[i]]));
		move(r, c);
		refresh();
		delay(5);
	}

	standend();

	for (i = 0; wordpath[i] != -1; i++) {
		row = BOARD_LINE + (wordpath[i] / 4) * 2 + 1;
		col = BOARD_COL + (wordpath[i] % 4) * 4 + 2;
		move(row, col);
		if (board[wordpath[i]] == 'q')
			printw("Qu");
		else
			printw("%c", toupper(board[wordpath[i]]));
	}
	move(r, c);
	clrtoeol();
	refresh();
}

/*
 * Display a string at the current cursor position for the given number of secs
 */
showstr(str, delaysecs)
char *str;
int delaysecs;
{

	addstr(str);
	refresh();
	delay(delaysecs * 10);
	move(crow, ccol);
	clrtoeol();
	refresh();
}

putstr(s)
char *s;
{

	addstr(s);
}

/*
 * Get a valid word and put it in the buffer
 */
getword(q)
char *q;
{
	int ch, col, done, i, row;
	char *p;

	done = 0;
	i = 0;
	p = q;
	addch('[');
	refresh();
	while (!done && i < MAXWORDLEN - 1) {
		ch = getch() & 0177;
		switch (ch) {
		case '\177':			/* <del> */
		case '\010':			/* <bs> */
			if (p == q)
				break;
			p--;
			getyx(stdscr, row, col);
			move(row, col - 1);
			clrtoeol();
			break;
		case '\025':			/* <^u> */
		case '\027':			/* <^w> */
			if (p == q)
				break;
			getyx(stdscr, row, col);
			move(row, col - (int) (p - q));
			p = q;
			clrtoeol();
			break;
		case ' ':
		case '\n':
		case '\r':
			done = 1;
			break;
		case '\014':			/* <^l> */
		case '\022':			/* <^r> */
			clearok(stdscr, 1);
			refresh();
			break;
		default:
			if (islower(ch)) {
				*p++ = ch;
				addch(ch);
				i++;
			}
			break;
		}
		refresh();
	}
	*p = '\0';
	addch(']');
	refresh();
}

showboard(b)
char *b;
{

	tty_showboard(b);
}

prompt(mesg)
char *mesg;
{

	move(PROMPT_LINE, PROMPT_COL);
	printw("%s", mesg);
	move(PROMPT_LINE + 1, PROMPT_COL);
	refresh();
}

rawch()
{

#ifdef TIMER
	return(timerch());
#else
	return(getch() & 0177);
#endif
}

static
tty_setup()
{
#ifdef SIGTSTP
	int stop_catcher(), cont_catcher();
#endif
#ifdef TIOCGWINSZ
	int winch_catcher();
#endif

	initscr();
	raw();
	noecho();

	/*
	 * Does curses look at the winsize structure?
	 * Should handle SIGWINCH ...
	 */
	nlines = LINES;
	lastline = nlines - 1;
	ncols = COLS;

#ifdef SIGTSTP
    (void) signal(SIGTSTP, stop_catcher);
    (void) signal(SIGCONT, cont_catcher);
#endif   
#ifdef TIOCGWINSZ
	(void) signal(SIGWINCH, winch_catcher);
#endif
 
	return(0);
}

#ifdef SIGTSTP
static
stop_catcher()
{

	stoptime();
	noraw();
	echo();
	move(nlines - 1, 0);
	refresh();

	(void) signal(SIGTSTP, SIG_DFL);
#ifdef BSD42 
	(void) sigsetmask(sigblock(0) & ~(1 << (SIGTSTP-1)));
#endif
	(void) kill(0, SIGTSTP);
	(void) signal(SIGTSTP, stop_catcher);
}
 
static
cont_catcher()
{

	(void) signal(SIGCONT, cont_catcher);
	noecho();
	raw();
	clearok(stdscr, 1);
	move(crow, ccol);
	refresh();
	starttime();
}
#endif SIGTSTP
 
#ifdef SIGWINCH
/*
 * The signal is caught but nothing is done about it...
 * It would mean reformatting the entire display
 */
static
winch_catcher()
{

	struct winsize win;

	(void) signal(SIGWINCH, winch_catcher);
	(void) ioctl(fileno(stdout), TIOCGWINSZ, &win);
	/*
	LINES = win.ws_row;
	COLS = win.ws_col;
	*/
}
#endif

static
tty_cleanup()
{

	move(nlines - 1, 0);
	refresh();
	noraw();
	echo();
	endwin();
}

static
tty_showboard(b)
char *b;
{
	register int i;
	int line;

	clear();
	move(BOARD_LINE, BOARD_COL);
	line = BOARD_LINE;
	printw("+---+---+---+---+");
	move(++line, BOARD_COL);
	for (i = 0; i < 16; i++) {
		if (b[i] == 'q')
			printw("| Qu");
		else
			printw("| %c ", toupper(b[i]));
		if ((i + 1) % 4 == 0) {
			printw("|");
			move(++line, BOARD_COL);
			printw("+---+---+---+---+");
			move(++line, BOARD_COL);
		}
	}
    move(SCORE_LINE, SCORE_COL);
	printw("Type '?' for help");
	refresh();
}

static
tty_prompt(p)
char *p;
{

	move(PROMPT_LINE, PROMPT_COL);
	printw("%s", p);
	clrtoeol();
	refresh();
}

