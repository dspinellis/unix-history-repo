/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "%Z%%M% %I% %G%";

/*
 * The canfield program
 *
 * Authors:
 *	Originally written: Steve Levine
 *	Converted to use curses and debugged: Steve Feldman
 *	Card counting: Kirk McKusick and Mikey Olson
 *
 * To compile:
 *	cc -O -o canfield canfield.c -lcurses -ltermcap -ltermlib
 */

#include <curses.h>
#include <ctype.h>
#include <signal.h>

#define	decksize	52
#define originrow	0
#define origincol	0
#define	basecol		1
#define	boxcol		42
#define	tboxrow		2
#define	bboxrow		16
#define	movecol		43
#define	moverow		15
#define	msgcol		43
#define	msgrow		14
#define	titlecol	30
#define	titlerow	0
#define	sidecol		1
#define	ottlrow		6
#define	foundcol	11
#define	foundrow	3
#define	stockcol	2
#define	stockrow	8
#define	fttlcol		10
#define	fttlrow		1
#define	taloncol	2
#define	talonrow	13
#define	tabrow		8
#define ctoprow		21
#define cbotrow		23
#define cinitcol	14
#define cheightcol	1
#define cwidthcol	4
#define handstatrow	21
#define handstatcol	7
#define talonstatrow	22
#define talonstatcol	7
#define stockstatrow	23
#define stockstatcol	7
#define	Ace		1
#define	Jack		11
#define	Queen		12
#define	King		13
#define	atabcol		11
#define	btabcol		18
#define	ctabcol		25
#define	dtabcol		32

#define	spades		's'
#define	clubs		'c'
#define	hearts		'h'
#define	diamonds	'd'
#define	black		'b'
#define	red		'r'

#define stk		1
#define	tal		2
#define tab		3
#define INCRHAND(row, col) {\
	row -= cheightcol;\
	if (row < ctoprow) {\
		row = cbotrow;\
		col += cwidthcol;\
	}\
}
#define DECRHAND(row, col) {\
	row += cheightcol;\
	if (row > cbotrow) {\
		row = ctoprow;\
		col -= cwidthcol;\
	}\
}


struct cardtype {
	char suit;
	char color;
	bool visible;
	int rank;
	struct cardtype *next;
};

#define	NIL	((struct cardtype *) -1)

struct cardtype *deck[decksize];
struct cardtype cards[decksize];
struct cardtype *bottom[4], *found[4], *tableau[4];
struct cardtype *talon, *hand, *stock, *basecard;
int length[4];
int cardsoff, base, cinhand, taloncnt, stockcnt, timesthru;
char suitmap[4] = {spades, clubs, hearts, diamonds};
char colormap[4] = {black, black, red, red};
char pilemap[4] = {atabcol, btabcol, ctabcol, dtabcol};
char srcpile, destpile;
int mtforigin, tempbase;
int coldcol, cnewcol, coldrow, cnewrow;
bool errmsg, done;
bool mtfdone, Cflag = FALSE;



/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The following procedures print the board onto the screen using the
 * addressible cursor. The end of these procedures will also be
 * separated from the rest of the program.
 */
/* procedure to set the move command box */
movebox()
{
	    move(tboxrow, boxcol);
	    printw("*--------------------------*");
	    move(tboxrow + 1, boxcol);
	    printw("|         MOVES            |");
	    move(tboxrow + 2, boxcol);
	    printw("|s# = stock to tableau     |");
	    move(tboxrow + 3, boxcol);
	    printw("|sf = stock to foundation  |");
	    move(tboxrow + 4, boxcol);
	    printw("|t# = talon to tableau     |");
	    move(tboxrow + 5, boxcol);
	    printw("|tf = talon to foundation  |");
	    move(tboxrow + 6, boxcol);
	    printw("|## = tableau to tableau   |");
	    move(tboxrow + 7, boxcol);
	    printw("|#f = tableau to foundation|");
	    move(tboxrow + 8, boxcol);
	    printw("|ht = hand to talon        |");
	    move(tboxrow + 9, boxcol);
	    printw("|c = toggle card counting  |");
	    move(tboxrow + 10, boxcol);
	    printw("|q = quit to end the game  |");
	    move(tboxrow + 11, boxcol);
	    printw("|==========================|");
	    move(moverow, boxcol);
	    printw("|                          |");
	    move(msgrow, boxcol);
	    printw("|                          |");
	    move(bboxrow, boxcol);
	    printw("|Replace the # with the    |");
	    move(bboxrow + 1, boxcol);
	    printw("|number of the tableau you |");
	    move(bboxrow + 2, boxcol);
	    printw("|want, 1, 2, 3, or 4.      |");
	    move(bboxrow + 3, boxcol);
	    printw("*--------------------------*");
	    refresh();
}

/* procedure to put the board on the screen using addressable cursor */
makeboard()
{
	clear();
	refresh();
	move(titlerow, titlecol);
	printw("=-> CANFIELD <-=");
	move(fttlrow, fttlcol);
	printw("foundation");
	move(foundrow - 1, fttlcol);
	printw("=---=  =---=  =---=  =---=");
	move(foundrow, fttlcol);
	printw("|   |  |   |  |   |  |   |");
	move(foundrow + 1, fttlcol);
	printw("=---=  =---=  =---=  =---=");
	move(ottlrow, sidecol);
	printw("stock     tableau");
	move(stockrow - 1, sidecol);
	printw("=---=");
	move(stockrow, sidecol);
	printw("|   |");
	move(stockrow + 1, sidecol);
	printw("=---=");
	move(talonrow - 2, sidecol);
	printw("talon");
	move(talonrow - 1, sidecol);
	printw("=---=");
	move(talonrow, sidecol);
	printw("|   |");
	move(talonrow + 1, sidecol);
	printw("=---=");
	move(tabrow - 1, atabcol);
	printw("-1-    -2-    -3-    -4-");
	movebox();
}
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/* clean up the board for another game */
cleanupboard()
{
	int cnt, row, col;
	struct cardtype *ptr;

	if (Cflag) {
		clearstat();
		for(ptr = stock, row = stockrow;
		    ptr != NIL;
		    ptr = ptr->next, row++) {
			move(row, sidecol);
			printw("     ");
		}
		move(row, sidecol);
		printw("     ");
		move(stockrow + 1, sidecol);
		printw("=---=");
		move(talonrow - 2, sidecol);
		printw("talon");
		move(talonrow - 1, sidecol);
		printw("=---=");
		move(talonrow + 1, sidecol);
		printw("=---=");
	}
	move(stockrow, sidecol);
	printw("|   |");
	move(talonrow, sidecol);
	printw("|   |");
	move(foundrow, fttlcol);
	printw("|   |  |   |  |   |  |   |");
	for (cnt = 0; cnt < 4; cnt++) {
		switch(cnt) {
		case 0:
			col = atabcol;
			break;
		case 1:
			col = btabcol;
			break;
		case 2:
			col = ctabcol;
			break;
		case 3:
			col = dtabcol;
			break;
		}
		for(ptr = tableau[cnt], row = tabrow;
		    ptr != NIL;
		    ptr = ptr->next, row++)
			removecard(col, row);
	}
}

/* procedure to create a deck of cards */
initdeck(deck)
struct cardtype *deck[];
{
	int i;
	int scnt;
	char s;
	int r;

	i = 0;
	for (scnt=0; scnt<4; scnt++) {
		s = suitmap[scnt];
		for (r=Ace; r<=King; r++) {
			deck[i] = &cards[i];
			cards[i].rank = r;
			cards[i].suit = s;
			cards[i].color = colormap[scnt];
			cards[i].next = NIL;
			i++;
		}
	}
}

/* procedure to shuffle the deck */
shuffle(deck)
struct cardtype *deck[];
{
	int i,j;
	struct cardtype *temp;

	for (i=0; i<decksize; i++)
		deck[i]->visible = FALSE;
	for (i = decksize-1; i>=0; i--) {
		j = rand() % decksize;
		if (i != j) {
			temp = deck[i];
			deck[i] = deck[j];
			deck[j] = temp;
		}
	}
}

/* procedure to remove the card from the board */
removecard(a, b)
{
	move(b, a);
	printw("   ");
}

/* procedure to print the cards on the board */
printrank(a, b, cp)
struct cardtype *cp;
{
	move(b, a);
	switch (cp->rank) {
		case 2: case 3: case 4: case 5: case 6: case 7:
		case 8: case 9: case 10:
			printw("%2d", cp->rank);
			break;
		case Ace:
			printw(" A");
			break;
		case Jack:
			printw(" J");
			break;
		case Queen:
			printw(" Q");
			break;
		case King:
			printw(" K");
	}
}

printcard(a, b, cp)
int a,b;
struct cardtype *cp;
{
	if (cp == NIL)
		removecard(a, b);
	else if (cp->visible == FALSE) {
		move(b, a);
		printw(" ? ");
	} else {
		printrank(a, b, cp);
		addch(cp->suit);
	}
}

/*
 * procedure to move the top card from one location to the top 
 * of another location. The pointers always point to the top
 * of the piles.
 */
transit(source, dest)
struct cardtype **source, **dest;
{
	struct cardtype *temp;
	
	temp = *source;
	*source = (*source)->next;
	temp->next = *dest;
	*dest = temp;
}

/*
 * Procedure to set the cards on the foundation base when available.
 * Note that it is only called on a foundation pile at the beginning of
 * the game, so the pile will have exactly one card in it.
 */

fndbase(cp, column, row)
struct cardtype **cp;
{
	bool nomore;

	if (*cp != NIL)
		do {
			if ((*cp)->rank == basecard->rank) {
				base++;
				printcard(pilemap[base], foundrow, *cp);
				if (*cp == tableau[0])
					length[0] = length[0] - 1;
				if (*cp == tableau[1])
					length[1] = length[1] - 1;
				if (*cp == tableau[2])
					length[2] = length[2] - 1;
				if (*cp == tableau[3])
					length[3] = length[3] - 1;
				transit(cp, &found[base]);
				if (cp == &talon)
					usedtalon();
				if (cp == &stock)
					usedstock();
				if (*cp != NIL) {
					printcard(column, row, *cp);
					nomore = FALSE;
				} else {
					removecard(column, row);
					nomore = TRUE;
				}
				cardsoff++;
			} else 
				nomore = TRUE;
	} while (nomore == FALSE);
}

/* procedure to initialize the things necessary for the game */
initgame()
{
	register i;

	for (i=0; i<18; i++)
		deck[i]->visible = TRUE;
	stockcnt = 13;
	stock = deck[12];
	for (i=12; i>=1; i--)
		deck[i]->next = deck[i - 1];
	deck[0]->next = NIL;
	found[0] = deck[13];
	deck[13]->next = NIL;
	for (i=1; i<4; i++)
		found[i] = NIL;
	basecard = found[0];
	for (i=14; i<18; i++) {
		tableau[i - 14] = deck[i];
		deck[i]->next = NIL;
	}
	for (i=0; i<4; i++) {
		bottom[i] = tableau[i];
		length[i] = tabrow;
	}
	hand = deck[18];
	for (i=18; i<decksize-1; i++)
		deck[i]->next = deck[i + 1];
	deck[decksize-1]->next = NIL;
	talon = NIL;
	base = 0;
	cinhand = 34;
	taloncnt = 0;
	timesthru = 0;
	cardsoff = 1;
	coldrow = ctoprow;
	coldcol = cinitcol;
	cnewrow = ctoprow;
	cnewcol = cinitcol + cwidthcol;
}

/* procedure to print the beginning cards and to start each game */
startgame()
{
	register int j;

	shuffle(deck);
	initgame();
	printcard(foundcol, foundrow, found[0]);
	printcard(stockcol, stockrow, stock);
	printcard(atabcol, tabrow, tableau[0]);
	printcard(btabcol, tabrow, tableau[1]);
	printcard(ctabcol, tabrow, tableau[2]);
	printcard(dtabcol, tabrow, tableau[3]);
	printcard(taloncol, talonrow, talon);
	move(foundrow - 2, basecol);
	printw("Base");
	move(foundrow - 1, basecol);
	printw("Rank");
	printrank(basecol, foundrow, found[0]);
	for (j=0; j<=3; j++)
		fndbase(&tableau[j], pilemap[j], tabrow);
	fndbase(&stock, stockcol, stockrow);
	showstat();	/* show card counting info to cheaters */
}


/* procedure to clear the message printed from an error */
clearmsg()
{
	int i;

	if (errmsg == TRUE) {
		errmsg = FALSE;
		move(msgrow, msgcol);
		for (i=0; i<25; i++)
			addch(' ');
		refresh();
	}
}

/* procedure to print an error message if the move is not listed */
dumberror()
{
	errmsg = TRUE;
	move(msgrow, msgcol);
	printw("Not a proper move       ");
}

/* procedure to print an error message if the move is not possible */
destinerror()
{
	errmsg = TRUE;
	move(msgrow, msgcol);
	printw("Error: Can't move there");
}

/* function to see if the source has cards in it */
bool
notempty(cp)
struct cardtype *cp;
{
	if (cp == NIL) {
		errmsg = TRUE;
		move(msgrow, msgcol);
		printw("Error: no cards to move");
		return (FALSE);
	} else 
		return (TRUE);
}


/* function to see if the rank of one card is less than another */

bool
ranklower(cp1, cp2)
struct cardtype *cp1, *cp2;
{
	if (cp2->rank == Ace) 
		if (cp1->rank == King)
			return (TRUE);
		else 
			return (FALSE);
	else if (cp1->rank + 1 == cp2->rank)
		return (TRUE);
	else 
		return (FALSE);
}

/* function to check the cardcolor for moving to a tableau */
bool
diffcolor(cp1, cp2)
struct cardtype *cp1, *cp2;
{
	if (cp1->color == cp2->color)
		return (FALSE);
	else 
		return (TRUE);
}

/* function to see if the card can move to the tableau */
bool
tabok(cp, des)
struct cardtype *cp;
{
	if ((cp == stock) && (tableau[des] == NIL))
		return (TRUE);
	else if (tableau[des] == NIL)
		if (stock == NIL)
			return (TRUE);
		else 
			return (FALSE);
	else if (ranklower(cp, tableau[des]) && diffcolor(cp, tableau[des]))
		return (TRUE);
	else 
		return (FALSE);
}


/* procedure to turn the cards onto the talon from the deck */
movetotalon()
{
	int i, fin;

	if (cinhand >= 3)
		fin = 3;
	else if (cinhand > 0)
		fin = cinhand;
	else if (talon != NIL) {
		timesthru++;
		errmsg = TRUE;
		move(msgrow, msgcol);
		if (timesthru != 4) {
			printw("Talon is now the new hand");
			while (talon != NIL) {
				transit(&talon, &hand);
				cinhand++;
			}
			if (cinhand >= 3)
				fin = 3;
			else
				fin = cinhand;
			taloncnt = 0;
			coldrow = ctoprow;
			coldcol = cinitcol;
			cnewrow = ctoprow;
			cnewcol = cinitcol + cwidthcol;
			clearstat();
			showstat();
		} else {
			fin = 0;
			done = TRUE;
			printw("I believe you have lost");
			refresh();
			sleep(5);
		}
	} else {
		errmsg = TRUE;
		move(msgrow, msgcol);
		printw("Talon and hand are empty");
		fin = 0;
	}
	for (i=0; i<fin; i++) {
		transit(&hand, &talon);
		INCRHAND(cnewrow, cnewcol);
		INCRHAND(coldrow, coldcol);
		removecard(cnewcol, cnewrow);
		if (i == fin - 1)
			talon->visible = TRUE;
		if (Cflag)
			printcard(coldcol, coldrow, talon);
	}
	if (fin != 0) {
		printcard(taloncol, talonrow, talon);
		cinhand -= fin;
		taloncnt += fin;
		if (Cflag) {
			move(handstatrow, handstatcol);
			printw("%3d", cinhand);
			move(talonstatrow, talonstatcol);
			printw("%3d", taloncnt);
		}
		fndbase(&talon, taloncol, talonrow);
	}
}


/* procedure to print card counting info on screen */
showstat()
{
	int row, col;
	register struct cardtype *ptr;

	if (Cflag) {
		move(talonstatrow, talonstatcol - 7);
		printw("Talon: %3d", taloncnt);
		move(handstatrow, handstatcol - 7);
		printw("Hand:  %3d", cinhand);
		move(stockstatrow, stockstatcol - 7);
		printw("Stock: %3d", stockcnt);
		for ( row = coldrow, col = coldcol, ptr = talon;
		      ptr != NIL;
		      ptr = ptr->next ) {
			printcard(col, row, ptr);
			DECRHAND(row, col);
		}
		for ( row = cnewrow, col = cnewcol, ptr = hand;
		      ptr != NIL;
		      ptr = ptr->next ) {
			INCRHAND(row, col);
			printcard(col, row, ptr);
		}
	}
}


/* procedure to clear card counting info from screen */
clearstat()
{
	int row;

	move(talonstatrow, talonstatcol - 7);
	printw("          ");
	move(handstatrow, handstatcol - 7);
	printw("          ");
	move(stockstatrow, stockstatcol - 7);
	printw("          ");
	for ( row = ctoprow ; row <= cbotrow ; row++ ) {
		move(row, cinitcol);
		printw("%56s", " ");
	}
}


/* procedure to update card counting base */
usedtalon()
{
	removecard(coldcol, coldrow);
	DECRHAND(coldrow, coldcol);
	if (talon != NIL && (talon->visible == FALSE)) {
		talon->visible = TRUE;
		if (Cflag)
			printcard(coldcol, coldrow, talon);
	}
	taloncnt--;
	if (Cflag) {
		move(talonstatrow, talonstatcol);
		printw("%3d", taloncnt);
	}
}


/* procedure to update stock card counting base */
usedstock()
{
	stockcnt--;
	if (Cflag) {
		move(stockstatrow, stockstatcol);
		printw("%3d", stockcnt);
	}
}


/* let 'em know how they lost! */
showcards()
{
	register struct cardtype *ptr;
	int row;

	if (!Cflag)
		return;
	for (ptr = talon; ptr != NIL; ptr = ptr->next)
		ptr->visible = TRUE;
	for (ptr = hand; ptr != NIL; ptr = ptr->next)
		ptr->visible = TRUE;
	showstat();
	move(stockrow + 1, sidecol);
	printw("     ");
	move(talonrow - 2, sidecol);
	printw("     ");
	move(talonrow - 1, sidecol);
	printw("     ");
	move(talonrow, sidecol);
	printw("     ");
	move(talonrow + 1, sidecol);
	printw("     ");
	for (ptr = stock, row = stockrow; ptr != NIL; ptr = ptr->next, row++) {
		move(row, stockcol - 1);
		printw("|   |");
		printcard(stockcol, row, ptr);
	}
	if (stock == NIL) {
		move(row, stockcol - 1);
		printw("|   |");
		row++;
	}
	move(handstatrow, handstatcol - 7);
	printw("          ");
	move(row, stockcol - 1);
	printw("=---=");
	getcmd(moverow, movecol, "Hit return to exit");
}


/* procedure to move a card from the stock or talon to the tableau */
simpletableau(cp, des)
struct cardtype **cp;
{
	int origin;

	if (notempty(*cp)) {
		if (tabok(*cp, des)) {
			if (*cp == stock)
				origin = stk;
			else
				origin = tal;
			if (tableau[des] == NIL)
				bottom[des] = *cp;
			transit(cp, &tableau[des]);
			length[des]++;
			printcard(pilemap[des], length[des], tableau[des]);
			timesthru = 0;
			if (origin == stk) {
				usedstock();
				printcard(stockcol, stockrow, stock);
			} else {
				usedtalon();
				printcard(taloncol, talonrow, talon);
			}
		} else 
			destinerror();
	}
}


tabprint(sour, des)
{
	int dlength, slength, i;
	struct cardtype *tempcard;

	for (i=tabrow; i<=length[sour]; i++)
		removecard(pilemap[sour], i);
	dlength = length[des] + 1;
	slength = length[sour];
	if (slength == tabrow)
		printcard(pilemap[des], dlength, tableau[sour]);
	else 
		while (slength != tabrow - 1) {
			tempcard = tableau[sour];
			for (i=1; i<=slength-tabrow; i++)
			    tempcard = tempcard->next;
			printcard(pilemap[des], dlength, tempcard);
			slength--;
			dlength++;
		}
}

/* procedure to move from the tableau to the tableau */
tabtotab(sour, des)
{
	struct cardtype *temp;

	if (notempty(tableau[sour])) {
		if (tabok(bottom[sour], des)) {
			tabprint(sour, des);
			temp = bottom[sour];
			bottom[sour] = NIL;
			temp->next = tableau[des];
			tableau[des] = tableau[sour];
			tableau[sour] = NIL;
			length[des] = length[des] + (length[sour] - (tabrow - 1));
			length[sour] = tabrow - 1;
			timesthru = 0;
		} else 
			destinerror();
	}
}


/* functions to see if the card can go onto the foundation */
bool
rankhigher(cp, let)
struct cardtype *cp;
{
	if (found[let]->rank == King)
		if (cp->rank == Ace)
			return(TRUE);
		else 
			return(FALSE);
	else if (cp->rank - 1 == found[let]->rank)
		return(TRUE);
	else 
		return(FALSE);
}

samesuit(cp, let)
struct cardtype *cp;
{
	if (cp->suit == found[let]->suit)
		return (TRUE);
	else 
		return (FALSE);
}

/* procedure to move a card to the correct foundation pile */

movetofound(cp, source)
struct cardtype **cp;
{
	tempbase = 0;
	mtfdone = FALSE;
	if (notempty(*cp)) {
		do {
			if (found[tempbase] != NIL)
				if (rankhigher(*cp, tempbase)
				    && samesuit(*cp, tempbase)) {
					if (*cp == stock)
						mtforigin = stk;
					else if (*cp == talon)
						mtforigin = tal;
					else
						mtforigin = tab;
					transit(cp, &found[tempbase]);
					printcard(pilemap[tempbase],
						foundrow, found[tempbase]);
					timesthru = 0;
					if (mtforigin == stk) {
						usedstock();
						printcard(stockcol, stockrow, stock);
					} else if (mtforigin == tal) {
						usedtalon();
						printcard(taloncol, talonrow, talon);
					} else {
						removecard(pilemap[source], length[source]);
						length[source]--;
					}
					cardsoff++;
					mtfdone = TRUE;
				} else
					tempbase++;
			else 
				tempbase++;
		} while ((tempbase != 4) && !mtfdone);
		if (!mtfdone)
			destinerror();
	}
}


/* procedure to get a command */

getcmd(row, col, cp)
	int row, col;
	char *cp;
{
	char cmd[2], ch;
	int i;

	i = 0;
	move(row, col);
	printw("%-24s", cp);
	col += 1 + strlen(cp);
	move(row, col);
	refresh();
	do {
		ch = getch() & 0177;
		if (ch >= 'A' && ch <= 'Z')
			ch += ('a' - 'A');
		if (ch == '\f') {
			wrefresh(curscr);
			refresh();
		} else if (i >= 2 && ch != _tty.sg_erase && ch != _tty.sg_kill) {
			if (ch != '\n' && ch != '\r' && ch != ' ')
				write(1, "\007", 1);
		} else if (ch == _tty.sg_erase && i > 0) {
			printw("\b \b");
			refresh();
			i--;
		} else if (ch == _tty.sg_kill && i > 0) {
			while (i > 0) {
				printw("\b \b");
				i--;
			}
			refresh();
		} else if (ch == '\032') {	/* Control-Z */
			suspend();
			move(row, col + i);
			refresh();
		} else if (isprint(ch)) {
			cmd[i++] = ch;
			addch(ch);
			refresh();
		}
	} while (ch != '\n' && ch != '\r' && ch != ' ');
	srcpile = cmd[0];
	destpile = cmd[1];
}

/* Suspend the game (shell escape if no process control on system) */

suspend()
{
#ifndef SIGTSTP
	char *sh;
#endif

	move(21, 0);
	refresh();
	endwin();
	fflush(stdout);
#ifdef SIGTSTP
	kill(getpid(), SIGTSTP);
#else
	sh = getenv("SHELL");
	if (sh == NULL)
		sh = "/bin/sh";
	system(sh);
#endif
	raw();
	noecho();
}

/* procedure to evaluate and make the specific moves */

movecard()
{
	int source, dest;

	done = FALSE;
	errmsg = FALSE;
	do {
		if (cardsoff == 52) {
			refresh();
			srcpile = 'q';
		} else
			getcmd(moverow, movecol, "Move:");
		clearmsg();
		if (srcpile >= '1' && srcpile <= '4')
			source = (int) (srcpile - '1');
		if (destpile >= '1' && destpile <= '4')
			dest = (int) (destpile - '1');
		switch (srcpile) {
			case 't':
				if (destpile == 'f' || destpile == 'F')
					movetofound(&talon, source);
				else if (destpile >= '1' && destpile <= '4')
					simpletableau(&talon, dest);
				else
					dumberror();
				break;
			case 's':
				if (destpile == 'f' || destpile == 'F')
					movetofound(&stock, source);
				else if (destpile >= '1' && destpile <= '4')
					simpletableau(&stock, dest);
				else dumberror();
				break;
			case 'h':
				if (destpile == 't' || destpile == 'T')
					movetotalon();
				else dumberror();
				break;
			case 'q':
				showcards();
				done = TRUE;
				break;
			case 'c':
				Cflag = !Cflag;
				if (Cflag)
					showstat();
				else
					clearstat();
				break;
			case '1': case '2': case '3': case '4':
				if (destpile == 'f' || destpile == 'F')
					movetofound(&tableau[source], source);
				else if (destpile >= '1' && destpile <= '4')
					tabtotab(source, dest);
				else dumberror();
				break;
			default:
				dumberror();
		}
		fndbase(&stock, stockcol, stockrow);
		fndbase(&talon, taloncol, talonrow);
	} while (!done);
}

/* procedure to printout instructions */
instruct()
{
	move(originrow, origincol);
	printw("This is the game of solitaire called Canfield.  Do\n");
	printw("you want instructions for the game?");
	do {
		getcmd(originrow + 3, origincol, "y or n?");
	} while (srcpile != 'y' && srcpile != 'n');
	if (srcpile == 'y') {
		clear();
		refresh();
		printw("Here are brief instuctions to the game of Canfield:\n");
		printw("\n");
		printw("     If you have never played solitaire before, it is recom-\n");
		printw("mended  that  you  consult  a solitaire instruction book. In\n");
		printw("Canfield, tableau cards may be built on each other  downward\n");
		printw("in  alternate colors. An entire pile must be moved as a unit\n");
		printw("in building. Top cards of the piles are available to be able\n");
		printw("to be played on foundations, but never into empty spaces.\n");
		printw("\n");
		printw("     Spaces must be filled from the stock. The top  card  of\n");
		printw("the  stock  also is available to be played on foundations or\n");
		printw("built on tableau piles. After the stock  is  exhausted,  ta-\n");
		printw("bleau spaces may be filled from the talon and the player may\n");
		printw("keep them open until he wishes to use them.\n");
		printw("\n");
		printw("     Cards are dealt from the hand to the  talon  by  threes\n");
		printw("and  this  repeats until there are no more cards in the hand\n");
		printw("or the player quits. To have cards dealt onto the talon  the\n");
		printw("player  types  'ht'  for his move. Foundation base cards are\n");
		printw("also automatically moved to the foundation when they  become\n");
		printw("available.\n\n");
		printw("push any key when you are finished: ");
		refresh();
		getch();
	}
}

/* procedure to initialize the game */
initall()

{
	srand(getpid());
	initdeck(deck);
}

/* procedure to end the game */
bool
finish()
{
	int row, col;

	if (cardsoff == 52) {
		clear();
		refresh();
		move(originrow, origincol);
		printw("CONGRATULATIONS!\n");
		printw("You won the game. That is a feat to be proud of.\n");
		move(originrow + 4, origincol);
		printw("Wish to play again?     ");
		row = originrow + 5;
		col = origincol;
	} else {
		move(msgrow, msgcol);
		printw("You got %d card", cardsoff);
		if (cardsoff > 1)
			printw("s");
		printw(" off    ");
		getcmd(moverow, movecol, "Hit return to continue");
		move(msgrow, msgcol);
		printw("Wish to play again?     ");
		row = moverow;
		col = movecol;
	}
	do {
		getcmd(row, col, "y or n?");
	} while (srcpile != 'y' && srcpile != 'n');
	errmsg = TRUE;
	clearmsg();
	if (srcpile == 'y')
		return (FALSE);
	else
		return (TRUE);
}

main(argc, argv)
	int argc;
	char *argv[];
{
#ifdef MAXLOAD
	double vec[3];

	loadav(vec);
	if (vec[2] >= MAXLOAD) {
		puts("The system load is too high.  Try again later.");
		exit(0);
	}
#endif
	initscr();
	raw();
	noecho();
	initall();
	instruct();
	makeboard();
	for (;;) {
		startgame();
		movecard();
		if (finish())
			break;
		if (cardsoff == 52)
			makeboard();
		else
			cleanupboard();
	}
	clear();
	move(22,0);
	refresh();
	endwin();
}
