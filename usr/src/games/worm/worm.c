/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)worm.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Worm.  Written by Michael Toy
 * UCSC
 */

#include <ctype.h>
#include <curses.h>
#include <signal.h>

#define newlink() (struct body *) malloc(sizeof (struct body));
#define HEAD '@'
#define BODY 'o'
#define LENGTH 7
#define RUNLEN 8
#define when break;case
#define otherwise break;default
#define CNTRL(p) ('p'-'A'+1)
#ifndef attron
# define	baudrate()	_tty.sg_ospeed
#endif

WINDOW *tv;
WINDOW *stw;
struct body {
	int x;
	int y;
	struct body *prev;
	struct body *next;
} *head, *tail, goody;
int growing = 0;
int running = 0;
int slow = 0;
int score = 0;
int start_len = LENGTH;
char lastch;
char outbuf[BUFSIZ];

main(argc, argv)
char **argv;
{
	int leave(), wake(), suspend();
	char ch;

	if (argc == 2)
		start_len = atoi(argv[1]);
	if ((start_len <= 0) || (start_len > 500))
		start_len = LENGTH;
	setbuf(stdout, outbuf);
	srand(getpid());
	signal(SIGALRM, wake);
	signal(SIGINT, leave);
	signal(SIGQUIT, leave);
#ifdef SIGTSTP
	signal(SIGTSTP, suspend);	/* process control signal */
#endif
	initscr();
	crmode();
	noecho();
	slow = (baudrate() <= B1200);
	clear();
	stw = newwin(1, COLS-1, 0, 0);
	tv = newwin(LINES-1, COLS-1, 1, 0);
	box(tv, '*', '*');
	scrollok(tv, FALSE);
	scrollok(stw, FALSE);
	wmove(stw, 0, 0);
	wprintw(stw, " Worm");
	refresh();
	wrefresh(stw);
	wrefresh(tv);
	life();			/* Create the worm */
	prize();		/* Put up a goal */
	while(1)
	{
		if (running)
		{
			running--;
			process(lastch);
		}
		else
		{
		    fflush(stdout);
		    if (read(0, &ch, 1) >= 0)
			process(ch);
		}
	}
}

life()
{
	register struct body *bp, *np;
	register int i;

	head = newlink();
	head->x = start_len+2;
	head->y = 12;
	head->next = NULL;
	display(head, HEAD);
	for (i = 0, bp = head; i < start_len; i++, bp = np) {
		np = newlink();
		np->next = bp;
		bp->prev = np;
		np->x = bp->x - 1;
		np->y = bp->y;
		display(np, BODY);
	}
	tail = np;
	tail->prev = NULL;
}

display(pos, chr)
struct body *pos;
char chr;
{
	wmove(tv, pos->y, pos->x);
	waddch(tv, chr);
}

leave()
{
	endwin();
	exit(0);
}

wake()
{
	signal(SIGALRM, wake);
	fflush(stdout);
	process(lastch);
}

rnd(range)
{
	return abs((rand()>>5)+(rand()>>5)) % range;
}

newpos(bp)
struct body * bp;
{
	do {
		bp->y = rnd(LINES-3)+ 2;
		bp->x = rnd(COLS-3) + 1;
		wmove(tv, bp->y, bp->x);
	} while(winch(tv) != ' ');
}

prize()
{
	int value;

	value = rnd(9) + 1;
	newpos(&goody);
	waddch(tv, value+'0');
	wrefresh(tv);
}

process(ch)
char ch;
{
	register int x,y;
	struct body *nh;

	alarm(0);
	x = head->x;
	y = head->y;
	switch(ch)
	{
		when 'h': x--;
		when 'j': y++;
		when 'k': y--;
		when 'l': x++;
		when 'H': x--; running = RUNLEN; ch = tolower(ch);
		when 'J': y++; running = RUNLEN/2; ch = tolower(ch);
		when 'K': y--; running = RUNLEN/2; ch = tolower(ch);
		when 'L': x++; running = RUNLEN; ch = tolower(ch);
		when '\f': setup(); return;
		when CNTRL(Z): suspend(); return;
		when CNTRL(C): crash(); return;
		when CNTRL(D): crash(); return;
		otherwise: if (! running) alarm(1);
			   return;
	}
	lastch = ch;
	if (growing == 0)
	{
		display(tail, ' ');
		tail->next->prev = NULL;
		nh = tail->next;
		free(tail);
		tail = nh;
	}
	else growing--;
	display(head, BODY);
	wmove(tv, y, x);
	if (isdigit(ch = winch(tv)))
	{
		growing += ch-'0';
		prize();
		score += growing;
		running = 0;
		wmove(stw, 0, 68);
		wprintw(stw, "Score: %3d", score);
		wrefresh(stw);
	}
	else if(ch != ' ') crash();
	nh = newlink();
	nh->next = NULL;
	nh->prev = head;
	head->next = nh;
	nh->y = y;
	nh->x = x;
	display(nh, HEAD);
	head = nh;
	if (!(slow && running))
		wrefresh(tv);
	if (!running)
		alarm(1);
}

crash()
{
	sleep(2);
	clear();
	move(23, 0);
	refresh();
	printf("Well you ran into something and the game is over.\n");
	printf("Your final score was %d\n", score);
	leave();
}

suspend()
{
	char *sh;

	move(LINES-1, 0);
	refresh();
	endwin();
	fflush(stdout);
#ifdef SIGTSTP
	kill(getpid(), SIGTSTP);
	signal(SIGTSTP, suspend);
#else
	sh = getenv("SHELL");
	if (sh == NULL)
		sh = "/bin/sh";
	system(sh);
#endif
	crmode();
	noecho();
	setup();
}

setup()
{
	clear();
	refresh();
	touchwin(stw);
	wrefresh(stw);
	touchwin(tv);
	wrefresh(tv);
	alarm(1);
}
