#ifndef lint
static	char *sccsid = "@(#)pl_4.c	1.1 83/07/20";
#endif

#include "player.h"

changesail()
{
	int rig, full;

	rig = mc->rig1;
	full = mf->FS;
	if (windspeed == 6 || windspeed == 5 && mc->class > 4)
		rig = 0;
	if (mc->crew3 && rig) {
		if (!full) {
			Signal("Increase to Full sails? ", (struct ship *)0);
			if (sgetch(1) == 'y') {
				changed = 1;
				Write(W_FS, ms, 0, 1, 0, 0, 0);
			}
		} else {
			Signal("Reduce to Battle sails? ", (struct ship *)0);
			if (sgetch(1) == 'y') {
				Write(W_FS, ms, 0, 0, 0, 0, 0);
				changed = 1;
			}
		}
	} else if (!rig)
		Signal("Sails rent to pieces", (struct ship *)0);
}

signalflags()
{
	register struct ship *sp;

	foreachship(sp) {
		if (*sp->file->signal) {
			(void) putchar('\7');
			Signal("%s (%c%c): %s", sp, sp->file->signal);
			*sp->file->signal = '\0';
		}
	}
}

acceptsignal()
{
	char buf[60];
	register char *p = buf;

	Signal("Message? ", (struct ship *)0);
	*p++ = '"';
	sgetstr(p, sizeof buf - 2);
	while (*p++)
		;
	p[-1] = '"';
	Write(W_SIGNAL, ms, 1, (int)buf, 0, 0, 0);
}

/*VARARGS2*/
Signal(fmt, ship, a, b, c, d)
char *fmt;
register struct ship *ship;
int a, b, c, d;
{
	Scroll();
	if (ship == 0)
		(void) wprintw(scroll_w, fmt, a, b, c, d);
	else
		(void) wprintw(scroll_w, fmt, ship->shipname, colours(ship),
			sterncolour(ship), a, b, c, d);
	(void) wrefresh(scroll_w);
}

static int sline = 0;

Scroll()
{
	(void) wmove(scroll_w, sline++, 0);
	(void) wclrtoeol(scroll_w);
	if (sline >= SCROLL_Y)
		sline = 0;
}

/* make sure we have two consecutive lines */
Scroll2()
{
	Scroll();
	if (sline > SCROLL_Y - 2)
		Scroll();
}

lastline()
{
	(void) wmove(scroll_w, SCROLL_Y-1, 0);
	(void) wclrtoeol(scroll_w);
	(void) wrefresh(scroll_w);
}

prompt()
{
	(void) wmove(scroll_w, sline, 0);
	(void) wclrtoeol(scroll_w);
	(void) addch('~');
	(void) wmove(scroll_w, sline, 0);
	(void) wrefresh(scroll_w);
}

sgetch(flag)
char flag;
{
	register c;

	while ((c = wgetch(scroll_w)) == EOF)
		;
	if (flag)
		(void) waddch(scroll_w, c);
	return c;
}

sgetstr(buf, n)
char *buf;
{
	register c;
	register char *p = buf;

	for (;;) {
		while ((c = wgetch(scroll_w)) == EOF)
			;
		switch (c) {
		case '\n':
		case '\r':
			*p = 0;
			return;
		case '\b':
			if (p > buf)
				p--;
			break;
		default:
			if (p < &buf[n] - 1) {
				*p++ = c;
				(void) waddch(scroll_w, c);
				(void) wrefresh(scroll_w);
			} else {
				(void) putchar(CTRL(g));
				(void) fflush(stdout);
			}
		}
	}
}

newturn()
{
	repaired = loaded = fired = changed = 0;

	(void) alarm(0);
	if (mf->readyL & R_LOADING)
		if (mf->readyL & R_DOUBLE)
			mf->readyL = R_LOADING;
		else
			mf->readyL = R_LOADED;
	if (mf->readyR & R_LOADING)
		if (mf->readyR & R_DOUBLE)
			mf->readyR = R_LOADING;
		else
			mf->readyR = R_LOADED;
	movebuf[0] = '\0';
	Sync();
	if (turn % 50 == 0)		/* still playing */
		Write(W_TIME, SHIP(0), 0, 1, 0, 0, 0); /* XXX */
	windspeed = cc->windspeed;
	winddir = cc->winddir;
	turn = cc->turn;
	if (mf->FS == 1)
		Write(W_FS, ms, 0, 2, 0, 0, 0);
	readpos();
	adjustview();

	screen();

	(void) signal(SIGALRM, newturn);
	(void) alarm(7);
}

screen()
{
	draw_view();
	draw_turn();
	draw_stat();
	draw_slot();
	(void) wrefresh(scroll_w);
}

draw_view()
{
	register struct ship *sp;

	(void) werase(view_w);
	foreachship(sp) {
		if (sp->file->dir
		    && sp->file->row > viewrow
		    && sp->file->row < viewrow + VIEW_Y
		    && sp->file->col > viewcol
		    && sp->file->col < viewcol + VIEW_X) {
			(void) wmove(view_w, sp->file->row - viewrow,
				sp->file->col - viewcol);
			(void) waddch(view_w, colours(sp));
			(void) wmove(view_w,
				sternrow(sp) - viewrow,
				sterncol(sp) - viewcol);
			(void) waddch(view_w, sterncolour(sp));
		}
	}
	(void) wrefresh(view_w);
}

draw_turn()
{
	(void) wmove(turn_w, 0, 0);
	(void) wprintw(turn_w, "Turn %d", turn);
	(void) wrefresh(turn_w);
}

draw_stat()
{
	(void) wmove(stat_w, STAT_1, 0);
	(void) wprintw(stat_w, "Points  %3d\n", mf->points);
	(void) wprintw(stat_w, "Fouls    %2d\n", fouled(ms));
	(void) wprintw(stat_w, "Grapples %2d\n", grappled(ms));

	(void) wmove(stat_w, STAT_2, 0);
	(void) wprintw(stat_w, "    0 %c(%c)\n",
		maxmove(ms, winddir + 3, -1) + '0',
		maxmove(ms, winddir + 3, 1) + '0');
	(void) waddstr(stat_w, "   \\|/\n");
	(void) wprintw(stat_w, "   -^-%c(%c)\n",
		maxmove(ms, winddir + 2, -1) + '0',
		maxmove(ms, winddir + 2, 1) + '0');
	(void) waddstr(stat_w, "   /|\\\n");
	(void) wprintw(stat_w, "    | %c(%c)\n",
		maxmove(ms, winddir + 1, -1) + '0',
		maxmove(ms, winddir + 1, 1) + '0');
	(void) wprintw(stat_w, "   %c(%c)\n",
		maxmove(ms, winddir, -1) + '0',
		maxmove(ms, winddir, 1) + '0');

	(void) wmove(stat_w, STAT_3, 0);
	(void) wprintw(stat_w, "Load  %c%c %c%c\n",
		loadname[mf->loadL], readyname(mf->readyL),
		loadname[mf->loadR], readyname(mf->readyR));
	(void) wprintw(stat_w, "Hull %2d\n", mc->hull);
	(void) wprintw(stat_w, "Crew %2d %2d %2d\n",
		mc->crew1, mc->crew2, mc->crew3);
	(void) wprintw(stat_w, "Guns %2d %2d\n", mc->gunL, mc->gunR);
	(void) wprintw(stat_w, "Carr %2d %2d\n", mc->carR, mc->carL);
	(void) wprintw(stat_w, "Rigg %d %d %d ", mc->rig1, mc->rig2, mc->rig3);
	if (mc->rig4 < 0)
		(void) waddch(stat_w, '-');
	else
		(void) wprintw(stat_w, "%d", mc->rig4);
	(void) wrefresh(stat_w);
}

draw_slot()
{
	if (!boarding(ms, 0)) {
		(void) mvwaddstr(slot_w, 0, 0, "   ");
		(void) mvwaddstr(slot_w, 1, 0, "   ");
	} else
		(void) mvwaddstr(slot_w, 1, 0, "OBP");
	if (!boarding(ms, 1)) {
		(void) mvwaddstr(slot_w, 2, 0, "   ");
		(void) mvwaddstr(slot_w, 3, 0, "   ");
	} else
		(void) mvwaddstr(slot_w, 3, 0, "DBP");

	(void) wmove(slot_w, SLOT_Y-4, 0);
	if (mf->RH)
		(void) wprintw(slot_w, "%dRH", mf->RH);
	else
		(void) waddstr(slot_w, "   ");
	(void) wmove(slot_w, SLOT_Y-3, 0);
	if (mf->RG)
		(void) wprintw(slot_w, "%dRG", mf->RG);
	else
		(void) waddstr(slot_w, "   ");
	(void) wmove(slot_w, SLOT_Y-2, 0);
	if (mf->RR)
		(void) wprintw(slot_w, "%dRR", mf->RR);
	else
		(void) waddstr(slot_w, "   ");

#define Y	(SLOT_Y/2)
	(void) wmove(slot_w, 7, 1);
	(void) wprintw(slot_w,"%d", windspeed);
	(void) mvwaddch(slot_w, Y, 0, ' ');
	(void) mvwaddch(slot_w, Y, 2, ' ');
	(void) mvwaddch(slot_w, Y-1, 0, ' ');
	(void) mvwaddch(slot_w, Y-1, 1, ' ');
	(void) mvwaddch(slot_w, Y-1, 2, ' ');
	(void) mvwaddch(slot_w, Y+1, 0, ' ');
	(void) mvwaddch(slot_w, Y+1, 1, ' ');
	(void) mvwaddch(slot_w, Y+1, 2, ' ');
	(void) wmove(slot_w, Y - dr[winddir], 1 - dc[winddir]);
	switch (winddir) {
	case 1:
	case 5:
		(void) waddch(slot_w, '|');
		break;
	case 2:
	case 6:
		(void) waddch(slot_w, '/');
		break;
	case 3:
	case 7:
		(void) waddch(slot_w, '-');
		break;
	case 4:
	case 8:
		(void) waddch(slot_w, '\\');
		break;
	}
	(void) mvwaddch(slot_w, Y + dr[winddir], 1 + dc[winddir], '+');
	(void) wrefresh(slot_w);
}

board()
{
	register int n;

	(void) clear();
	(void) werase(view_w);
	(void) werase(slot_w);
	(void) werase(scroll_w);
	(void) werase(stat_w);
	(void) werase(turn_w);

	(void) move(BOX_T, BOX_L);
	for (n = 0; n < BOX_X; n++)
		(void) addch('-');
	(void) move(BOX_B, BOX_L);
	for (n = 0; n < BOX_X; n++)
		(void) addch('-');
	for (n = BOX_T+1; n < BOX_B; n++) {
		(void) mvaddch(n, BOX_L, '|');
		(void) mvaddch(n, BOX_R, '|');
	}
	(void) mvaddch(BOX_T, BOX_L, '+');
	(void) mvaddch(BOX_T, BOX_R, '+');
	(void) mvaddch(BOX_B, BOX_L, '+');
	(void) mvaddch(BOX_B, BOX_R, '+');
	(void) refresh();

#define WSaIM "Wooden Ships & Iron Men"
	(void) wmove(view_w, 2, (VIEW_X - sizeof WSaIM - 1) / 2);
	(void) waddstr(view_w, WSaIM);
	(void) wmove(view_w, 4, (VIEW_X - strlen(cc->name)) / 2);
	(void) waddstr(view_w, cc->name);
	(void) wrefresh(view_w);

	(void) move(LINE_T, LINE_L);
	(void) printw("Class %d %s (%d guns) '%s' (%c%c)",
		mc->class,
		classname[mc->class],
		mc->guns,
		ms->shipname,
		colours(ms),
		sterncolour(ms));
	(void) refresh();
}

initscreen()
{
	(void) initscr();
	view_w = newwin(VIEW_Y, VIEW_X, VIEW_T, VIEW_L);
	slot_w = newwin(SLOT_Y, SLOT_X, SLOT_T, SLOT_L);
	scroll_w = newwin(SCROLL_Y, SCROLL_X, SCROLL_T, SCROLL_L);
	stat_w = newwin(STAT_Y, STAT_X, STAT_T, STAT_L);
	turn_w = newwin(TURN_Y, TURN_X, TURN_T, TURN_L);
	done_curses++;
	noecho();
	crmode();
}

centerview()
{
	viewrow = mf->row - VIEW_Y / 2;
	viewcol = mf->col - VIEW_X / 2;
}

upview()
{
	viewrow -= VIEW_Y / 3;
}

downview()
{
	viewrow += VIEW_Y / 3;
}

leftview()
{
	viewcol -= VIEW_X / 5;
}

rightview()
{
	viewcol += VIEW_X / 5;
}

adjustview()
{
	if (dont_adjust)
		return;			/* don't adjust if out of sight */
	if (mf->row < viewrow + VIEW_Y/4)
		viewrow = mf->row - (VIEW_Y - VIEW_Y/4);
	else if (mf->row > viewrow + (VIEW_Y - VIEW_Y/4))
		viewrow = mf->row - VIEW_Y/4;
	if (mf->col < viewcol + VIEW_X/8)
		viewcol = mf->col - (VIEW_X - VIEW_X/8);
	else if (mf->col > viewcol + (VIEW_X - VIEW_X/8))
		viewcol = mf->col - VIEW_X/8;
}
