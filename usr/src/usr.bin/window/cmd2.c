#ifndef lint
static	char *sccsid = "@(#)cmd2.c	1.8 83/07/28";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

dohelp()
{
	register struct ww *w;

	if ((w = openwin(wwnrow - 1, "Help")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open help window.  ", cmdwin);
		return;
	}
	wwprintf(w, "The escape character is %s, which gets you into command mode.\r\n\n",
		unctrl(escapec));
	wwprintf(w, "Short commands:\r\n\n");
	wwprintf(w, "{1-9}   Select window {1-9} and return to conversation mode.\r\n");
	wwprintf(w, "%%{1-9}  Select window {1-9}.\r\n");
	wwprintf(w, "c{1-9}  Close window {1-9}.\r\n");
	wwprintf(w, "C       Close all windows.\r\n");
	wwprintf(w, "S       Show all windows in sequence.\r\n");
	wwprintf(w, "L       List all windows with their labels.\r\n");
	wwprintf(w, "w       Open a new window.\r\n");
	wwprintf(w, "[^U^D]  Scroll [up, down] half a window.\r\n");
	wwprintf(w, "[^B^F]  Scroll [up, down] a full window.\r\n");
	wwprintf(w, "[hjkl]  Move cursor [left, down, up, right].\r\n");
	wwprintf(w, "escape  Exit command mode.\r\n");
	wwprintf(w, "^L      Redraw screen.\r\n");
	wwprintf(w, "^Z      Suspend.\r\n");
	wwprintf(w, ".       Quit.\r\n");
	waitnl(w);
	wwprintf(w, "Long commands:\r\n\n");
	wwprintf(w, ":terse [off]            Turn on (or off) terse mode.\r\n");
	wwprintf(w, ":refresh {1-9} [off]    Turn on (or off) refresh after every newline\r\n");
	wwprintf(w, "                        for window {1-9}.\r\n");
	wwprintf(w, ":label {1-9} string     Label window {1-9}.\r\n");
	wwprintf(w, ":escape c               Set escape character to c.\r\n");
	wwprintf(w, ":%{1-9}                 Select window {1-9}.\r\n");
	wwprintf(w, ":window r c nr nc       Open a window at row r column c\r\n");
	wwprintf(w, "                        with nr rows and nc colomns\r\n");
	waitnl(w);
	closewin(w);
}

dotime(flag)
{
	register struct ww *w;
	struct rusage rusage;
	struct timeval timeval;

	if ((w = openwin(8, "Timing and Resource Usage")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open time window.  ", cmdwin);
		return;
	}

	gettimeofday(&timeval, &timezone);
	timeval.tv_sec -= starttime.tv_sec;
	if ((timeval.tv_usec -= starttime.tv_usec) < 0) {
		timeval.tv_sec--;
		timeval.tv_usec += 1000000;
	}
	getrusage(flag, &rusage);

	wwprintf(w, "time\t\tutime\t\tstime\t\tmaxrss\tixrss\tidrss\tisrss\r\n");
	wwprintf(w, "%-16s", strtime(&timeval));
	wwprintf(w, "%-16s", strtime(&rusage.ru_utime));
	wwprintf(w, "%-16s", strtime(&rusage.ru_stime));
	wwprintf(w, "%D\t%D\t%D\t%D\r\n",
		rusage.ru_maxrss, rusage.ru_ixrss,
		rusage.ru_idrss, rusage.ru_isrss);
	wwprintf(w, "minflt\tmajflt\tnswap\tinblk\toublk\tmsgsnd\tmsgrcv\tnsigs\tnvcsw\tnivcsw\r\n");
	wwprintf(w, "%D\%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\r\n",
		rusage.ru_minflt, rusage.ru_majflt, rusage.ru_nswap,
		rusage.ru_inblock, rusage.ru_oublock,
		rusage.ru_msgsnd, rusage.ru_msgrcv, rusage.ru_nsignals,
		rusage.ru_nvcsw, rusage.ru_nivcsw);

	waitnl(w);
	closewin(w);
}

char *
strtime(t)
register struct timeval *t;
{
	char fill = 0;
	static char buf[20];
	register char *p = buf;

	if (t->tv_sec > 60*60) {
		sprintf(p, "%D:", t->tv_sec / (60*60));
		while (*p++)
			;
		p--;
		t->tv_sec %= 60*60;
		fill++;
	}
	if (t->tv_sec > 60) {
		sprintf(p, fill ? "%02D:" : "%D:", t->tv_sec / 60);
		while (*p++)
			;
		p--;
		t->tv_sec %= 60;
		fill++;
	}
	sprintf(p, fill ? "%02D.%02d" : "%D.%02D",
		t->tv_sec, t->tv_usec / 10000);
	return buf;
}

dostat()
{
	register struct ww *w;

	if ((w = openwin(6, "IO Statics")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open statistics window.  ", cmdwin);
		return;
	}
	wwprintf(w, "nread\tnreadz\tnreade\tnreadc\tnwrite\tnwritec\r\n");
	wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\r\n",
		nread, nreadz, nreade, nreadc, wwnwrite, wwnwritec);
	waitnl(w);
	closewin(w);
}

dolist()
{
	register struct ww *w, *w1;
	int id;
	char doneit = 0;

	if ((w = openwin(14, "Active Windows")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open listing window.  ", cmdwin);
		return;
	}
	for (id = 1; id <= NWINDOW; id++) {
		if ((w1 = wwfind(id)) == 0)
			continue;
		doneit = 1;
		wwprintf(w, "%d   %s\r\n", id, w1->ww_label);
	}
	if (!doneit)
		wwprintf(w, "No windows.\r\n");
	waitnl(w);
	closewin(w);
}

doquit()
{
	if (terse)
		Wunhide(cmdwin->ww_win);
	wwputs("Really quit [yn]? ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while (bpeekc() < 0)
		bread();
	if (bgetc() == 'y') {
		wwputs("Yes", cmdwin);
		quit++;
	} else
		wwputs("\r\n", cmdwin);
	if (terse)
		Whide(cmdwin->ww_win);
}

struct ww *
openwin(nrow, label)
char *label;
{
	register struct ww *w;
	int startcol;

	if ((w = wwopen(WW_NONE, 0, nrow, wwncol, 0, 0)) == 0)
		return 0;
	wwframe(w);
	if ((startcol = (wwncol - strlen(label)) / 2) <= 0)
		startcol = 1;
	wwlabel(w, startcol, label, WINVERSE);
	wwsetcurwin(w);
	return w;
}

waitnl(w)
register struct ww *w;
{
	wwsetcurwin(w);
	wwprintf(w, "\r\nType return to continue: ");
	wwsetcursor(WCurRow(w->ww_win), WCurCol(w->ww_win));
	while (bgetc() < 0)
		bread();
	wwputs("\033E", w);			/* clear and home cursor */
}

closewin(w)
register struct ww *w;
{
	wwclose(w);
	wwsetcurwin(cmdwin);
}
