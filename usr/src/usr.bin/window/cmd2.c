#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.17 84/01/11";
#endif

#include "defs.h"

char *help_shortcmd[] = {
	"{1-9}   Select window {1-9} and return to conversation mode.",
	"%{1-9}  Select window {1-9} but stay in command mode.",
	"escape  Return to conversation mode",
	"        and don't change the current window.",
	"^^      Return to conversation mode",
	"        and change to previously selected window.",
	"c{1-9}  Close window {1-9}.",
	"C       Close all windows.",
	"S       Show all windows in sequence.",
	"L       List all windows with their labels.",
	"w       Open a new window.",
	"m{1-9}  Move window {1-9}.",
	"M{1-9}  Move window {1-9} to previous position.",
	"v       List all variables.",
	"{^Y^E}  Scroll {up, down} one line",
	"{^U^D}  Scroll {up, down} half a window.",
	"{^B^F}  Scroll {up, down} a full window.",
	"{hjkl}  Move cursor {left, down, up, right}.",
	"^L      Redraw screen.",
	"^Z      Suspend.",
	"q       Quit.",
	0
};
char *help_longcmd[] = {
	":%{1-9}               Select window {1-9}.",
	":buffer lines         Set the default window buffer size.",
	":close {1-9}          Close window.",
	":cursor modes         Set the cursor modes.",
	":escape C             Set escape character to C.",
	":label {1-9} string   Label window {1-9}.",
	":source filename      Execute commands in ``filename''.",
	":terse [off]          Turn on (or off) terse mode.",
	":window row col nrow ncol [nline label]",
	"                      Open a window at ``row'', ``col''",
	"                      of size ``nrow'', ``ncol'',",
	"                      with ``nline'', and ``label''.",
	":write {1-9} string   Write ``string'' to window {1-9}.",
	0
};

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
	help_print(w, "Short commands", help_shortcmd);
	help_print(w, "Long commands", help_longcmd);
	closewin(w);
}

help_print(w, name, list)
register struct ww *w;
char *name;
char **list;
{
	register char **p;
	char firsttime = 1;

	for (p = list; *p;) {
		(void) wwprintf(w, "%s:%s\n\n",
			name, firsttime ? "" : " (continued)");
		firsttime = 0;
		while (*p && w->ww_cur.r < w->ww_w.b - 2) {
			(void) wwputs(*p++, w);
			(void) wwputc('\n', w);
		}
		waitnl(w);
		(void) wwputs("\033E", w);	/* clear and home cursor */
	}
}

#ifndef O_4_1A
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
#endif

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
	(void) wwprintf(w, "nread\tnreadz\tnreade\tnreadc\tnwrite\tnwritec\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\n",
		nread, nreadz, nreade, nreadc, wwnwrite, wwnwritec);
	(void) wwprintf(w, "nupdate\tnupdlin\tnupdmis\tnmajlin\tnmajmis\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\n",
		wwnupdate, wwnupdline, wwnupdmiss, wwnmajline, wwnmajmiss);
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

/*VARARGS2*/
more(w, fmt, a, b, c, d, e)
register struct ww *w;
char *fmt;
{
	if (w->ww_cur.r > w->ww_w.b - 3) {
		waitnl(w);
		(void) wwputs("\033E", w);
	}
	(void) wwprintf(w, fmt, a, b, c, d, e);
}

closewin(w)
register struct ww *w;
{
	wwclose(w);
	wwsetcurwin(cmdwin);
}
