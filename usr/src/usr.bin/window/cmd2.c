#ifndef lint
static	char *sccsid = "@(#)cmd2.c	1.6 83/07/27";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

dohelp()
{
	register struct ww *w;

	if ((w = openwin(22, "Help")) == 0) {
		wwputs("Can't open help window.  ", cmdwin);
		return;
	}
	wwprintf(w, "The escape character is ^P, which gets you into command mode.\r\n");
	wwprintf(w, "The commands are:\r\n");
	wwprintf(w, "[1-9]   Select window [1-9] and exit command mode.\r\n");
	wwprintf(w, "%%[1-9]  Select window [1-9].\r\n");
	wwprintf(w, "c[1-9]  Close window [1-9].\r\n");
	wwprintf(w, "C       Close all empty windows.\r\n");
	wwprintf(w, "Z       Close all windows.\r\n");
	wwprintf(w, "Q       Show all windows in sequence.\r\n");
	wwprintf(w, "R       Force refresh after every newline (current window only).\r\n");
	wwprintf(w, "r       Don't refresh every line.\r\n");
	wwprintf(w, "w       Open a new window.\r\n");
	wwprintf(w, "^U      Scroll up.\r\n");
	wwprintf(w, "^D      Scroll down.\r\n");
	wwprintf(w, "[hjkl]  Move cursor [left, down, up, right].\r\n");
	/*
	wwprintf(w, "s       Print IO statistics.\r\n");
	wwprintf(w, "t       Print resource usage of this program.\r\n");
	wwprintf(w, "T       Print resource usage of children.\r\n");
	*/
	wwprintf(w, "escape  Exit command mode.\r\n");
	wwprintf(w, "^L      Redraw screen.\r\n");
	wwprintf(w, "^Z      Suspend.\r\n");
	wwprintf(w, ".       Quit.\r\n");
	waitnl(w);
	closewin(w);
}

dotime(flag)
{
	register struct ww *w;
	struct rusage rusage;
	struct timeval timeval;

	if ((w = openwin(8, "Timing and Resource Usage")) == 0) {
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
		wwputs("Can't open statistics window.  ", cmdwin);
		return;
	}
	wwprintf(w, "nread\tnreadz\tnreade\tnreadc\tnwrite\tnwritec\r\n");
	wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\r\n",
		nread, nreadz, nreade, nreadc, wwnwrite, wwnwritec);
	waitnl(w);
	closewin(w);
}

doquit()
{
	wwputs("Really quit [yn]? ", cmdwin);
	wwsetcursor(WCurRow(cmdwin->ww_win), WCurCol(cmdwin->ww_win));
	while (bpeekc() < 0)
		bread();
	if (bgetc() == 'y') {
		wwputs("Yes", cmdwin);
		quit++;
	} else
		wwputs("\r\n", cmdwin);
}

struct ww *
openwin(nrow, label)
char *label;
{
	register struct ww *w;

	if ((w = wwopen(WW_NONE, 0, nrow, wwncol, 1, 0)) == 0)
		return 0;
	wwframe(w);
	wwlabel(w, label, WINVERSE);
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
}

closewin(w)
register struct ww *w;
{
	wwclose(w);
	wwsetcurwin(cmdwin);
}
