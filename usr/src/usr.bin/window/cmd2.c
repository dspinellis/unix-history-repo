#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.12 83/09/14";
#endif

#include "defs.h"

c_help()
{
	register struct ww *w;

	if ((w = openiwin(wwnrow - 3, "Help")) == 0) {
		error("Can't open help window: %s.", wwerror());
		return;
	}
	(void) wwprintf(w, "The escape character is %s, which gets you into command mode.\n\n",
		unctrl(escapec));
	(void) wwprintf(w, "Short commands:\n\n");
	(void) wwprintf(w, "{1-9}   Select window {1-9} and return to conversation mode.\n");
	(void) wwprintf(w, "%%{1-9}  Select window {1-9} but stay in command mode.\n");
	(void) wwprintf(w, "escape  Return to conversation mode and don't change the current window.\n");
	(void) wwprintf(w, "c{1-9}  Close window {1-9}.\n");
	(void) wwprintf(w, "C       Close all windows.\n");
	(void) wwprintf(w, "S       Show all windows in sequence.\n");
	(void) wwprintf(w, "L       List all windows with their labels.\n");
	(void) wwprintf(w, "w       Open a new window.\n");
	(void) wwprintf(w, "{^Y^E}  Scroll {up, down} one line\n");
	(void) wwprintf(w, "{^U^D}  Scroll {up, down} half a window.\n");
	(void) wwprintf(w, "{^B^F}  Scroll {up, down} a full window.\n");
	(void) wwprintf(w, "{hjkl}  Move cursor {left, down, up, right}.\n");
	(void) wwprintf(w, "^L      Redraw screen.\n");
	(void) wwprintf(w, "^Z      Suspend.\n");
	(void) wwprintf(w, "q       Quit.\n");
	waitnl(w);
	(void) wwprintf(w, "Long commands:\n\n");
	(void) wwprintf(w, ":terse [off]              Turn on (or off) terse mode.\n");
	(void) wwprintf(w, ":label {1-9} string       Label window {1-9}.\n");
	(void) wwprintf(w, ":write {1-9} string       Write ``strings'' to window {1-9}.\n");
	(void) wwprintf(w, ":escape C                 Set escape character to C.\n");
	(void) wwprintf(w, ":%%{1-9}                   Select window {1-9}.\n");
	(void) wwprintf(w, ":window r c nr nc [nl]    Open a window at row r column c\n");
	(void) wwprintf(w, "                          with nr rows, nc colomns,\n");
	(void) wwprintf(w, "                          and nl lines in the buffer.\n");
	(void) wwprintf(w, ":buffer lines             Set the default window buffer size.\n");
	(void) wwprintf(w, ":close {1-9}              Close window.\n");
	(void) wwprintf(w, ":source filename          Execute commands in ``filename''.\n");
	waitnl(w);
	closeiwin(w);
}

char *strtime();

c_time(flag)
{
	register struct ww *w;
	struct rusage rusage;
	struct timeval timeval;
	struct timezone timezone;

	if ((w = openiwin(6, "Timing and Resource Usage")) == 0) {
		error("Can't open time window: %s.", wwerror());
		return;
	}

	(void) gettimeofday(&timeval, &timezone);
	timeval.tv_sec -= starttime.tv_sec;
	if ((timeval.tv_usec -= starttime.tv_usec) < 0) {
		timeval.tv_sec--;
		timeval.tv_usec += 1000000;
	}
	(void) getrusage(flag, &rusage);

	(void) wwprintf(w, "time\t\tutime\t\tstime\t\tmaxrss\tixrss\tidrss\tisrss\n");
	(void) wwprintf(w, "%-16s", strtime(&timeval));
	(void) wwprintf(w, "%-16s", strtime(&rusage.ru_utime));
	(void) wwprintf(w, "%-16s", strtime(&rusage.ru_stime));
	(void) wwprintf(w, "%D\t%D\t%D\t%D\n",
		rusage.ru_maxrss, rusage.ru_ixrss,
		rusage.ru_idrss, rusage.ru_isrss);
	(void) wwprintf(w, "minflt\tmajflt\tnswap\tinblk\toublk\tmsgsnd\tmsgrcv\tnsigs\tnvcsw\tnivcsw\n");
	(void) wwprintf(w, "%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\n",
		rusage.ru_minflt, rusage.ru_majflt, rusage.ru_nswap,
		rusage.ru_inblock, rusage.ru_oublock,
		rusage.ru_msgsnd, rusage.ru_msgrcv, rusage.ru_nsignals,
		rusage.ru_nvcsw, rusage.ru_nivcsw);

	waitnl(w);
	closeiwin(w);
}

char *
strtime(t)
register struct timeval *t;
{
	char fill = 0;
	static char buf[20];
	register char *p = buf;

	if (t->tv_sec > 60*60) {
		(void) sprintf(p, "%D:", t->tv_sec / (60*60));
		while (*p++)
			;
		p--;
		t->tv_sec %= 60*60;
		fill++;
	}
	if (t->tv_sec > 60) {
		(void) sprintf(p, fill ? "%02D:" : "%D:", t->tv_sec / 60);
		while (*p++)
			;
		p--;
		t->tv_sec %= 60;
		fill++;
	}
	(void) sprintf(p, fill ? "%02D.%02d" : "%D.%02D",
		t->tv_sec, t->tv_usec / 10000);
	return buf;
}

c_stat()
{
	register struct ww *w;

	if ((w = openiwin(4, "IO Statics")) == 0) {
		error("Can't open statistics window: %s.", wwerror());
		return;
	}
	(void) wwprintf(w, "nread\tnreadz\tnreade\tnreadc\tnwrite\tnwritec\tnupdate\tntouchd\tnmiss\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
		nread, nreadz, nreade, nreadc, wwnwrite, wwnwritec,
		wwnupdate, wwntouched, wwnmiss);
	waitnl(w);
	closeiwin(w);
}

c_list()
{
	register struct ww *w, *wp;
	register i;
	int n;

	for (n = 0, i = 0; i < NWINDOW; i++)
		if (window[i] != 0)
			n++;
	if ((w = openiwin(MAX(n, 1) + 2, "Windows")) == 0) {
		error("Can't open listing window: %s.", wwerror());
		return;
	}
	if (n == 0) {
		(void) wwputs("No windows.\n", w);
	} else {
		for (i = 0; i < NWINDOW; i++) {
			if ((wp = window[i]) == 0)
				continue;
			(void) wwprintf(w, "%c %c %-13s %-.*s\n",
				wp == selwin ? '*' : ' ',
				i + '1',
				wp->ww_state == WWS_HASPROC
					? "" : "(No process)",
				wwncol - 20,
				wp->ww_label
					? wp->ww_label : "(No label)");
		}
	}
	waitnl(w);
	closeiwin(w);
}

c_quit()
{
	if (terse)
		wwadd(cmdwin, &wwhead);
	(void) wwputs("Really quit [yn]? ", cmdwin);
	wwcurtowin(cmdwin);
	while (bpeekc() < 0)
		bread();
	if (bgetc() == 'y') {
		(void) wwputs("Yes", cmdwin);
		quit++;
	} else
		(void) wwputs("\r\n", cmdwin);
	if (terse && !quit)
		wwdelete(cmdwin);
}

/*
 * Open an information window.
 */
struct ww *
openiwin(nrow, label)
char *label;
{
	register struct ww *w;

	if ((w = wwopen(0, nrow, wwncol, 2, 0, 0)) == 0)
		return 0;
	w->ww_mapnl = 1;
	w->ww_hasframe = 1;
	w->ww_id = -1;
	w->ww_center = 1;
	(void) setlabel(w, label);
	wwadd(w, framewin);
	reframe();
	return w;
}

waitnl(w)
register struct ww *w;
{
	front(w);
	(void) wwputs("\nType return to continue: ", w);
	wwcurtowin(w);
	while (bgetc() < 0)
		bread();
	(void) wwputs("\033E", w);		/* clear and home cursor */
}

closeiwin(w)
struct ww *w;
{
	closewin(w);
	reframe();
}
