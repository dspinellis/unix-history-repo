#ifndef lint
static	char *sccsid = "@(#)cmd2.c	2.1.1.1 83/08/09";
#endif

#include "defs.h"

struct ww *openiwin();

c_help()
{
	register struct ww *w;

	if ((w = openiwin(wwnrow - 2, "Help")) == 0) {
		error("Can't open help window.");
		return;
	}
	(void) wwprintf(w, "The escape character is %s, which gets you into command mode.\r\n\n",
		unctrl(escapec));
	(void) wwprintf(w, "Short commands:\r\n\n");
	(void) wwprintf(w, "{1-9}   Select window {1-9} and return to conversation mode.\r\n");
	(void) wwprintf(w, "%%{1-9}  Select window {1-9}.\r\n");
	(void) wwprintf(w, "c{1-9}  Close window {1-9}.\r\n");
	(void) wwprintf(w, "C       Close all windows.\r\n");
	(void) wwprintf(w, "S       Show all windows in sequence.\r\n");
	(void) wwprintf(w, "L       List all windows with their labels.\r\n");
	(void) wwprintf(w, "w       Open a new window.\r\n");
	(void) wwprintf(w, "[^U^D]  Scroll [up, down] half a window.\r\n");
	(void) wwprintf(w, "[^B^F]  Scroll [up, down] a full window.\r\n");
	(void) wwprintf(w, "[hjkl]  Move cursor [left, down, up, right].\r\n");
	(void) wwprintf(w, "escape  Exit command mode.\r\n");
	(void) wwprintf(w, "^L      Redraw screen.\r\n");
	(void) wwprintf(w, "^Z      Suspend.\r\n");
	(void) wwprintf(w, ".       Quit.\r\n");
	waitnl(w);
	(void) wwprintf(w, "Long commands:\r\n\n");
	(void) wwprintf(w, ":terse [off]            Turn on (or off) terse mode.\r\n");
	(void) wwprintf(w, ":refresh {1-9} [off]    Turn on (or off) refresh after every newline\r\n");
	(void) wwprintf(w, "                        for window {1-9}.\r\n");
	(void) wwprintf(w, ":label {1-9} string     Label window {1-9}.\r\n");
	(void) wwprintf(w, ":escape C               Set escape character to C.\r\n");
	(void) wwprintf(w, ":%%{1-9}                 Select window {1-9}.\r\n");
	(void) wwprintf(w, ":window r c nr nc       Open a window at row r column c\r\n");
	(void) wwprintf(w, "                        with nr rows and nc colomns\r\n");
	(void) wwprintf(w, ":source filename        Execute the commands in `filename'.\r\n");
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
		error("Can't open time window.");
		return;
	}

	(void) gettimeofday(&timeval, &timezone);
	timeval.tv_sec -= starttime.tv_sec;
	if ((timeval.tv_usec -= starttime.tv_usec) < 0) {
		timeval.tv_sec--;
		timeval.tv_usec += 1000000;
	}
	(void) getrusage(flag, &rusage);

	(void) wwprintf(w, "time\t\tutime\t\tstime\t\tmaxrss\tixrss\tidrss\tisrss\r\n");
	(void) wwprintf(w, "%-16s", strtime(&timeval));
	(void) wwprintf(w, "%-16s", strtime(&rusage.ru_utime));
	(void) wwprintf(w, "%-16s", strtime(&rusage.ru_stime));
	(void) wwprintf(w, "%D\t%D\t%D\t%D\r\n",
		rusage.ru_maxrss, rusage.ru_ixrss,
		rusage.ru_idrss, rusage.ru_isrss);
	(void) wwprintf(w, "minflt\tmajflt\tnswap\tinblk\toublk\tmsgsnd\tmsgrcv\tnsigs\tnvcsw\tnivcsw\r\n");
	(void) wwprintf(w, "%D\%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\r\n",
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

	if ((w = openiwin(6, "IO Statics")) == 0) {
		error("Can't open statistics window.");
		return;
	}
	(void) wwprintf(w, "nread\tnreadz\tnreade\tnreadc\tnwrite\tnwritec\r\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\r\n",
		nread, nreadz, nreade, nreadc, wwnwrite, wwnwritec);
	waitnl(w);
	closeiwin(w);
}

c_list()
{
	register struct ww *w;
	register i;
	int n;

	for (n = 0, i = 0; i < NWINDOW; i++)
		if (window[i] != 0)
			n++;
	if ((w = openiwin(MIN(n, 1) + 2, "Active Windows")) == 0) {
		error("Can't open listing window.");
		return;
	}
	if (n == 0) {
		(void) wwputs("No windows.\r\n", w);
	} else {
		for (i = 0; i < NWINDOW; i++) {
			if (window[i] == 0)
				continue;
			(void) wwprintf(w, "%c   %s\r\n", i + '1',
				window[i]->ww_label ? window[i]->ww_label
					: "(No label)");
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
	/*
	w->ww_mapnl = 1;
	*/
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
	if (w->ww_back != framewin) {
		(void) wwputs("reframed", w);
		wwdelete(w);
		wwadd(w, framewin);
		reframe();
	}
	(void) wwputs("\r\nType return to continue: ", w);
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
