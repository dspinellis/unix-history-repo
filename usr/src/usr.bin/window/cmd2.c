#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.22 84/01/16";
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
	":unset variable       Deallocate ``variable''.",
	":window row col nrow ncol [nline label]",
	"                      Open a window at ``row'', ``col''",
	"                      of size ``nrow'', ``ncol'',",
	"                      with ``nline'', and ``label''.",
	":write {1-9} string   Write ``string'' to window {1-9}.",
	0
};

c_help()
{
	register struct ww *w;

	if ((w = openiwin(wwnrow - 3, "Help")) == 0) {
		error("Can't open help window: %s.", wwerror());
		return;
	}
	(void) wwprintf(w, "The escape character is %s, which gets you into command mode.\n\n",
		unctrl(escapec));
	if (help_print(w, "Short commands", help_shortcmd) >= 0)
		(void) help_print(w, "Long commands", help_longcmd);
	closeiwin(w);
}

help_print(w, name, list)
register struct ww *w;
char *name;
register char **list;
{
	(void) wwprintf(w, "%s:\n\n", name);
	while (*list)
		switch (more(w, 0)) {
		case 0:
			(void) wwputs(*list++, w);
			(void) wwputc('\n', w);
			break;
		case 1:
			(void) wwprintf(w, "%s: (continued)\n\n", name);
			break;
		case 2:
			return -1;
		}
	return more(w, 1) == 2 ? -1 : 0;
}

#ifndef O_4_1A
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
#endif

c_stat()
{
	register struct ww *w;

	if ((w = openiwin(8, "IO Statistics")) == 0) {
		error("Can't open statistics window: %s.", wwerror());
		return;
	}
	(void) wwprintf(w, "nwrite\tnwritec\tnupdate\tnupdlin\tnupdmis\tnmajlin\tnmajmis\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
		wwnwrite, wwnwritec,
		wwnupdate, wwnupdline, wwnupdmiss, wwnmajline, wwnmajmiss);
	(void) wwprintf(w, "nsel\tnselz\tnsele\tnread\tnreadz\tnreade\tnreadc\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
		wwnselect, wwnselectz, wwnselecte,
		wwnread, wwnreadz, wwnreade, wwnreadc);
	(void) wwprintf(w, "nwread\tnwreadz\tnwreade\tnwreadd\tnwreadc\tnwreadp\n");
	(void) wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\n",
		wwnwread, wwnwreadz, wwnwreade,
		wwnwreadd, wwnwreadc, wwnwreadp);
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
	if (n == 0) {
		error("No windows.");
		return;
	}
	if ((w = openiwin(n + 2, "Windows")) == 0) {
		error("Can't open listing window: %s.", wwerror());
		return;
	}
	for (i = 0; i < NWINDOW; i++) {
		if ((wp = window[i]) == 0)
			continue;
		(void) wwprintf(w, "%c %c %-13s %-.*s\n",
			wp == selwin ? '*' : ' ',
			i + '1',
			wp->ww_state == WWS_HASPROC ? "" : "(No process)",
			wwncol - 20,
			wp->ww_label ? wp->ww_label : "(No label)");
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
	while (wwpeekc() < 0)
		wwiomux();
	if (wwgetc() == 'y') {
		(void) wwputs("Yes", cmdwin);
		quit++;
	} else
		(void) wwputs("\r\n", cmdwin);
	if (terse && !quit)
		wwdelete(cmdwin);
}
