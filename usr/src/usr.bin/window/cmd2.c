#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.23 84/03/03";
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
	if (help_print(w, "Short commands", help_shortcmd) >= 0)
		(void) help_print(w, "Long commands", help_longcmd);
	closewin(w);
}

help_print(w, name, list)
register struct ww *w;
char *name;
register char **list;
{
	wwprintf(w, "%s:\n\n", name);
	while (*list)
		switch (more(w, 0)) {
		case 0:
			wwputs(*list++, w);
			wwputc('\n', w);
			break;
		case 1:
			wwprintf(w, "%s: (continued)\n\n", name);
			break;
		case 2:
			return -1;
		}
	return more(w, 1) == 2 ? -1 : 0;
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
	wwprintf(w, "ttflush\twrite\terror\tzero\tchar\n");
	wwprintf(w, "%d\t%d\t%d\t%d\t%d\n",
		wwnflush, wwnwr, wwnwre, wwnwrz, wwnwrc);
	wwprintf(w, "wwwrite\tattmpt\tchar\n");
	wwprintf(w, "%d\t%d\t%d\n",
		wwnwwr, wwnwwra, wwnwwrc);
	wwprintf(w, "wwupdat\tline\tmiss\tmajor\tmiss\n");
	wwprintf(w, "%d\t%d\t%d\t%d\t%d\n",
		wwnupdate, wwnupdline, wwnupdmiss, wwnmajline, wwnmajmiss);
	wwprintf(w, "select\terror\tzero\n");
	wwprintf(w, "%d\t%d\t%d\n",
		wwnselect, wwnselecte, wwnselectz);
	wwprintf(w, "read\terror\tzero\tchar\n");
	wwprintf(w, "%d\t%d\t%d\t%d\n",
		wwnread, wwnreade, wwnreadz, wwnreadc);
	wwprintf(w, "ptyread\terror\tzero\tcontrol\tdata\tchar\n");
	wwprintf(w, "%d\t%d\t%d\t%d\t%d\t%d\n",
		wwnwread, wwnwreade, wwnwreadz,
		wwnwreadp, wwnwreadd, wwnwreadc);
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
	while (wwpeekc() < 0)
		wwiomux();
	if (wwgetc() == 'y') {
		wwputs("Yes", cmdwin);
		quit++;
	} else
		wwputs("\r\n", cmdwin);
	if (terse)
		Whide(cmdwin->ww_win);
}
