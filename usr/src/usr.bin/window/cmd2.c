#ifndef lint
static	char *sccsid = "@(#)cmd2.c	1.1 83/07/18";
#endif

#include "defs.h"

struct ww *getwin();
struct ww *openwin();
char *strtime();

dohelp()
{
	register struct ww *w;

	if ((w = openwin(20, "Help")) == 0)
		return;
	wwprintf(w, "The escape character is ^P, which gets you into command mode.\r\n");
	wwprintf(w, "The commands are:\r\n");
	wwprintf(w, "%%[1-9]  select window [1-9]\r\n");
	wwprintf(w, "[1-9]   select window [1-9] and exit command mode\r\n");
	wwprintf(w, "c[1-9]  close window [1-9]\r\n");
	wwprintf(w, "C       close all empty windows\r\n");
	wwprintf(w, "R       force refresh after every newline in current window\r\n");
	wwprintf(w, "r       don't refresh every line\r\n");
	wwprintf(w, "w       open a new window\r\n");
	wwprintf(w, "s       print IO statistics\r\n");
	wwprintf(w, "t       print resource usage of this program\r\n");
	wwprintf(w, "T       print resource usage of children\r\n");
	wwprintf(w, "escape  exit command mode\r\n");
	wwprintf(w, "^L      redraw screen\r\n");
	wwprintf(w, "^Z      suspend\r\n");
	wwprintf(w, ".       quit\r\n");
	closewin(w);
}

dotime(flag)
{
	register struct ww *w;
	struct rusage rusage;
	struct timeval timeval;

	if ((w = openwin(9, "Time")) == 0)
		return;

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

struct ww *
openwin(nrow, label)
char *label;
{
	register struct ww *w;

	if ((w = wwopen(WW_NONE, 0, nrow, WCols, 1, 0)) == 0) {
		wwputs("\r\nCan't open help window.  ", cmdwin);
		return 0;
	}
	wwframe(w);
	wwlabel(w, label, WINVERSE);
	wwsetcurrent(w);
	return w;
}

closewin(w)
register struct ww *w;
{
	wwprintf(w, "\r\nType return to continue: ");
	wwsetcursor(WCurRow(w->ww_win), WCurCol(w->ww_win));
	while (bgetc() < 0) {
		wwflush();
		bread();
	}
	wwclose(w);
	wwsetcurrent(cmdwin);
}
