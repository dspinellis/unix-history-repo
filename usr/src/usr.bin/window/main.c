#ifndef lint
static	char *sccsid = "@(#)main.c	1.2 83/07/17";
#endif

#include "ww.h"
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define ESCAPE CTRL(p)

int nread = 0;
struct timeval starttime;
struct timezone timezone;

main()
{
	register n;
	register char *p;
	struct ww *w1, *w2;
	int wwchild();
	int imask;
	char buf[512];
	char escape = 0;
	char c;
	struct rusage rusage;

	if (wwinit() < 0) {
		fflush(stdout);
		fprintf("Can't do windows on this terminal.\n");
		exit(1);
	}
	n = WRows / 2;
	if ((w1 = wwopen(1, n, WCols, 0, 0)) == 0
	    || (w2 = wwopen(2, WRows - n, WCols, n, 0)) == 0) {
		fflush(stdout);
		fprintf(stderr, "Can't open windows.\n");
		goto bad;
	}
	wwsetcurrent(w1);
	(void) signal(SIGCHLD, wwchild);
	switch (wwfork(w1)) {
	case -1:
		perror("wwfork");
		goto bad;
	case 0:
		execl("/bin/csh", "csh", 0);
		perror("execl(/bin/csh)");
		exit(1);
	}
	switch (wwfork(w2)) {
	case -1:
		perror("wwfork");
		goto bad;
	case 0:
		execl("/bin/csh", "csh", 0);
		perror("execl(/bin/csh)");
		exit(1);
	}
	gettimeofday(&starttime, &timezone);
	wwputstr("Type ^P? for help.\r\n");
	for (;;) {
		wwflush();
		if (!wwhaschildren())
			break;
		while (imask = 1<<0, wwforce(&imask) < 0)
			;
		if ((imask & 1<<0) == 0)
			continue;
		n = read(0, buf, sizeof buf);
		for (p = buf; n-- > 0; p++) {
			nread++;
			*p &= 0x7f;
			if (!escape) {
				if (*p == ESCAPE)
					escape++;
				else
					write(curwin->ww_pty, p, 1);
				continue;
			}
			escape = 0;
			switch (*p) {
			case ESCAPE:
				write(curwin->ww_pty, p, 1);
				break;
			case '1':
				wwsetcurrent(w1);
				break;
			case '2':
				wwsetcurrent(w2);
				break;
			case CTRL(z):
				wwsuspend();
				break;
			case '.':
				goto out;
			/*
			case CTRL(\\):
				wwend();
				kill(getpid(), SIGILL);
				break;
			*/
			case CTRL(l):
				ScreenGarbaged = 1;
				break;
			case 'r':
				curwin->ww_refresh = 0;
				break;
			case 'R':
				curwin->ww_refresh = 1;
				break;
			case 's':
				wwprintf(curwin, "\r\nread: %d, write: %d\r\n",
					nread, wwnwrite);
				break;
			case 't':
				printrusage(curwin, RUSAGE_SELF);
				break;
			case 'T':
				printrusage(curwin, RUSAGE_CHILDREN);
				break;
			case '?':
				help();
				break;
			default:
				c = ESCAPE;
				write(curwin->ww_pty, &c, 1);
				write(curwin->ww_pty, p, 1);
			}
		}
	}
out:
bad:
	wwend();
	return 0;
}

help()
{
	wwputstr("\r\n");
	wwputstr("^P1   goto top window\r\n");
	wwputstr("^P2   goto bottom window\r\n");
	wwputstr("^PR   force refresh after every newline\r\n");
	wwputstr("^Pr   don't refresh every line\r\n");
	wwputstr("^Ps   print io statistics\r\n");
	wwputstr("^Pt   print resource usage of this program\r\n");
	wwputstr("^PT   print resource usage of children\r\n");
	wwputstr("^P^L  redraw screen\r\n");
	wwputstr("^P^Z  suspend\r\n");
	wwputstr("^P.   quit\r\n");
}

printrusage(w, flag)
register struct ww *w;
{
	struct rusage rusage;
	struct timeval timeval;
	char *strtime();

	gettimeofday(&timeval, &timezone);
	timeval.tv_sec -= starttime.tv_sec;
	if ((timeval.tv_usec -= starttime.tv_usec) < 0) {
		timeval.tv_sec--;
		timeval.tv_usec += 1000000;
	}
	getrusage(flag, &rusage);

	wwprintf(w, "\r\ntime\t\tutime\tstime\tmaxrss\tixrss\tidrss\tisrss\r\n");
	wwprintf(w, "%-16s", strtime(&timeval));
	wwprintf(w, "%-8s", strtime(&rusage.ru_utime));
	wwprintf(w, "%-8s", strtime(&rusage.ru_stime));
	wwprintf(w, "%D\t%D\t%D\t%D\r\n",
		rusage.ru_maxrss, rusage.ru_ixrss,
		rusage.ru_idrss, rusage.ru_isrss);
	wwprintf(w, "minflt\tmajflt\tnswap\tinblk\toublk\tmsgsnd\tmsgrcv\tnsigs\tnvcsw\tnivcsw\r\n");
	wwprintf(w, "%D\%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\r\n",
		rusage.ru_minflt, rusage.ru_majflt, rusage.ru_nswap,
		rusage.ru_inblock, rusage.ru_oublock,
		rusage.ru_msgsnd, rusage.ru_msgrcv, rusage.ru_nsignals,
		rusage.ru_nvcsw, rusage.ru_nivcsw);
}

char *
strtime(t)
register struct timeval *t;
{
	char fill = 0;
	static char buf[20];
	register char *p = buf;

	if (t->tv_sec > 60*60) {
		sprintf(p, "%D:", t->tv_sec / 60*60);
		while (*p++)
			;
		p--;
		t->tv_sec %= 60*60;
		fill++;
	}
	if (t->tv_sec > 60) {
		sprintf(p, fill ? "%02D:" : "%D:", t->tv_sec / 60, ':');
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
