#ifndef lint
static	char *sccsid = "@(#)lcmd2.c	3.1 84/04/05";
#endif

#include "defs.h"
#include "string.h"
#include "value.h"
#include "lcmd.h"
#include <sys/resource.h>

struct lcmd_arg arg_iostat[] = {
	0
};

l_iostat(v, a)
struct value *v, *a;
{
	register struct ww *w;

	if ((w = openiwin(14, "IO Statistics")) == 0) {
		error("Can't open statistics window: %s.", wwerror());
		return;
	}
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
	closeiwin(w);
}

struct lcmd_arg arg_time[] = {
	{ "who",	1,	V_STR },
	0
};

l_time(v, a)
struct value *v;
register struct value *a;
{
	register struct ww *w;
	struct rusage rusage;
	struct timeval timeval;
	int flag;
	char *strtime();

	if ((w = openiwin(6, "Timing and Resource Usage")) == 0) {
		error("Can't open time window: %s.", wwerror());
		return;
	}

	(void) gettimeofday(&timeval, (struct timezone *)0);
	timeval.tv_sec -= starttime.tv_sec;
	if ((timeval.tv_usec -= starttime.tv_usec) < 0) {
		timeval.tv_sec--;
		timeval.tv_usec += 1000000;
	}
	(void) getrusage(a->v_type == V_STR
			&& str_match(a->v_str, "children", 1)
		? RUSAGE_CHILDREN : RUSAGE_SELF, &rusage);

	wwprintf(w, "time\t\tutime\t\tstime\t\tmaxrss\tixrss\tidrss\tisrss\n");
	wwprintf(w, "%-16s", strtime(&timeval));
	wwprintf(w, "%-16s", strtime(&rusage.ru_utime));
	wwprintf(w, "%-16s", strtime(&rusage.ru_stime));
	wwprintf(w, "%D\t%D\t%D\t%D\n",
		rusage.ru_maxrss, rusage.ru_ixrss,
		rusage.ru_idrss, rusage.ru_isrss);
	wwprintf(w, "minflt\tmajflt\tnswap\tinblk\toublk\tmsgsnd\tmsgrcv\tnsigs\tnvcsw\tnivcsw\n");
	wwprintf(w, "%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\t%D\n",
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
