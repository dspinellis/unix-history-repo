/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pigs.c	5.7 (Berkeley) 5/29/89";
#endif not lint

/*
 * Pigs display from Bill Reeves at Lucasfilm
 */

#include "systat.h"
#include <sys/dir.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <pwd.h>
#include <paths.h>

WINDOW *
openpigs()
{
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

closepigs(w)
	WINDOW *w;
{
	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

int	maxind;
int     factor;
float   total;
struct  passwd *getpwuid();
char    pidname[30];
long	stime[CPUSTATES];
double	idle;

showpigs()
{
	register short auid;
	register int i, j, y;
	register float max;
	register struct p_times *ptptr;
	struct p_times temppt;
	register struct users *knptr;
	char *getpname(), *pnamp;

	if (pt == NULL)
		return;
	/* Accumulate the percent of cpu per user. */
	ptptr = pt;
	numprocs = 0;
	total = 0.0;
	for (i = 0; i < nproc; i++) {
		/* discard inactive processes */
		if (ptptr->pt_uid == -1) {
			ptptr++;
			continue;
		}
		/* Accumulate the percentage. */
		total += ptptr->pt_pctcpu;
		numprocs++;
		ptptr++;
	}

	pt[numprocs].pt_pctcpu = idle;
	total += idle;
	pt[numprocs].pt_uid = -1;
	pt[numprocs].pt_pid = -1;
	pt[numprocs].pt_pp = NULL;

	if (total < 1.0)
		total = 1.0;
	factor = 50.0/total;

	/* Find the top few by executing a "bubble pass" ten times. */
	y = numprocs + 1;
	if (y > wnd->_maxy-1)
		y = wnd->_maxy-1;
	for (i = 0; i < y; i++) {
		ptptr = &pt[i];
		max = -10000.0;
		maxind = i;
		for (j = i; j < numprocs + 1; j++) {
			if (ptptr->pt_pctcpu > max) {
				max = ptptr->pt_pctcpu;
				maxind = j;
			}
			ptptr++;
		}
		if (maxind != i) {
			temppt = pt[i];
			pt[i] = pt[maxind];
			pt[maxind] = temppt;
		}
	}
	y = 1;
	ptptr = pt;
	i = numprocs + 1;
	if (i > wnd->_maxy-1)
		i = wnd->_maxy-1;
	for (; i > 0 && ptptr->pt_pctcpu > 0.01; i--) {
		/* Find the user's name. */
		knptr = known;
		auid = ptptr->pt_uid;
		for (j = numknown - 1; j >= 0; j--) {
			if (knptr->k_uid == auid) {
				namp = knptr->k_name;
				break;
			}
			knptr++;
		}
		if (j < 0) {
			if (numknown < 30) {
				knptr = &known[numknown];
				namp = strncpy(knptr->k_name,
				    getpwuid(auid)->pw_name, 15);
				knptr->k_name[15] = '\0';
				knptr->k_uid = auid;
				numknown++;
			} else
				namp = getpwuid(auid)-> pw_name;
		}
		pnamp = getpname(ptptr->pt_pid, ptptr->pt_pp);
		wmove(wnd, y, 0);
		wclrtoeol(wnd);
		mvwaddstr(wnd, y, 0, namp);
		sprintf(pidname, "%10.10s", pnamp);
		mvwaddstr(wnd, y, 9, pidname);
		wmove(wnd, y++, 20);
		for (j = ptptr->pt_pctcpu*factor + 0.5; j > 0; j--)
			waddch(wnd, 'X');
		ptptr++;
	}
	wmove(wnd, y, 0); wclrtobot(wnd);
}

static struct nlist nlst[] = {
#define X_PROC          0
	{ "_proc" },
#define X_NPROC         1
	{ "_nproc" },
#define X_USRPTMAP      2
	{ "_Usrptmap" },
#define X_USRPT         3
	{ "_usrpt" },
#define X_CPTIME	4
	{ "_cp_time" },
	{ "" }
};

initpigs()
{
	if (nlst[X_PROC].n_type == 0) {
		nlist(_PATH_UNIX, nlst);
		if (nlst[X_PROC].n_type == 0) {
			error("namelist on %s failed", _PATH_UNIX);
			return(0);
		}
	}
	if (procp == NULL) {
		procp = getw(nlst[X_PROC].n_value);
		nproc = getw(nlst[X_NPROC].n_value);
	}
	if (kprocp == NULL)
		kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
	if (usrpt != NULL)
		return(1);
	usrpt = (struct pte *)nlst[X_USRPT].n_value;
	Usrptma = (struct pte *)nlst[X_USRPTMAP].n_value;
	if (pt == NULL)
		pt = (struct p_times *)calloc(nproc, sizeof (struct p_times));
	lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
	read(kmem, stime, sizeof stime);
	return(1);
}

fetchpigs()
{
	register int i;
	register struct p_times *prt;
	register float time;
	register struct proc *pp;
	long ctime[CPUSTATES];
	double t;

	if (nlst[X_PROC].n_type == 0)
		return;
	if (kprocp == NULL) {
		kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
		if (kprocp == NULL)
			return;
	}
	if (pt == NULL) {
		pt = (struct p_times *)calloc(nproc, sizeof (struct p_times));
		if (pt == NULL)
			return;
	}
	prt = pt;
	lseek(kmem, procp, L_SET);
	read(kmem, kprocp, sizeof (struct proc) * nproc);
	for (i = 0, pp = kprocp; i < nproc; i++, pp++) {
		time = pp->p_time;
		if (time == 0 || (pp->p_flag & SLOAD) == 0)
			continue;
		prt->pt_pid = pp->p_pid;
		prt->pt_pp = pp;
		prt->pt_pctcpu = ((double) pp->p_pctcpu / fscale) /
				(1.0 - exp(time * lccpu));
		prt->pt_uid = pp->p_uid;
		prt++;
	}
	for (; prt < &pt[nproc]; prt++)
		prt->pt_uid = -1;
	lseek(kmem, (long)nlst[X_CPTIME].n_value, L_SET);
	read(kmem, ctime, sizeof ctime);
	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += ctime[i] - stime[i];
	if (t == 0.0)
		t = 1.0;
	idle = (ctime[CP_IDLE] - stime[CP_IDLE]) / t;
	for (i = 0; i < CPUSTATES; i++)
		stime[i] = ctime[i];
}

labelpigs()
{
	wmove(wnd, 0, 0); wclrtoeol(wnd);
	mvwaddstr(wnd, 0, 20,
	    "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
}
