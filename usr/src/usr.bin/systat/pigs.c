/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pigs.c	5.9 (Berkeley) %G%";
#endif not lint

/*
 * Pigs display from Bill Reeves at Lucasfilm
 */

#include "systat.h"
#include <sys/dir.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <sys/kinfo.h>
#include <sys/kinfo_proc.h>
#include <pwd.h>
#include <kvm.h>

struct p_times {
	float   pt_pctcpu;
	struct  proc *pt_pp;
} *pt;
int	ptsize;
int	nproc;
int     fscale;
double  lccpu;


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
char    pidname[30];
long	stime[CPUSTATES];

showpigs()
{
	register short auid;
	register int i, j, y;
	register float max;
	register struct p_times *ptptr;
	struct	p_times temppt;
	struct	eproc *ep;
	char *uname, *pname;

	if (pt == NULL)
		return;
	/* Accumulate the percent of cpu per user. */
	ptptr = pt;
	total = 0.0;
	for (i = 0; i < nproc; i++) {
		/* Accumulate the percentage. */
		total += ptptr->pt_pctcpu;
		ptptr++;
	}

	total += pt[nproc].pt_pctcpu;	/* idle "process" */

	if (total < 1.0)
		total = 1.0;
	factor = 50.0/total;

	/* Find the top few by executing a "bubble pass" ten times. */
	y = nproc + 1;
	if (y > wnd->_maxy-1)
		y = wnd->_maxy-1;
	for (i = 0; i < y; i++) {
		ptptr = &pt[i];
		max = -10000.0;
		maxind = i;
		for (j = i; j < nproc + 1; j++) {
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
	i = nproc + 1;
	if (i > wnd->_maxy-1)
		i = wnd->_maxy-1;
	for (; i > 0 && ptptr->pt_pctcpu > 0.01; i--, y++, ptptr++) {
		if (ptptr->pt_pp == NULL) {
			uname = "";
			pname = "<idle>";
		}
		else {
			ep = kvm_geteproc(ptptr->pt_pp);
			uname = (char *)user_from_uid(ep->e_ucred.cr_uid, 0);
			pname = ptptr->pt_pp->p_comm;
		}
		wmove(wnd, y, 0);
		wclrtoeol(wnd);
		mvwaddstr(wnd, y, 0, uname);
		sprintf(pidname, "%10.10s", pname, 0);
		mvwaddstr(wnd, y, 9, pidname);
		wmove(wnd, y, 20);
		for (j = ptptr->pt_pctcpu*factor + 0.5; j > 0; j--)
			waddch(wnd, 'X');
	}
	wmove(wnd, y, 0); wclrtobot(wnd);
}

static struct nlist nlst[] = {
#define X_FIRST		0
#define X_CPTIME	0
	{ "_cp_time" },
#define X_CCPU          1
	{ "_ccpu" },
#define X_FSCALE        2
	{ "_fscale" },

	{ "" }
};

initpigs()
{
	fixpt_t ccpu;

	if (nlst[X_FIRST].n_type == 0) {
		kvm_nlist(nlst);
		if (nlst[X_FIRST].n_type == 0) {
			error("namelist failed");
			return(0);
		}
	}
	KREAD(NPTR(X_CPTIME), stime, sizeof (stime));
	NREAD(X_CCPU, &ccpu, LONG);
	NREAD(X_FSCALE,  &fscale, LONG);
	lccpu = log((double) ccpu / fscale);

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

	if (nlst[X_FIRST].n_type == 0)
		return;
	kvm_freeprocs();	/* clear previous buffer */
	if ((nproc = kvm_getprocs(KINFO_PROC_ALL, 0)) == -1) {
		error("%s", kvm_geterr());
		if (pt)
			free(pt);
		ptsize = 0;
		return;
	}
	/*
	 * The nproc + 1'th entry is for the imaginary "idle"
	 * process to which we attribute the unused part of the cpu
	 */
	if ((nproc + 1) * sizeof (struct p_times) > ptsize) {
		if (pt)
			free(pt);
		ptsize = (nproc + 1) * sizeof (struct p_times);
		if ((pt = (struct p_times *)malloc(ptsize)) == NULL) {
			error("out of memory");
			die();
		}
	}
	/*
	 * calculate %cpu for each proc
	 */
	for (prt = pt; (pp = kvm_nextproc()) != NULL; prt++) {
		prt->pt_pp = pp;
		time = pp->p_time;
		if (time == 0 || (pp->p_flag & SLOAD) == 0)
			prt->pt_pctcpu = 0;
		else
			prt->pt_pctcpu = ((double) pp->p_pctcpu / 
					fscale) / (1.0 - exp(time * lccpu));
	}
	/*
	 * and for the imaginary "idle" process, pt[nproc]
	 */
	KREAD(NPTR(X_CPTIME), ctime, sizeof (ctime));
	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += ctime[i] - stime[i];
	if (t == 0.0)
		t = 1.0;
	pt[nproc].pt_pp = NULL;	
	pt[nproc].pt_pctcpu = (ctime[CP_IDLE] - stime[CP_IDLE]) / t;
	for (i = 0; i < CPUSTATES; i++)
		stime[i] = ctime[i];
}

labelpigs()
{
	wmove(wnd, 0, 0); wclrtoeol(wnd);
	mvwaddstr(wnd, 0, 20,
	    "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
}
