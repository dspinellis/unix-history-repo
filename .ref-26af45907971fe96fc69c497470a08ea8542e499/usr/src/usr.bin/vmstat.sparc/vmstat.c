/*
 * Copyright (c) 1992, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)vmstat.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/device.h>
#include <sys/disklabel.h>
#include <sys/disk.h>
#include <sys/time.h>
#include <sys/dkstat.h>
#include <sys/ioctl.h>
#include <sys/sysctl.h>
#include <vm/vm.h>

#include <errno.h>
#include <kvm.h>
#include <limits.h>
#include <nlist.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "extern.h"

int	hz, hdrcnt, winlines;

struct {
	long	old[CPUSTATES];		/* previous cp_time from kernel */
	long	delta[CPUSTATES];	/* delta between current & prev */
	long	total;			/* sum of deltas */
} cputime;

void	cpustats __P((void));
void	dkstats __P((void));
void	getcputime __P(());
int	getwinsize __P((void));
void	needhdr __P((int));
void	printhdr __P((void));

static struct nlist nl[] = {
	{ "_cnt" },
#define	X_CNT		0
	{ "_cp_time" },
#define	X_CPTIME	1
	0
};

void
dovmstat(interval, reps)
	u_int interval;
	int reps;
{
	int mib[2], size;
	time_t uptime, halfuptime;
	struct clockinfo ci;
	struct vmtotal total;
	struct vmmeter cnt, ocnt;

	knlist(nl);
	winlines = getwinsize();
	uptime = getuptime();
	halfuptime = uptime / 2;
	(void)signal(SIGCONT, needhdr);
	size = sizeof(ci);
	mib[0] = CTL_KERN;
	mib[1] = KERN_CLOCKRATE;
	if (sysctl(mib, 2, &ci, &size, NULL, 0) < 0)
		errexit("sysctl(KERN_CLOCKRATE): %s\n", strerror(errno));
	hz = ci.stathz ? ci.stathz : ci.hz;
	for (hdrcnt = 1;;) {
		if (--hdrcnt == 0)
			printhdr();
		kread(nl[X_CNT].n_value, &cnt, sizeof cnt, "cnt");
		getcputime();
		size = sizeof(total);
		mib[0] = CTL_VM;
		mib[1] = VM_METER;
		if (sysctl(mib, 2, &total, &size, NULL, 0) < 0)
			errexit("sysctl(VM_METER): %s\n", strerror(errno));
		(void)printf("%2d%2d%2d",
		    total.t_rq, total.t_dw + total.t_pw, total.t_sw);
#define pgtok(a) ((a) * cnt.v_page_size >> 10)
#define	rate(x)	(((x) + halfuptime) / uptime)	/* round */
		(void)printf("%6ld%6ld ",
		    pgtok(total.t_avm), pgtok(total.t_free));
		(void)printf("%4lu ", rate(cnt.v_faults - ocnt.v_faults));
		(void)printf("%3lu ",
		    rate(cnt.v_reactivated - ocnt.v_reactivated));
		(void)printf("%3lu ", rate(cnt.v_pageins - ocnt.v_pageins));
		(void)printf("%3lu %3lu ",
		    rate(cnt.v_pageouts - ocnt.v_pageouts), 0);
		(void)printf("%3lu ", rate(cnt.v_scan - ocnt.v_scan));
		dkstats();
		(void)printf("%4lu %4lu %3lu ",
		    rate(cnt.v_intr - ocnt.v_intr),
		    rate(cnt.v_syscall - ocnt.v_syscall),
		    rate(cnt.v_swtch - ocnt.v_swtch));
		cpustats();
		(void)printf("\n");
		(void)fflush(stdout);
		if (reps >= 0 && --reps <= 0)
			break;
		ocnt = cnt;
		uptime = interval;
		/*
		 * We round upward to avoid losing low-frequency events
		 * (i.e., >= 1 per interval but < 1 per second).
		 */
		halfuptime = (uptime + 1) / 2;
		(void)sleep(interval);
	}
}

int
getwinsize()
{
	struct winsize winsize;

	winsize.ws_row = 0;
	(void) ioctl(STDOUT_FILENO, TIOCGWINSZ, (char *)&winsize);
	return (winsize.ws_row > 0 ? winsize.ws_row : 20);
}

/*
 * Get cpu times for dkstats and cpustats().
 */
void
getcputime()
{
	register int state;
	long t, sum, cp_time[CPUSTATES];

	kread(nl[X_CPTIME].n_value, cp_time, sizeof cp_time, "cp_time");
	for (sum = 0, state = 0; state < CPUSTATES; ++state) {
		t = cp_time[state] - cputime.old[state];
		cputime.old[state] = cp_time[state];
		cputime.delta[state] = t;
		sum += t;
	}
	cputime.total = sum;
}

/*
 * Print disk statistics for dovmstat().
 */
void
dkstats()
{
	register struct dkinfo *dk;
	double etime;
#ifdef notyet
	long xfer;

	for (dk = dkinfo; dk != NULL; dk = dk->dk_next) {
		kread(addr + offsetof(struct dkdevice, dk_xfer),
		    &xfer, sizeof xfer, dk->dk_name);
		dk->dk_dxfer = xfer - dk->dk_oxfer;
		dk->dk_oxfer = xfer;
	}
#endif
	etime = (cputime.total ? (double)cputime.total : 1.0) / hz;
	for (dk = dkinfo; dk != NULL; dk = dk->dk_next)
		if (dk->dk_sel)
			(void)printf("%2.0f ", dk->dk_dxfer / etime);
}

/*
 * Print cpu statistics for dovmstat().
 */
void
cpustats()
{
	double pct;

	if (cputime.total)
		pct = 100.0 / cputime.total;
	else
		pct = 0.0;
	(void)printf("%2.0f %2.0f %2.0f",
	    (cputime.delta[CP_USER] + cputime.delta[CP_NICE]) * pct,
	    (cputime.delta[CP_SYS] + cputime.delta[CP_INTR]) * pct,
	    cputime.delta[CP_IDLE] * pct);
}

void
printhdr()
{
	register struct dkinfo *dk;

	(void)printf(" procs   memory     page%*s", 20, "");
	if (ndrives > 1)
		(void)printf("disks %*s  faults      cpu\n",
		   ndrives * 3 - 6, "");
	else
		(void)printf("%*s  faults      cpu\n", ndrives * 3, "");
	(void)printf(" r b w   avm   fre  flt  re  pi  po  fr  sr ");
	for (dk = dkinfo; dk != NULL; dk = dk->dk_next)
		if (dk->dk_sel)
			(void)printf("%s ", dk->dk_2c);
	(void)printf("  in   sy  cs us sy id\n");
	hdrcnt = winlines - 2;
}

/*
 * Force a header to be prepended to the next output.
 */
void
needhdr(sig)
	int sig;
{

	hdrcnt = 1;
}
