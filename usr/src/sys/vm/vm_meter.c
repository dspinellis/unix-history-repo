/*	vm_meter.c	4.6	81/04/23	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/seg.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/vm.h"
#include "../h/cmap.h"

int	maxslp = MAXSLP;
int	saferss = SAFERSS;

/*
 * The following parameters control operation of the page replacement
 * algorithm.  They are initialized to 0, and then computed at boot time
 * based on the size of the system.  If they are patched non-zero in
 * a loaded vmunix they are left alone and may thus be changed per system
 * using adb on the loaded system.
 */
int	maxpgio = 0;
int	minfree = 0;
int	desfree = 0;
int	lotsfree = 0;
int	slowscan = 0;
int	fastscan = 0;
int	klin = KLIN;
int	klseql = KLSEQL;
int	kltxt = KLTXT;
int	klout = KLOUT;
int	multprog = -1;		/* so we don't count process 2 */

double	avenrun[3];		/* load average, of runnable procs */

/*
 * Setup the paging constants for the clock algorithm.
 * Called after the system is initialized and the amount of memory
 * and number of paging devices is known.
 */
setupclock()
{
	int nclust, nkb;

	/*
	 * Setup thresholds for paging:
	 *	lotsfree	is threshold where paging daemon turns on
	 *	desfree		is amount of memory desired free.  if less
	 *			than this for extended period, do swapping
	 *	minfree		is minimal amount of free memory which is
	 *			tolerable.
	 *
	 * Strategy of 4/22/81:
	 *	lotsfree is 1/4 of memory free.
	 *	desfree is 200k bytes, but at most 1/8 of memory
	 *	minfree is 32k bytes.
	 */
	if (lotsfree == 0)
		lotsfree = LOOPPAGES / 4;
	if (desfree == 0) {
		desfree = (200*1024) / NBPG;
		if (desfree > LOOPPAGES / 8)
			desfree = LOOPPAGES / 8;
	}
	if (minfree == 0)
		minfree = (32*1024) / NBPG;

	/*
	 * Maxpgio thresholds how much paging is acceptable.
	 * This figures that 2/3 busy on an arm is all that is
	 * tolerable for paging.  We assume one operation per disk rev.
	 */
	if (maxpgio == 0)
		maxpgio = (DISKRPM * 2) / 3;

	/*
	 * Clock to scan using max of 10% of processor time for sampling,
	 *     this estimated to allow maximum of 400 samples per second.
	 * Allow slighly higher angular velocity if 2 or more swap devices,
	 *     allow max of 600 samples per second (but only >= 2m)
	 * Basic scan time for ``fastscan'', the time for a clock rev
	 * with given memory and CLSIZE=2:
	 *	swap ilv	<=1m	2m	3m	4m	6m	8m
	 * 	one-way		4s	5s	7s	XXX	XXX	XXX
	 * 	two-way		4s	4s	5s	6s	10s	13s
	 * XXXs here are situations we should not be in.
	 */
	if (fastscan == 0) {
		nclust = LOOPPAGES / CLSIZE;
		nkb = (LOOPPAGES * NBPG) / 1024;
		if (nswdev == 1 && nkb >= 2*1024)
			printf("WARNING: should run interleaved swap with >= 2Mb\n");
		if (nswdev == 1 || nkb < 2*1024)
			fastscan = nclust / 400;
		else {
			maxpgio = (maxpgio * 3) / 2;
			fastscan = nclust / 600;
		}
	}
	if (fastscan < 4)
		fastscan = 4;
	if (fastscan > maxslp)
		fastscan = maxslp;

	/*
	 * Set slow scan time to 1/3 the fast scan time but at most
	 * maxslp (a macroscopic slow).
	 */
	if (slowscan == 0)
		slowscan = 3 * fastscan;
	if (slowscan > maxslp)
		slowscan = maxslp;
#ifdef defined(BERT) || defined(ERNIE)
	printf("slowscan %d, fastscan %d, maxpgio %d\n",
	    slowscan, fastscan, maxpgio);
	printf("lotsfree %d, desfree %d, minfree %d\n",
	    lotsfree, desfree, minfree);
#endif
}

/*
 * The main loop of the scheduling (swapping) process.
 *
 * The basic idea is:
 *	see if anyone wants to be swapped in;
 *	swap out processes until there is room;
 *	swap him in;
 *	repeat.
 * If the paging rate is too high, or the average free memory
 * is very low, then we do not consider swapping anyone in,
 * but rather look for someone to swap out.
 *
 * The runout flag is set whenever someone is swapped out.
 * Sched sleeps on it awaiting work.
 *
 * Sched sleeps on runin whenever it cannot find enough
 * core (by swapping out or otherwise) to fit the
 * selected swapped process.  It is awakened when the
 * core situation changes and in any case once per second.
 *
 * sched DOESN'T ACCOUNT FOR PAGE TABLE SIZE IN CALCULATIONS.
 */

#define	swappable(p) \
	(((p)->p_flag&(SSYS|SLOCK|SULOCK|SLOAD|SPAGE|SKEEP|SWEXIT|SPHYSIO))==SLOAD)

/* insure non-zero */
#define	nz(x)	(x != 0 ? x : 1)

#define	NBIG	4
#define	MAXNBIG	10
int	nbig = NBIG;

struct bigp {
	struct	proc *bp_proc;
	int	bp_pri;
	struct	bigp *bp_link;
} bigp[MAXNBIG], bplist;

sched()
{
	register struct proc *rp, *p, *inp;
	int outpri, inpri, rppri;
	int sleeper, desperate, deservin, needs, divisor;
	register struct bigp *bp, *nbp;
	int biggot, gives;

	/*
	 * Check if paging rate is too high, or average of
	 * free list very low and if so, adjust multiprogramming
	 * load by swapping someone out.
	 */
loop:
	wantin = 0;
	deservin = 0;
	sleeper = 0;
	p = 0;
	/*
	 * Conditions for hard outswap are:
	 *	if need kernel map (mix it up).
	 * or
	 *	1. if there are at least 2 runnable processes (on the average)
	 * and	2. the paging rate is excessive or memory is now VERY low.
	 * and	3. the short (5-second) and longer (30-second) average
	 *	   memory is less than desirable.
	 */
	if (kmapwnt || (avenrun[0] >= 2 && max(avefree, avefree30) < desfree &&
	    (rate.v_pgin + rate.v_pgout > maxpgio || avefree < minfree))) {
		desperate = 1;
		goto hardswap;
	}
	desperate = 0;
	/*
	 * Not desperate for core,
	 * look for someone who deserves to be brought in.
	 */
	outpri = -20000;
	for (rp = proc; rp < procNPROC; rp++) switch(rp->p_stat) {

	case SRUN:
		if ((rp->p_flag&SLOAD) == 0) {
			rppri = rp->p_time -
			    rp->p_swrss / nz((maxpgio/2) * (klin * CLSIZE)) +
			    rp->p_slptime - (rp->p_nice-NZERO)*8;
			if (rppri > outpri) {
				if (rp->p_poip)
					continue;
				if (rp->p_textp && rp->p_textp->x_poip)
					continue;
				p = rp;
				outpri = rppri;
			}
		}
		continue;

	case SSLEEP:
	case SSTOP:
		if ((freemem < desfree || rp->p_rssize == 0) &&
		    rp->p_slptime > maxslp &&
		    (!rp->p_textp || (rp->p_textp->x_flag&XLOCK)==0) &&
		    swappable(rp)) {
			/*
			 * Kick out deadwood.
			 */
			(void) spl6();
			rp->p_flag &= ~SLOAD;
			if (rp->p_stat == SRUN)
				remrq(rp);
			(void) spl0();
			(void) swapout(rp, rp->p_dsize, rp->p_ssize);
			goto loop;
		}
		continue;
	}

	/*
	 * No one wants in, so nothing to do.
	 */
	if (outpri == -20000) {
		(void) spl6();
		if (wantin) {
			wantin = 0;
			sleep((caddr_t)&lbolt, PSWP);
		} else {
			runout++;
			sleep((caddr_t)&runout, PSWP);
		}
		(void) spl0();
		goto loop;
	}
	/*
	 * Decide how deserving this guy is.  If he is deserving
	 * we will be willing to work harder to bring him in.
	 * Needs is an estimate of how much core he will need.
	 * If he has been out for a while, then we will
	 * bring him in with 1/2 the core he will need, otherwise
	 * we are conservative.
	 */
	deservin = 0;
	divisor = 1;
	if (outpri > maxslp/2) {
		deservin = 1;
		divisor = 2;
	}
	needs = p->p_swrss;
	if (p->p_textp && p->p_textp->x_ccount == 0)
		needs += p->p_textp->x_swrss;
	needs = imin(needs, lotsfree);
	if (freemem - deficit > needs / divisor) {
		deficit += needs;
		if (swapin(p))
			goto loop;
		deficit -= imin(needs, deficit);
	}

hardswap:
	/*
	 * Need resources (kernel map or memory), swap someone out.
	 * Select the nbig largest jobs, then the oldest of these
	 * is ``most likely to get booted.''
	 */
	inp = p;
	sleeper = 0;
	if (nbig > MAXNBIG)
		nbig = MAXNBIG;
	if (nbig < 1)
		nbig = 1;
	biggot = 0;
	bplist.bp_link = 0;
	for (rp = proc; rp < procNPROC; rp++) {
		if (!swappable(rp))
			continue;
		if (rp->p_stat==SZOMB)
			continue;
		if (rp == inp)
			continue;
		if (rp->p_textp && rp->p_textp->x_flag&XLOCK)
			continue;
		if (rp->p_slptime > maxslp &&
		    (rp->p_stat==SSLEEP&&rp->p_pri>PZERO||rp->p_stat==SSTOP)) {
			if (sleeper < rp->p_slptime) {
				p = rp;
				sleeper = rp->p_slptime;
			}
		} else if (!sleeper && (rp->p_stat==SRUN||rp->p_stat==SSLEEP)) {
			rppri = rp->p_rssize;
			if (rp->p_textp)
				rppri += rp->p_textp->x_rssize/rp->p_textp->x_ccount;
			if (biggot < nbig)
				nbp = &bigp[biggot++];
			else {
				nbp = bplist.bp_link;
				if (nbp->bp_pri > rppri)
					continue;
				bplist.bp_link = nbp->bp_link;
			}
			for (bp = &bplist; bp->bp_link; bp = bp->bp_link)
				if (rppri < bp->bp_link->bp_pri)
					break;
			nbp->bp_link = bp->bp_link;
			bp->bp_link = nbp;
			nbp->bp_pri = rppri;
			nbp->bp_proc = rp;
		}
	}
	if (!sleeper) {
		p = NULL;
		inpri = -1000;
		for (bp = bplist.bp_link; bp; bp = bp->bp_link) {
			rp = bp->bp_proc;
			rppri = rp->p_time+rp->p_nice-NZERO;
			if (rppri >= inpri) {
				p = rp;
				inpri = rppri;
			}
		}
	}
	/*
	 * If we found a long-time sleeper, or we are desperate and
	 * found anyone to swap out, or if someone deserves to come
	 * in and we didn't find a sleeper, but found someone who
	 * has been in core for a reasonable length of time, then
	 * we kick the poor luser out.
	 */
	if (sleeper || desperate && p || deservin && inpri > maxslp) {
		(void) spl6();
		p->p_flag &= ~SLOAD;
		if (p->p_stat == SRUN)
			remrq(p);
		(void) spl0();
		if (desperate) {
			/*
			 * Want to give this space to the rest of
			 * the processes in core so give them a chance
			 * by increasing the deficit.
			 */
			gives = p->p_rssize;
			if (p->p_textp)
				gives += p->p_textp->x_rssize / p->p_textp->x_ccount;
			gives = min(gives, lotsfree);
			deficit += gives;
		} else
			gives = 0;	/* someone else taketh away */
		if (swapout(p, p->p_dsize, p->p_ssize) == 0)
			deficit -= imin(gives, deficit);
		goto loop;
	}
	/*
	 * Want to swap someone in, but can't
	 * so wait on runin.
	 */
	(void) spl6();
	runin++;
	sleep((caddr_t)&runin, PSWP);
	(void) spl0();
	goto loop;
}

vmmeter()
{
	register unsigned *cp, *rp, *sp;

	deficit -= imin(deficit,
	    imax(deficit / 10, ((klin * CLSIZE) / 2) * maxpgio / 2));
	ave(avefree, freemem, 5);
	ave(avefree30, freemem, 30);
	/* v_pgin is maintained by clock.c */
	cp = &cnt.v_first; rp = &rate.v_first; sp = &sum.v_first;
	while (cp <= &cnt.v_last) {
		ave(*rp, *cp, 5);
		*sp += *cp;
		*cp = 0;
		rp++, cp++, sp++;
	}
	if (time % 5 == 0) {
		vmtotal();
		rate.v_swpin = cnt.v_swpin;
		sum.v_swpin += cnt.v_swpin;
		cnt.v_swpin = 0;
		rate.v_swpout = cnt.v_swpout;
		sum.v_swpout += cnt.v_swpout;
		cnt.v_swpout = 0;
	}
	if (avefree < minfree && runout || proc[0].p_slptime > maxslp/2) {
		runout = 0;
		runin = 0;
		wakeup((caddr_t)&runin);
		wakeup((caddr_t)&runout);
	}
}

vmpago()
{
	register int vavail;
	register int scanrate;

	/*
	 * Compute new rate for clock; if
	 * nonzero, restart clock.
	 * Rate ranges linearly from one rev per
	 * slowscan seconds when there is lotsfree memory
	 * available to one rev per fastscan seconds when
	 * there is no memory available.
	 */
	nscan = desscan = 0;
	vavail = freemem - deficit;
	if (vavail < 0)
		vavail = 0;
	if (freemem >= lotsfree)
		return;
	scanrate = (slowscan * vavail + fastscan * (lotsfree - vavail)) / nz(lotsfree);
	desscan = (LOOPPAGES / CLSIZE) / nz(scanrate);
	/*
	 * DIVIDE BY 4 TO ACCOUNT FOR RUNNING 4* A SECOND (see clock.c)
	 */
	desscan /= 4;
	wakeup((caddr_t)&proc[2]);
}

vmtotal()
{
	register struct proc *p;
	register struct text *xp;
	int nrun = 0;

	total.t_vmtxt = 0;
	total.t_avmtxt = 0;
	total.t_rmtxt = 0;
	total.t_armtxt = 0;
	for (xp = text; xp < textNTEXT; xp++)
		if (xp->x_iptr) {
			total.t_vmtxt += xp->x_size;
			total.t_rmtxt += xp->x_rssize;
			for (p = xp->x_caddr; p; p = p->p_xlink)
			switch (p->p_stat) {

			case SSTOP:
			case SSLEEP:
				if (p->p_slptime >= maxslp)
					continue;
				/* fall into... */

			case SRUN:
			case SIDL:
				total.t_avmtxt += xp->x_size;
				total.t_armtxt += xp->x_rssize;
				goto next;
			}
next:
			;
		}
	total.t_vm = 0;
	total.t_avm = 0;
	total.t_rm = 0;
	total.t_arm = 0;
	total.t_rq = 0;
	total.t_dw = 0;
	total.t_pw = 0;
	total.t_sl = 0;
	total.t_sw = 0;
	for (p = proc; p < procNPROC; p++) {
		if (p->p_flag & SSYS)
			continue;
		if (p->p_stat) {
			total.t_vm += p->p_dsize + p->p_ssize;
			total.t_rm += p->p_rssize;
			switch (p->p_stat) {

			case SSLEEP:
			case SSTOP:
				if (p->p_pri <= PZERO)
					nrun++;
				if (p->p_flag & SPAGE)
					total.t_pw++;
				else if (p->p_flag & SLOAD) {
					if (p->p_pri <= PZERO)
						total.t_dw++;
					else if (p->p_slptime < maxslp)
						total.t_sl++;
				} else if (p->p_slptime < maxslp)
					total.t_sw++;
				if (p->p_slptime < maxslp)
					goto active;
				break;

			case SRUN:
			case SIDL:
				nrun++;
				if (p->p_flag & SLOAD)
					total.t_rq++;
				else
					total.t_sw++;
active:
				total.t_avm += p->p_dsize + p->p_ssize;
				total.t_arm += p->p_rssize;
				break;
			}
		}
	}
	total.t_vm += total.t_vmtxt;
	total.t_avm += total.t_avmtxt;
	total.t_rm += total.t_rmtxt;
	total.t_arm += total.t_armtxt;
	total.t_free = avefree;
	loadav(avenrun, nrun);
}

/*
 * Constants for averages over 1, 5, and 15 minutes
 * when sampling at 5 second intervals.
 */
double	cexp[3] = {
	0.9200444146293232,	/* exp(-1/12) */
	0.9834714538216174,	/* exp(-1/60) */
	0.9944598480048967,	/* exp(-1/180) */
};

/*
 * Compute a tenex style load average of a quantity on
 * 1, 5 and 15 minute intervals.
 */
loadav(avg, n)
	register double *avg;
	int n;
{
	register int i;

	for (i = 0; i < 3; i++)
		avg[i] = cexp[i] * avg[i] + n * (1.0 - cexp[i]);
}
