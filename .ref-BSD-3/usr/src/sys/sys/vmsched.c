/*	vmsched.c	2.2	2/10/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/seg.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/vm.h"
#include "../h/cmap.h"

int	maxpgio = MAXPGIO;
int	maxslp = MAXSLP;
int	minfree = MINFREE;
int	desfree = DESFREE;
/* In main.c since LOTSFREE is variable */
/* int	lotsfree = LOTSFREE; */
int	saferss = SAFERSS;
int	slowscan = SLOWSCAN;
int	fastscan = FASTSCAN;
int	multprog = -1;		/* so we don't count process 2 */

double	avenrun[3];		/* load average, of runnable procs */

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
 */

#define	swappable(p) \
	(((p)->p_flag&(SSYS|SLOCK|SULOCK|SLOAD|SPAGE|SKEEP|SWEXIT))==SLOAD)

/* insure non-zero */
#define	nz(x)	(x != 0 ? x : 1)

sched()
{
	register struct proc *rp, *p, *inp;
	register int outpri, inpri, rppri;
	int smax;

	/*
	 * Check if paging rate is too high, or average of
	 * free list very low and if so, adjust multiprogramming
	 * load by swapping someone out.
	 *
	 * Avoid glitches: don't swap out only process to do this,
	 * and don't swap based on paging rate if there is a reasonable
	 * amount of free memory.
	 */
loop:
	VOID spl6();
	if (kmapwnt || (multprog > 1 && avefree < desfree &&
	    (rate.v_pgin + rate.v_pgout > maxpgio || avefree < minfree))) {
		outpri = 10000;
		p = 0;
	} else {
		/*
		 * Number of pages available and paging rate seem
		 * reasonable, consider increasing multiprogramming
		 * by swapping in process which has been out longest.
		 * If you went out with a lot of pages, then you are
		 * lower priority to come in... but are not brought in
		 * until there is a reasonable fraction of the memory
		 * you are expected to need available.  The system will
		 * also protect memory for you to some extent in this
		 * case by computing the expected ``deficit'' (pages
		 * ``owed'' to you) and not giving them away via further
		 * swapins of process which want many pages.
		 */
		outpri = -20000;
		for (rp = &proc[0]; rp < &proc[NPROC]; rp++) {
			rppri = rp->p_time - (rp->p_nice-NZERO)*8;
			if (rp->p_time < MAXSLP)
				rppri -= rp->p_swrss / nz(maxpgio / 2);	
			if (rp->p_stat==SRUN && (rp->p_flag&SLOAD)==0 &&
			    rp->p_poip==0 &&
			    (rp->p_textp==0||rp->p_textp->x_poip==0) &&
			    rppri > outpri) {
				p = rp;
				outpri = rppri;
			} else if ((rp->p_stat==SSLEEP||rp->p_stat==SSTOP) &&
			    (freemem < desfree || rp->p_rssize == 0) &&
			    rp->p_slptime > maxslp &&
			    (!rp->p_textp || (rp->p_textp->x_flag&XLOCK)==0) &&
			    swappable(rp)) {
				/*
				 * We found a process which has been blocked
				 * in core for a long time, and memory is
				 * not as free as we would prefer.
				 * Swap it out to free its u. and page table
				 * pages, then start over.  We do this here
				 * because we want to get rid of this guy
				 * even if noone wants to come in.
				 */
				rp->p_flag &= ~SLOAD;
				VOID swapout(rp, rp->p_dsize, rp->p_ssize);
				goto loop;
			}
		}
		/*
		 * If there is no one there, wait.
		 */
		if (outpri == -20000) {
			runout++;
			sleep((caddr_t)&runout, PSWP);
			goto loop;
		}
		VOID spl0();

		/*
		 * If there are resources (kernel map, memory), swap p in.
		 * If the process was swapped out while it still had pages,
		 * don't bring it back unless there is a reasonable amount
		 * of memory for it to work with.
		 */
		if (freemem > imin(deficit, lotsfree) + imin(p->p_swrss / 2, 2 * maxpgio) ||
		    p->p_swrss < 2 * maxpgio && freemem > desfree) {
			if (swapin(p))
				goto loop;
		}
	}

	/*
	 * Need resources (kernel map or memory), swap someone out.
	 * Select the person who has been sleeping longest
	 * at bad priority; if none, select the oldest.
	 */
	VOID spl6();
	inp = p;
	p = NULL;
	smax = -1;
	inpri = -1;
	for (rp = &proc[0]; rp < &proc[NPROC]; rp++) {
		if (rp->p_stat==SZOMB)
			continue;
		if (rp == inp)
			continue;
		if (!swappable(rp))
			continue;
		if (rp->p_textp && rp->p_textp->x_flag&XLOCK)
			continue;
		if ((rp->p_stat==SSLEEP&&rp->p_pri>=PZERO || rp->p_stat==SSTOP)
		    && rp->p_slptime > maxslp) {
			if (smax < rp->p_slptime) {
				p = rp;
				smax = rp->p_slptime;
			}
		} else if (smax<0 && (rp->p_stat==SRUN||rp->p_stat==SSLEEP)) {
			rppri = rp->p_time+rp->p_nice-NZERO;
			if (rp->p_time < maxslp)
				rppri -= imin(rp->p_swrss / nz(maxpgio), maxslp / 2);
			if (rppri > inpri) {
				p = rp;
				inpri = rppri;
			}
		}
	}
	/*
	 * Swap found user out if sleeping at bad pri for maxslp seconds,
	 * or if he has spent at least 5 seconds in core and
	 * the swapped-out process has spent at least 5 seconds out.
	 * Otherwise wait a bit and try again.
	 * (Note these are not really ``times'' but priorities.
	 */
	if (smax>=0 || (outpri>=5 && inpri>=5)) {
		p->p_flag &= ~SLOAD;
		VOID swapout(p, p->p_dsize, p->p_ssize);
		goto loop;
	}
	VOID spl6();
	runin++;
	sleep((caddr_t)&runin, PSWP);
	goto loop;
}

#define	vave(field, time) \
	ave(rate.field, cnt.field, time); sum.field += cnt.field; cnt.field = 0

vmmeter()
{
	register int scanrate;
	register int vavail;

	deficit -= imin(deficit, imax(deficit / 10, maxpgio / 2));
	ave(avefree, freemem, 5);
	/* v_pgin is maintained by clock.c */
	vave(v_pgout, 5);
	vave(v_intrans, 5);
	vave(v_pgrec, 5);
	vave(v_exfod, 5);
	vave(v_zfod, 5);
	vave(v_vrfod, 5);
	vave(v_nexfod, 5);
	vave(v_nzfod, 5);
	vave(v_nvrfod, 5);
	vave(v_pgfrec, 5);
	vave(v_faults, 5);
	vave(v_scan, 5);
	vave(v_rev, 5);
	vave(v_dfree, 5);
	vave(v_swtch, 5);
	if (time % 5 == 0)
		vmtotal();
	if (time % 10 == 0) {
		vave(v_swpin, 2);
		vave(v_swpout, 2);
	}
	if (avefree < minfree && runout || proc[0].p_slptime > 5) {
		runout = 0;
		runin = 0;
		wakeup((caddr_t)&runin);
		wakeup((caddr_t)&runout);
	}

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
	if (freemem >= lotsfree)
		return;
	scanrate = (slowscan * vavail + fastscan * (lotsfree - vavail)) / nz(lotsfree);
	desscan = LOOPSIZ / nz(scanrate);
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
	for (xp = &text[0]; xp < &text[NTEXT]; xp++)
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
	for (p = &proc[0]; p < &proc[NPROC]; p++) {
		if (p->p_flag & SSYS)
			continue;
		if (p->p_stat) {
			total.t_vm += p->p_dsize + p->p_ssize;
			total.t_rm += p->p_rssize;
			switch (p->p_stat) {

			case SSLEEP:
			case SSTOP:
				if (p->p_pri < PZERO)
					nrun++;
				if (p->p_flag & SPAGE)
					total.t_pw++;
				else if (p->p_flag & SLOAD) {
					if (p->p_pri < PZERO)
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
