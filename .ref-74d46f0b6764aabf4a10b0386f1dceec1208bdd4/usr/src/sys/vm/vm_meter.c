/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_meter.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "kernel.h"
#include "machine/vmparam.h"
#include "vmmeter.h"

fixpt_t	averunnable[3];		/* load average, of runnable procs */

int	maxslp = MAXSLP;
int	saferss = SAFERSS;


vmmeter()
{
	register unsigned *cp, *rp, *sp;

	if (time.tv_sec % 5 == 0)
		vmtotal();
	if (proc[0].p_slptime > maxslp/2) {
		runout = 0;
		wakeup((caddr_t)&runout);
	}
}

vmtotal()
{
	register struct proc *p;
	int nrun = 0;

	total.t_vm = 0;
	total.t_avm = 0;
	total.t_rm = 0;
	total.t_arm = 0;
	total.t_rq = 0;
	total.t_dw = 0;
	total.t_pw = 0;
	total.t_sl = 0;
	total.t_sw = 0;
	for (p = allproc; p != NULL; p = p->p_nxt) {
		if (p->p_flag & SSYS)
			continue;
		if (p->p_stat) {
			switch (p->p_stat) {

			case SSLEEP:
				if (p->p_pri <= PZERO && p->p_slptime == 0)
					nrun++;
				/* fall through */
			case SSTOP:
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
				break;
			}
		}
	}
	loadav(averunnable, nrun);
}

/*
 * Constants for averages over 1, 5, and 15 minutes
 * when sampling at 5 second intervals.
 */
fixpt_t	cexp[3] = {
	0.9200444146293232 * FSCALE,	/* exp(-1/12) */
	0.9834714538216174 * FSCALE,	/* exp(-1/60) */
	0.9944598480048967 * FSCALE,	/* exp(-1/180) */
};

/*
 * Compute a tenex style load average of a quantity on
 * 1, 5 and 15 minute intervals.
 */
loadav(avg, n)
	register fixpt_t *avg;
	int n;
{
	register int i;

	for (i = 0; i < 3; i++)
		avg[i] = (cexp[i] * avg[i] + n * FSCALE * (FSCALE - cexp[i]))
		         >> FSHIFT;
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
	for (i = 0; i < 3; i++)
		avenrun[i] = (double) averunnable[i] / FSCALE;
#endif /* COMPAT_43 */
}
