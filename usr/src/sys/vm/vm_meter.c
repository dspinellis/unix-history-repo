/*
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_meter.c	8.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <vm/vm.h>
#include <sys/sysctl.h>

struct	loadavg averunnable;		/* load average, of runnable procs */

int	maxslp = MAXSLP;
int	saferss = SAFERSS;

void
vmmeter()
{
	register unsigned *cp, *rp, *sp;

	if (time.tv_sec % 5 == 0)
		loadav(&averunnable);
	if (proc0.p_slptime > maxslp/2)
		wakeup((caddr_t)&proc0);
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
void
loadav(avg)
	register struct loadavg *avg;
{
	register int i, nrun;
	register struct proc *p;

	for (nrun = 0, p = (struct proc *)allproc; p != NULL; p = p->p_next) {
		switch (p->p_stat) {
		case SSLEEP:
			if (p->p_priority > PZERO || p->p_slptime != 0)
				continue;
			/* fall through */
		case SRUN:
		case SIDL:
			nrun++;
		}
	}
	for (i = 0; i < 3; i++)
		avg->ldavg[i] = (cexp[i] * avg->ldavg[i] +
			nrun * FSCALE * (FSCALE - cexp[i])) >> FSHIFT;
}

/*
 * Attributes associated with virtual memory.
 */
vm_sysctl(name, namelen, oldp, oldlenp, newp, newlen, p)
	int *name;
	u_int namelen;
	void *oldp;
	size_t *oldlenp;
	void *newp;
	size_t newlen;
	struct proc *p;
{
	struct vmtotal vmtotals;

	/* all sysctl names at this level are terminal */
	if (namelen != 1)
		return (ENOTDIR);		/* overloaded */

	switch (name[0]) {
	case VM_LOADAVG:
		averunnable.fscale = FSCALE;
		return (sysctl_rdstruct(oldp, oldlenp, newp, &averunnable,
		    sizeof(averunnable)));
	case VM_METER:
		vmtotal(&vmtotals);
		return (sysctl_rdstruct(oldp, oldlenp, newp, &vmtotals,
		    sizeof(vmtotals)));
	default:
		return (EOPNOTSUPP);
	}
	/* NOTREACHED */
}

/*
 * Calculate the current state of the system.
 * Done on demand from getkerninfo().
 */
void
vmtotal(totalp)
	register struct vmtotal *totalp;
{
	register struct proc *p;
	register vm_map_entry_t	entry;
	register vm_object_t object;
	register vm_map_t map;
	int paging;

	bzero(totalp, sizeof *totalp);
	/*
	 * Mark all objects as inactive.
	 */
	simple_lock(&vm_object_list_lock);
	for (object = vm_object_list.tqh_first;
	     object != NULL;
	     object = object->object_list.tqe_next)
		object->flags &= ~OBJ_ACTIVE;
	simple_unlock(&vm_object_list_lock);
	/*
	 * Calculate process statistics.
	 */
	for (p = (struct proc *)allproc; p != NULL; p = p->p_next) {
		if (p->p_flag & P_SYSTEM)
			continue;
		switch (p->p_stat) {
		case 0:
			continue;

		case SSLEEP:
		case SSTOP:
			if (p->p_flag & P_INMEM) {
				if (p->p_priority <= PZERO)
					totalp->t_dw++;
				else if (p->p_slptime < maxslp)
					totalp->t_sl++;
			} else if (p->p_slptime < maxslp)
				totalp->t_sw++;
			if (p->p_slptime >= maxslp)
				continue;
			break;

		case SRUN:
		case SIDL:
			if (p->p_flag & P_INMEM)
				totalp->t_rq++;
			else
				totalp->t_sw++;
			if (p->p_stat == SIDL)
				continue;
			break;
		}
		/*
		 * Note active objects.
		 */
		paging = 0;
		for (map = &p->p_vmspace->vm_map, entry = map->header.next;
		     entry != &map->header; entry = entry->next) {
			if (entry->is_a_map || entry->is_sub_map ||
			    entry->object.vm_object == NULL)
				continue;
			entry->object.vm_object->flags |= OBJ_ACTIVE;
			paging |= entry->object.vm_object->paging_in_progress;
		}
		if (paging)
			totalp->t_pw++;
	}
	/*
	 * Calculate object memory usage statistics.
	 */
	simple_lock(&vm_object_list_lock);
	for (object = vm_object_list.tqh_first;
	     object != NULL;
	     object = object->object_list.tqe_next) {
		totalp->t_vm += num_pages(object->size);
		totalp->t_rm += object->resident_page_count;
		if (object->flags & OBJ_ACTIVE) {
			totalp->t_avm += num_pages(object->size);
			totalp->t_arm += object->resident_page_count;
		}
		if (object->ref_count > 1) {
			/* shared object */
			totalp->t_vmshr += num_pages(object->size);
			totalp->t_rmshr += object->resident_page_count;
			if (object->flags & OBJ_ACTIVE) {
				totalp->t_avmshr += num_pages(object->size);
				totalp->t_armshr += object->resident_page_count;
			}
		}
	}
	totalp->t_free = cnt.v_free_count;
}
