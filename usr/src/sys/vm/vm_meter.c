/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_meter.c	7.12 (Berkeley) %G%
 */

#include "param.h"
#include "proc.h"
#include "systm.h"
#include "kernel.h"
#include "vm.h"

fixpt_t	averunnable[3];		/* load average, of runnable procs */

int	maxslp = MAXSLP;
int	saferss = SAFERSS;


vmmeter()
{
	register unsigned *cp, *rp, *sp;

	if (time.tv_sec % 5 == 0)
		loadav(averunnable);
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
loadav(avg)
	register fixpt_t *avg;
{
	register int i, nrun;
	register struct proc *p;

	for (nrun = 0, p = allproc; p != NULL; p = p->p_nxt) {
		switch (p->p_stat) {
		case SSLEEP:
			if (p->p_pri > PZERO || p->p_slptime != 0)
				continue;
			/* fall through */
		case SRUN:
		case SIDL:
			nrun++;
		}
	}
	for (i = 0; i < 3; i++)
		avg[i] = (cexp[i] * avg[i] + nrun * FSCALE * (FSCALE - cexp[i]))
		         >> FSHIFT;
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
	for (i = 0; i < 3; i++)
		avenrun[i] = (double) averunnable[i] / FSCALE;
#endif /* COMPAT_43 */
}

/*
 * Calculate and return vmtotals structure.
 */
kinfo_meter(op, where, acopysize, arg, aneeded)
	int op;
	caddr_t where;
	int *acopysize, arg, *aneeded;
{
	struct vmtotal vmtotals;
	int error;

	*aneeded = sizeof(struct vmtotal);
	if (where == NULL)
		return (0);
	if (*acopysize < sizeof(struct vmtotal))
		return (EINVAL);
	vmtotal(&vmtotals);
	if (error = copyout((caddr_t)&vmtotals, where, sizeof(struct vmtotal)))
		return (error);
	*acopysize = sizeof(struct vmtotal);
	return (0);
}

/*
 * Calculate the current state of the system.
 * Done on demand from getkerninfo().
 */
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
	simple_unlock(&vm_object_list_lock);
	simple_lock(&vm_object_list_lock);
	object = (vm_object_t) queue_first(&vm_object_list);
	while (!queue_end(&vm_object_list, (queue_entry_t) object)) {
		object->flags &= ~OBJ_ACTIVE;
		object = (vm_object_t) queue_next(&object->object_list);
	}
	simple_unlock(&vm_object_list_lock);
	/*
	 * Calculate process statistics.
	 */
	for (p = allproc; p != NULL; p = p->p_nxt) {
		if (p->p_flag & SSYS)
			continue;
		switch (p->p_stat) {
		case 0:
			continue;

		case SSLEEP:
		case SSTOP:
			if (p->p_flag & SLOAD) {
				if (p->p_pri <= PZERO)
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
			if (p->p_flag & SLOAD)
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
			if (entry->is_a_map || entry->is_sub_map)
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
	object = (vm_object_t) queue_first(&vm_object_list);
	while (!queue_end(&vm_object_list, (queue_entry_t) object)) {
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
		object = (vm_object_t) queue_next(&object->object_list);
	}
	totalp->t_free = cnt.v_free_count;
}
