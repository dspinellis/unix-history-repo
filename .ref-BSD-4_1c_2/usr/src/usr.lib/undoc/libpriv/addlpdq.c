/*	@(#)addlpdq.c	4.4	(Melbourne)	82/02/27	*/

/*
 * add lpdquota data
 */

#include <sys/types.h>
#include <lpdquota.h>

#define	Max(a)	if (from->a == 0 || to->a && from->a > to->a) to->a = from->a
#define	Min(a)	if (from->a < to->a) to->a = from->a
#define	Sum(a)	if (from->a && to->a) to->a += from->a; else to->a = 0

addlpdq(to, from)
register struct lpquota *to, *from;
{
	if (from->lpq_limit)
		return;
	if (to->lpq_limit) {
		*to = *from;
		return;
	}
	to->lpq_prclass |= from->lpq_prclass;

	if (to->lpq_daily && !from->lpq_daily &&
	   !to->lpq_allow && from->lpq_allow) {
		if (to->lpq_daily * 5 < from->lpq_allow) {
			to->lpq_allow = from->lpq_allow + to->lpq_daily*5;
			to->lpq_wmax = from->lpq_wmax + to->lpq_dmax*5;
			to->lpq_daily = 0;
			to->lpq_dmax = 0;
		} else {
			to->lpq_daily += from->lpq_allow / 5;
			to->lpq_dmax += from->lpq_wmax / 5;
		}
	} else if (from->lpq_daily && !to->lpq_daily &&
	   !from->lpq_allow && to->lpq_allow) {
		if (from->lpq_daily * 5 > to->lpq_allow) {
			to->lpq_daily = from->lpq_daily + to->lpq_allow/5;
			to->lpq_dmax = from->lpq_dmax + to->lpq_wmax/5;
			to->lpq_allow = 0;
			to->lpq_wmax = 0;
		} else {
			to->lpq_allow += from->lpq_daily * 5;
			to->lpq_wmax += from->lpq_dmax * 5;
		}
	} else {
		Sum(lpq_daily);
		Sum(lpq_dmax);
		Sum(lpq_allow);
		Sum(lpq_wmax);
	}
	Sum(lpq_limit);
}
