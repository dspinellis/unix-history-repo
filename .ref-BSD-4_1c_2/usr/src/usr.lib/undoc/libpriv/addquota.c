/*	@(#)addquota.c	4.5	(Melbourne)	82/02/16	*/

/*
 * add lpdquota data
 */

#include <sys/types.h>
#include <sys/quota.h>

/* the "-1" in the next line is cause quotas are vals you have to be < than */
#define	Sum(a)	if (from->a && to->a) to->a += from->a - 1; else to->a = 0

addquota(to, from)
register struct dqblk *to, *from;
{
	register d1, d2;

	d1 = to->dqb_ilim - to->dqb_iq;
	d2 = from->dqb_ilim - from->dqb_iq;
	if (d1 < 0)
		d1 = 0;
	if (d2 < 0)
		d2 = 0;

	Sum(dqb_iq);

	if (to->dqb_iq && to->dqb_ilim && from->dqb_ilim)
		to->dqb_ilim = to->dqb_iq + (d1 > d2 ? d1 : d2);
	else
		to->dqb_ilim = 0;

	d1 = to->dqb_blim - to->dqb_quot;
	d2 = from->dqb_blim - from->dqb_quot;
	if (d1 < 0)
		d1 = 0;
	if (d2 < 0)
		d2 = 0;

	Sum(dqb_quot);

	if (to->dqb_quot && to->dqb_blim && from->dqb_blim)
		to->dqb_blim = to->dqb_quot + (d1 > d2 ? d1 : d2);
	else
		to->dqb_blim = 0;
}
