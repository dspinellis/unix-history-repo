/*	quota_share.c	Basser 2.1 Melb 4.2	82/11/13	*/

/*
 *	Share scheduling
 */

#if	MUSH
#include "../h/param.h"
#include "../h/mount.h"
#include "../h/quota.h"
#include "../h/share.h"


#ifdef notdef
/*
 * This code should be called from the clock, but in rewriting
 * that stuff, it was dropped out, so we ``notdef'' it for now.
 */
share()
{
	register struct quota *	q;
	register struct quota *	qq;
	register struct qclass* cl;
	register long		totshare;
	register long		totrate;
	register float		totusage;
	register float		avrate;
	register long		c;
	register int		maxn;
	register int		minn;
	register		n;
	static	 float		Flt_0 = 0.0;

	if ( shareflags & NOSHARE )
		return;

	n = 0;
	totshare = 0;
	totrate = 0;
	totusage = Flt_0;
	Totusage = Flt_0;
	Totcost = 0;
	qq = quotaNQUOTA;

	for (q = quota; q < qq; q++)
		if (q->q_cnt) {
			c = q->q_cost;
			if (c <= 3)
				c = 4;	/* no divides by zero! */
			q->q_cost = 0;
			q->q_usage *= Usagek;
			q->q_usage += c;
			if (q->q_usage < MINUSAGE)
				q->q_usage = MINUSAGE;
			q->q_rate = ((q->q_rate + c) >> 2) + (q->q_rate >> 1);
			Totcost += c;
			if (q->q_shares) {
				totshare += q->q_shares;
				totrate += q->q_rate;
				totusage += q->q_usage;
				Totusage += q->q_shares / q->q_usage;
				n++;
			}
		}

	if (n == 0)
		return;

	Totusers = n;
	totusage = totshare / totusage;
	totusage *= USAGEFACTOR;
	avrate = (float)totshare / (float)totrate;
	Avrate = avrate;
	avrate *= RATEFACTOR;

	if (shareflags & NONICE)
		return;

	for (q = &quota[1]; q < qq; q++)
		if (q->q_cnt)
			if (q->q_shares == 0)
				q->q_nice = BLOODYNICE;
			else {
				maxn = maxnice;
				minn = 0;
				c = 0;
				for (cl=curclass; cl < &curclass[NCLASS]; cl++)
					if (cl->class & q->q_class) {
						c += cl->cost;
						maxn = cl->maxn;
						minn = cl->minn;
					} else
						break;
				q->q_cost = c;
				n = (q->q_usage*totusage + q->q_rate*avrate)
						/ q->q_shares;
				if (n > maxn)
					n = maxn;
				if (n < minn)
					n = minn;
				q->q_nice = n;
			}
}
#endif

evalshare()
{
	register struct quota *	q;
	register struct quota *	qq;
	register long		totshare;
	register long		totrate;
	register		n;
	static	 float		Flt_0 = 0.0;

	if ( shareflags & NOSHARE )
		return;

	n = 0;
	totshare = 0;
	totrate = 0;
	Totusage = Flt_0;
	Totcost = 0;
	qq = quotaNQUOTA;

	for (q = quota; q < qq; q++)
		if (q->q_cnt) {
			Totcost += q->q_cost;
			if (q->q_usage < MINUSAGE)
				q->q_usage = MINUSAGE;
			if (q->q_shares) {
				totshare += q->q_shares;
				totrate += q->q_rate;
				Totusage += q->q_shares / q->q_usage;
				n++;
			}
		}

	if (n == 0)
		return;

	Totusers = n;
	if (totrate)
		Avrate = (float)totshare / (float)totrate;
}
#endif
