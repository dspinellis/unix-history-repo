/*	@(#)addmush.c	4.3	(Melbourne)	82/02/27	*/

/*
 * add mush data
 */

#include <sys/types.h>
#include <sys/quota.h>
#include <sys/mush.h>
#include <mushmuck.h>

#define	Max(a)	if (from->a == 0 || to->a && from->a > to->a) to->a = from->a
#define	Min(a)	if (from->a < to->a) to->a = from->a
#define	Sum(a)	if (from->a && to->a) to->a += from->a; else to->a = 0

addmush(to, from)
register struct mushmuck *to, *from;
{
	register shares = to->mm_qu.qu_shares;

	shares += from->mm_qu.qu_shares;
	shares *= 2;
	shares /= 3;
	if (shares < to->mm_qu.qu_shares)
		shares = to->mm_qu.qu_shares;
	if (shares < from->mm_qu.qu_shares)
		shares = from->mm_qu.qu_shares;
	to->mm_qu.qu_shares = shares;

	to->mm_qu.qu_class |= from->mm_qu.qu_class;
	Max(mm_qu.qu_plim);

	if (to->mm_dinc && !from->mm_dinc &&
	    !to->mm_winc && from->mm_winc) {
		if (to->mm_dinc * 5 < from->mm_winc) {
			to->mm_winc = from->mm_winc + to->mm_dinc * 5;
			to->mm_wmax = from->mm_wmax + to->mm_dmax * 5;
			to->mm_dinc = 0;
			to->mm_dmax = 5;
		} else {
			to->mm_dinc += from->mm_winc / 5;
			to->mm_dmax += from->mm_wmax / 5;
		}
	} else if (!to->mm_dinc && from->mm_dinc &&
	   to->mm_winc && !from->mm_winc) {
		if (to->mm_winc < from->mm_dinc * 5) {
			to->mm_dinc = from->mm_dinc + to->mm_winc / 5;
			to->mm_dmax = from->mm_dmax + to->mm_wmax / 5;
			to->mm_winc = 0;
			to->mm_wmax = 0;
		} else {
			to->mm_winc += from->mm_dinc * 5;
			to->mm_wmax += from->mm_dmax * 5;
		}
	} else {
		Sum(mm_dinc);
		Sum(mm_winc);
		Sum(mm_dmax);
		Sum(mm_wmax);
	}

	Max(mm_smax);
	Min(mm_lgap);

	to->mm_qu.qu_syflags = 
	   (to->mm_qu.qu_syflags|from->mm_qu.qu_syflags) & (QF_MODTTY|QF_FASTTY)
	    | (to->mm_qu.qu_syflags&from->mm_qu.qu_syflags);
}
