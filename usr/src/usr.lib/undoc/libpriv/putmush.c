/*	@(#)putmush.c	4.2	(Melbourne)	82/02/21	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/quota.h>
#include <mushmuck.h>

#define	Mushlset(req, var)	if (!omm || omm->var != mm->var) \
					mushlset(uid, req, mm->var)
#define	Mushset(req, var)	if (!omm || omm->var != mm->var) \
					mushset(uid, req, mm->var)

putmush(uid, mm, omm)
register uid;
register struct mushmuck *mm;
register struct mushmuck *omm;
{
	if (nargs() < 3)
		omm = (struct mushmuck *)0;

	Mushlset(MM_A_FLAGS, mm_qu.qu_syflags);
	Mushlset(MM_A_CLASS, mm_qu.qu_class);
	Mushset(MM_A_SHARE, mm_qu.qu_shares);
	Mushset(MM_A_PLIM, mm_qu.qu_plim);

	Mushlset(MM_A_MAXUSE, mm_maxuse);
	Mushset(MM_A_WREM, mm_wrem);
	Mushset(MM_A_WMAX, mm_wmax);
	Mushset(MM_A_WINC, mm_winc);
	Mushset(MM_A_DREM, mm_drem);
	Mushset(MM_A_DMAX, mm_dmax);
	Mushset(MM_A_DINC, mm_dinc);
	Mushset(MM_A_LEFT, mm_left);
	Mushset(MM_A_LGAP, mm_lgap);
	Mushset(MM_A_SMAX, mm_smax);
}
