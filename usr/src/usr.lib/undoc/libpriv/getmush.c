/*	@(#)getmush.c	4.1	(Melbourne)	82/02/21	*/

#include <sys/types.h>
#include <sys/mush.h>
#include <sys/quota.h>
#include <mushmuck.h>

#define	Mushlreq(req, var)	mm->var = mushlreq(uid, req)
#define	Mushreq(req, var)	mm->var = mushreq(uid, req)

long mushlreq();

getmush(uid, mm)
register uid;
register struct mushmuck *mm;
{
	Mushlreq(MM_A_FLAGS, mm_qu.qu_syflags);
	Mushlreq(MM_A_CLASS, mm_qu.qu_class);
	Mushreq(MM_A_SHARE, mm_qu.qu_shares);
	Mushreq(MM_A_PLIM, mm_qu.qu_plim);

	Mushlreq(MM_A_MAXUSE, mm_maxuse);
	Mushlreq(MM_A_USED, mm_used);
	Mushreq(MM_A_WREM, mm_wrem);
	Mushreq(MM_A_WMAX, mm_wmax);
	Mushreq(MM_A_WINC, mm_winc);
	Mushreq(MM_A_DREM, mm_drem);
	Mushreq(MM_A_DMAX, mm_dmax);
	Mushreq(MM_A_DINC, mm_dinc);
	Mushreq(MM_A_LEFT, mm_left);
	Mushreq(MM_A_LGAP, mm_lgap);
	Mushreq(MM_A_SMAX, mm_smax);
}
