/*	@(#)setprivs.c	4.2	(Melbourne)	82/02/20	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <sys/mush.h>
#include <mushmuck.h>
#include <udata.h>
#include <lpdquota.h>

setprivs(uid, mm, lq, dq, dqf, ud)
register uid;
register struct mushmuck *mm;
register struct lpquota *lq;
register struct dquot *dq;
register char (*dqf)[32];
register struct udata *ud;
{
	putudata(uid, ud);
	putlpquota(uid, lq);
	putmush(uid, mm, (struct mushmuck *)0);
	putdiscq(uid, dq, dqf);
}
