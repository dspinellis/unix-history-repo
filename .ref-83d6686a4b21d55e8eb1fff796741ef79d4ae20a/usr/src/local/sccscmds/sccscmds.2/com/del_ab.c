# include	"../hdr/defines.h"

SCCSID(@(#)del_ab	2.1);

del_ab(p,dt,pkt)
register char *p;
register struct deltab *dt;
struct packet *pkt;
{
	extern	char *satoi(), *index();
	int n;
	register char *cp;
	extern char *Datep;

	if (*p++ != CTLCHAR)
		fmterr(pkt);
	if (*p++ != BDELTAB)
		return(*--p);
	NONBLANK(p);
	dt->d_type = *p++;
	NONBLANK(p);
	p = sid_ab(p,&dt->d_sid);
	NONBLANK(p);
	date_ab(p,&dt->d_datetime);
	p = Datep;
	NONBLANK(p);
	if ((cp = index(p,' ')) == 0)
		fmterr(pkt);
	bcopy(p,dt->d_pgmr,cp-p);
	dt->d_pgmr[cp-p] = 0;
	p = cp + 1;
	NONBLANK(p);
	p = satoi(p,&dt->d_serial);
	NONBLANK(p);
	p = satoi(p,&dt->d_pred);
	if (*p != '\n')
		fmterr(pkt);
	return(BDELTAB);
}
