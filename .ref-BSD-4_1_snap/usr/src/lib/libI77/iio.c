#include "fio.h"
#include "fmt.h"
char *icptr,*icend;
icilist *svic;
extern int rd_ed(),rd_ned(),w_ed(),w_ned(),y_ierr();
extern int z_wnew();
int icnum,icpos;
z_getc()
{
	if(icptr >= icend) err(svic->iciend,(EOF),"endfile");
	if(icpos++ < svic->icirlen)
		return(*icptr++);
	else	err(svic->icierr,110,"recend");
}
z_putc(c)
{
	if(icptr >= icend) err(svic->icierr,110,"inwrite");
	if(icpos++ < svic->icirlen)
		*icptr++ = c;
	else	err(svic->icierr,110,"recend");
	return(0);
}
z_rnew()
{
	icptr = svic->iciunit + (++icnum)*svic->icirlen;
	icpos = 0;
}
s_rsfi(a) icilist *a;
{	int n;
	if(n=c_si(a)) return(n);
	reading=1;
	doed=rd_ed;
	doned=rd_ned;
	getn=z_getc;
	dorevert = donewrec = y_ierr;
	doend = z_rnew;
	return(0);
}
s_wsfi(a) icilist *a;
{	int n;
	if(n=c_si(a)) return(n);
	reading=0;
	doed=w_ed;
	doned=w_ned;
	putn=z_putc;
	dorevert = donewrec = y_ierr;
	doend = z_wnew;
	return(0);
}
c_si(a) icilist *a;
{
	fmtbuf=a->icifmt;
	if(pars_f(fmtbuf)<0)
		err(a->icierr,100,"startint");
	fmt_bg();
	sequential=formatted=1;
	external=0;
	cblank=cplus=scale=0;
	svic=a;
	icnum=icpos=0;
	icptr=svic->iciunit;
	icend=icptr+svic->icirlen*svic->icirnum;
	return(0);
}
z_wnew()
{
	while(icpos++ < svic->icirlen)
		*icptr++ = ' ';
	icpos = 0;
	icnum++;
}
e_rsfi()
{	int n;
	n = en_fio();
	fmtbuf = NULL;
	return(n);
}
e_wsfi()
{
	int n;
	n = en_fio();
	fmtbuf = NULL;
	if(icnum >= svic->icirnum)
		return(n);
	while(icpos++ < svic->icirlen)
		*icptr++ = ' ';
	return(n);
}
y_ierr()
{
	err(elist->cierr, 110, "iio");
}
