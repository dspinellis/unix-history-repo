/*write sequential formatted external*/
#include "fio.h"
#include "fmt.h"
extern int x_putc(),w_ed(),w_ned();
extern int xw_end(),xw_rev(),x_wSL();
s_wsfe(a) cilist *a;	/*start*/
{	int n;
	if(!init) f_init();
	if(n=c_sfe(a,WRITE)) return(n);
	reading=0;
	sequential=1;
	formatted=1;
	external=1;
	elist=a;
	cursor=recpos=0;
	scale=0;
	fmtbuf=a->cifmt;
	if(pars_f(fmtbuf)<0) err(a->cierr,100,"startio");
	curunit = &units[a->ciunit];
	cf=curunit->ufd;
	putn= x_putc;
	doed= w_ed;
	doned= w_ned;
	doend=xw_end;
	dorevert=xw_rev;
	donewrec=x_wSL;
	fmt_bg();
	cplus=0;
	cblank=curunit->ublnk;
	if(!curunit->uwrt) nowwriting(curunit);
	return(0);
}
x_putc(c)
{
	recpos++;
	putc(c,cf);
}
pr_put(c)
{	static flag new = 1;
	recpos++;
	if(c=='\n')
	{	new=1;
		putc(c,cf);
	}
	else if(new==1)
	{	new=0;
		if(c=='0') putc('\n',cf);
		else if(c=='1') putc('\f',cf);
	}
	else putc(c,cf);
}
x_wSL()
{
	recpos=0;
	cursor = 0;
	(*putn)('\n');
	return(1);
}
xw_end()
{
	(*putn)('\n');
	return(0);
}
xw_rev()
{
	if(workdone) (*putn)('\n');
	return(workdone=0);
}
