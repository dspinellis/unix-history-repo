/* read sequential formatted external */
#include "fio.h"
#include "fmt.h"
extern int x_getc(),rd_ed(),rd_ned();
extern int x_endp(),x_rev(),xrd_SL();
s_rsfe(a) cilist *a; /* start */
{	int n;
	if(!init) f_init();
	if(n=c_sfe(a,READ)) return(n);
	reading=1;
	sequential=1;
	formatted=1;
	external=1;
	elist=a;
	cursor=recpos=0;
	scale=0;
	fmtbuf=a->cifmt;
	curunit= &units[a->ciunit];
	cf=curunit->ufd;
	if(pars_f(fmtbuf)<0) err(a->cierr,100,"startio");
	getn= x_getc;
	doed= rd_ed;
	doned= rd_ned;
	fmt_bg();
	doend=x_endp;
	donewrec=xrd_SL;
	dorevert=x_rev;
	cblank=curunit->ublnk;
	cplus=0;
	if(curunit->uwrt) nowreading(curunit);
	return(0);
}
xrd_SL()
{	int ch;
	if(!curunit->uend)
		while((ch=getc(cf))!='\n' && ch!=EOF);
	cursor=recpos=0;
	return(1);
}
x_getc()
{	int ch;
	if(curunit->uend) return(EOF);
	if((ch=getc(cf))!=EOF && ch!='\n')
	{	recpos++;
		return(ch);
	}
	if(ch=='\n')
	{	ungetc(ch,cf);
		return(ch);
	}
	if(feof(cf))
	{	errno=0;
		curunit->uend=1;
		return(-1);
	}
	return(-1);
}
x_endp()
{
	xrd_SL();
	return(0);
}
x_rev()
{
	xrd_SL();
	return(0);
}
