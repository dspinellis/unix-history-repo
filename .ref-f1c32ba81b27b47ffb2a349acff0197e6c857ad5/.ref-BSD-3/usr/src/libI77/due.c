#include "fio.h"
s_rdue(a) cilist *a;
{
	int n;
	if(n=c_due(a,READ)) return(n);
	reading=1;
	if(curunit->uwrt) nowreading(curunit);
	return(0);
}
s_wdue(a) cilist *a;
{
	int n;
	if(n=c_due(a,WRITE)) return(n);
	reading=0;
	if(!curunit->uwrt) nowwriting(curunit);
	return(0);
}
c_due(a,flag) cilist *a;
{
	if(!init) f_init();
	if(a->ciunit>=MXUNIT || a->ciunit<0)
		err(a->cierr,101,"startio");
	recpos=sequential=formatted=0;
	external=1;
	curunit = &units[a->ciunit];
	elist=a;
	if(curunit->ufd==NULL && fk_open(flag,DIR,UNF,a->ciunit) ) err(a->cierr,104,"due");
	cf=curunit->ufd;
	if(curunit->ufmt) err(a->cierr,102,"cdue")
	if(!curunit->useek) err(a->cierr,104,"cdue")
	if(curunit->ufd==NULL) err(a->cierr,114,"cdue")
	fseek(cf,(long)(a->cirec-1)*curunit->url,0);
	curunit->uend = 0;
	return(0);
}
e_rdue()
{
	if(curunit->url==1 || recpos==curunit->url)
		return(0);
	fseek(cf,(long)(curunit->url-recpos),1);
	if(ftell(cf)%curunit->url)
		err(elist->cierr,200,"syserr");
	return(0);
}
e_wdue()
{
	return(e_rdue());
}
