/* sequential formatted external common routines*/
#include "fio.h"
extern char *fmtbuf;
e_rsfe()
{	int n;
	n=en_fio();
	fmtbuf=NULL;
	return(n);
}
c_sfe(a,flag) cilist *a; /* check */
{	unit *p;
	if(a->ciunit >= MXUNIT || a->ciunit<0)
		err(a->cierr,101,"startio");
	p = &units[a->ciunit];
	if(p->ufd==NULL && fk_open(flag,SEQ,FMT,a->ciunit)) err(a->cierr,114,"sfe")
	if(!p->ufmt) err(a->cierr,102,"sfe")
	return(0);
}
e_wsfe()
{	return(e_rsfe());
}
