#include "fio.h"
f_rew(a) alist *a;
{
	unit *b;
	if(a->aunit>=MXUNIT || a->aunit<0) err(a->aerr,101,"rewind");
	b = &units[a->aunit];
	if(b->ufd==NULL && fk_open(READ,SEQ,FMT,a->aunit)) err(a->aerr,114,"rewind")
	if(!b->useek) err(a->aerr,106,"rewind")
	if(b->uwrt)
	{	nowreading(b);
		t_runc(b);
	}
	rewind(b->ufd);
	b->uend=0;
	return(0);
}
