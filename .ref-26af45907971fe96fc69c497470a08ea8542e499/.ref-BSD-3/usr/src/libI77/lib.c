#include "fio.h"
setcilist(x,u,fmt,rec,xerr,end) cilist *x;
char *fmt;
{
	x->ciunit=u;
	x->cifmt=fmt;
	x->cirec=rec;
	x->cierr=xerr;
	x->ciend=end;
}
setolist(x,xunit,fname,sta,fm,rl,blnk,oerr) olist *x;
	char *fname,*sta,*fm,*blnk;
{
	x->oerr=oerr;
	x->ounit=xunit;
	x->ofnm=fname;
	x->ofnmlen=strlen(fname);
	x->osta=sta;
	x->ofm=fm;
	x->orl=rl;
	x->oblnk=blnk;
}
stcllist(x,xunit,stat,cerr) cllist *x; char *stat;
{
	x->cerr=cerr;
	x->cunit=xunit;
	x->csta=stat;
}
setalist(x,xunit,aerr) alist *x;
{
	x->aunit=xunit;
	x->aerr=aerr;
}
