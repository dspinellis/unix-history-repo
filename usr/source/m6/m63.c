#

#include "m6.h"

char *
finddef(n) {
	char *p;
	if((p = lookup(arg(n)))==0)
		if((p = lookup(""))==0) diag("Software error");
	return(p); 
}

char *
lookup(s) {
	char *dt;
	dt = df;
	while(dt>d0) {
		if(dt->dswitch>=0 && comp(s,&dt->dident)) return(dt);
		dt =+ dt->prev; 
	}
	return(0); 
}

comp(s,t) 
char *s, *t; 
{
	for(;*s++ == *t;t++)
		if(*t==0) return(1);
	return(0); 
}

remove(n) {
	char *p;
	if(p = lookup(arg(n))) if(p>d0+2) {
		trashflag++;
		p->dswitch = -1; 
	} 
}

trash() {
	char *p,*q,*r;
	if(lg>0) return;
	while(df->dswitch<0) {
		de = df;
		df =+ df->prev;
		trashflag--; 
	}
	if(trashflag<=0) return;
	de = (de+1)&0177776;
	revptr(de,df,&p,&q);
	q = p;
	for(;p->word!=0;p=+r->word) {
		r = p;
		if(p->dswitch>=0) {
			r = q;
			q = move(p,q,p->word); 
		} 
	}
	q->word = 0;
	revptr(d0,d0+2,&df,&de); 
}


revptr(p,q,np,nq) 
char *p, *q, **np, **nq;
{
	int t;
	p->word = 0;
	while((t = q->word)!=0) {
		q->word = p-q; 
		p = q; 
		q =+ t;
		if(q<d0 || q>dmax) diag("Software error"); 
	}
	*np = p;
	*nq = q; 
}

char *
move(from,to,count) 
char *from, *to;
{
	while(count-->0) *to++ = *from++;
	return(to); 
}
