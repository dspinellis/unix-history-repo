#

#include "m6.h"


doif() {
	int i;
	int *p;
	char *arg();
	i = 1;
	while(!comp(arg(i),one)) if((i =+ 2)>8) return;
	p = arg(i+1) - 2;
	ge = move(p,&gf->ga0,p->word+1);
	setscan(dummy); 
}

bindec(n) {
	if(n == 0) return;
	bindec(n/10);
	*ge++ = (n%10) + '0'; 
}

result(n) {
	char *p;
	setscan(dummy);
	ge = 2 + (p = &gf->ga0);
	if(n<0) {
		*ge++ = '-'; 
		n = -n; 
	}
	if(n==0) *ge++ = '0'; 
	else bindec(n);
	*ge++ = 0; 
	ge = (ge+1)&0177776;
	p->word = ge - p;
	*ge++ = *ge++ = 0; 
}

binop(code) {
	int r1,r2;
	int r,t;
	int arg1, arg2;
	arg1 = decbin(1);
	arg2 = decbin(2);
	if(code < 7)	/* relationals */
		result((code & ((arg1<arg2)?4:(arg1==arg2)?2:1)) != 0);
	else if(code < 9) 	/* seq=7 sne=8 */
		result((code==7)==comp(arg(1),arg(2)));
	else switch (code) {
	case 9: 
		result(arg1+arg2); 
		return;
	case 10: 
		result(arg1-arg2); 
		return;
	case 11: 
		result(arg1*arg2); 
		return;
	case 12: 
		result(arg1/arg2);
		if(arg2==0) gf->ga0 = 0; 
		return;
	case 13:	/* exp */
		r = 1;
		while(arg2-->0) r =* arg1;
		result(r);
		if(arg2<-1) gf->ga0 = 0; 
	} 
}

decbin(i) {
	char *s;
	char t;
	int n;
	if(t = (*(s = arg(i))=='-')) s++;
	n = 0;
	while(*s>='0' && *s<='9') n = 10*n + *s++ - '0';
	return(t?-n:n); 
}

dnl() {
	char d;
	d = 0;
	while(d=getchar()) if(d=='\n') return; 
}

quote() {
	char *p,*s;
	p = finddef(1);
	s = &p[p->dtext];
	while(c = *s++) put(); 
}

list() {
	int n,i;
	char *p;
	if((n=decbin(1))<=0) return;
	p = df;
	for(i=1;;) {
		if(p<=d0) return;
		if(p->dswitch>=0)
			if(i++>=n) break;
		p =+ p->prev; 
	}
	for(p = &p->dident;c = *p++;)
		put(); 
}

copy() {
	char *p;
	p = finddef(1);
	remove(2);
	newdef(p->dswitch);
	setdef(arg(2),p+p->dtext); 
}

go(n) {
	if(comp(arg(1),one)) {
		popget();
		if(lg>0)
			if(n==26) popget();
			else setscan(gf->mframe);	/* gobk=27 */
	}  
}

size() {
	int i;
	char *p;
	i = 0;
	p = arg(1);
	while(*p++ != 0) i++;
	result(i); 
}

meta() {
	char d;
	int i;
	char *arg();
	if((d = *arg(2))!=0) {
		for(i=0;i<NMETA;i++)
			if(metas[i]== *arg(1)) metas[i] = d; 
	}
}

substr() {
	char *s;
	int arg2,arg3;
	char *arg();
	newdef(-1);
	setscan(df);
	s = arg(1);
	arg2 = decbin(2);
	arg3 = *arg(3)==0?32767:decbin(3);
	if(arg2<1) { 
		arg3 =+ arg2-1; 
		arg2=1; 
	}
	while(--arg2>0 && *s!=0) s++;
	while(arg3-->0) {
		if((*de++ = *s++)==0) return;
		if(de>dmax) diag("No room for substr"); 
	}
	*de++ = 0; 
}
