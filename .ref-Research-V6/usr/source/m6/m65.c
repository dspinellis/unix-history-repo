#

#include "m6.h"

define() {
	remove(1);
	newdef(decbin(3));
	setdef(arg(1),arg(2)); 
}

newdef(n) {
	pushdef();
	de = &df[df->dtext = dend]; 
	if((df->dswitch = n)<0) trashflag++;
}

setdef(s1,s2) {
	copydef(s1);
	df->dtext = de -df;
	copydef(s2); 
}

copydef(s) 
char *s; 
{
	while(*de++ = *s++)
		if(de>dmax) diag("Definition table overflow"); 
}

char *
arg(n) {
	char *p;
	p = &gf->ga0;
	while(--n>=0 && p->word!=0) p =+ p->word;
	return(p->word!=0?p+2:p); 
}

function(n) {
	if(n==0) ;
	else if(1<=n && n<=13) binop(n);
	else {
		switch(n) {
		case 20: 
			doif(); 
			return;
		case 21: 
			define(); 
			return;
		case 22: 
			copy(); 
			return;
		case 23: 
			meta(); 
			return;
		case 24: 
			size(); 
			return;
		case 25: 
			substr(); 
			return;
		case 26: 
		case 27: 
			go(n); 
			return;
		case 28: 
			remove(1); 
			return;
		case 29: 
			dnl(); 
			return;
		case 32: 
			quote(); 
			return;
		case 33: 
			result(finddef(1)->dswitch); 
			return;
		case 34: 
			list(); 
			return;
		case 35: 
			traceflag = comp(arg(1),one); 
			return;
		}
	} 
}
