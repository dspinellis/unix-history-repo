#

#include "m6.h"

pushget() {
	ge = (ge+1)&0177776;
	ge->word = gf-ge; 
	gf = ge; 
	ge =+2;
	++lg;
}

dotrace() {
	char *arg();
	int i,j;
	printf("\n%d %c%s",lg,sharp,arg(0));
	for(j=9;j>0&&*arg(j)==0;j--);
	for(i=1;i<=j;i++)
		printf("%c%c%s%c",comma,lquote,arg(i),rquote);
	printf("%c\n",c); 
}

popget() {
	ge = gf; 
	gf =+ gf->word;
	--lg;
	if(gf<g0) diag("Software error"); 
}

pushput() {
	if(pe&1) {
		pf->prev =| 1;
		pe++; 
	}
	pe->word = pf-pe; 
	pf = pe; 
	pe =+2;
}

popput() {
	pe = pf; 
	pf =+ pf->word;
	if(pf->prev&1) {
		pe--;
		pf->prev =& 0177776; 
	}
	if(pf<p0) diag("Software error"); 
}

pushdef() {
	de = (de+1)&0177776;
	de->word = df-de; 
	df = de; 
	de =+2;
}

put() {
	if(lp>0) {
		*pe++ = c; 
		if(pe>pmax) diag("Arg collection overflow"); 
	}
	else putchar(c); 
}

get() {
	char *p;
	int n;
	if(lg==0) 
		c = getchar();
	else while(1) {
		if(gf->marg!=0) {
			if((c = gf[gf->marg++])==0) gf->marg = 0;
			else return; 
		}
		c = (p = gf->mframe)[gf->mchar++];
		if(c!=dollar) return;
		n = p[gf->mchar] - '0';
		if(n<0 || n>9) return;
		++gf->mchar;
		gf->marg = arg(n) - gf; 
	}
}
