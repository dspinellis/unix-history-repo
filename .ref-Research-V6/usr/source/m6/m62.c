#

#include "m6.h"

control() {

	while(1) {
		get();
		if(c==0 && lg>0)  {
			popget(); 
			trash(); 
			rescan = 1; 
		}
		else {
			if(!rescan) put();
			else if(lq>0) {
				if(c==lquote) {
					lq++; 
					put(); 
				}
				else if(c==rquote || c==0)  {
					lq--;
					if(lq>0 || l9>0) put (rquote); 
				}
				else put(); 
			}
			else if(l9>0) 
				if(c==colon||c==0||c==semi) 
					if(--l9<=0) endcall();
					else put();
				else {
					if(c==sharp) l9++;
					else if(c==lquote) lq++;
					put(); 
				}
			else {
				if(c==sharp) begincall();
				else if(c==lquote) lq++;
				else if(lp>0) {
					if(c==colon||c==0||c==semi) endcall();
					else if(c==comma) newarg();
					else put(); 
				}
				else if(c==0) return;	/* lg=lp=lq=0 */
				else put(); 
			} 
		} 
	} 
}

endcall() {
	char *pt, *p;
	rescan = c!=semi;
	newarg();
	pushget();
	pt = &pf->pa0;
	ge = move(pt,&gf->ga0,pe-pt);
	if(ge>gmax) diag("Call stack overflow");
	if(traceflag) dotrace();
	p = finddef(0);
	setscan(p);
	popput();
	lp--;
	function(p->dswitch); 
}

begincall() {
	lp++;
	pushput();
	pe = pf+pend;
	pf->pan = pf->pa0 = 0;
}

newarg() {
	char *p;
	if(++pf->pan>=9) if(c==comma) l9++;
	*pe++ = 0;
	pe = (pe+1)&0177776;
	p = &pf->pa0;
	while(p->word!=0) p =+ p->word;
	p->word = pe - p;
	*pe++ = *pe++ = 0; 
}

setscan(p) 
char *p;
{
	gf->mchar = (gf->mframe = p)->dtext;
	gf->marg = 0; 
}
