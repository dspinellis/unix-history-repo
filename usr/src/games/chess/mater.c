#include "old.h"

mate(n, f)
{
	int a, b;

	mantom = !mantom;
	if(f == 0) {
		b = mater(n);
		mantom = !mantom;
		return(b);
	}
	b = 0;
	if(matflg) {
		a = 1;
		while(!mater(a)) {
			if(a >= n) {
				matflg = 0;
				return(0);
			}
			a++;
		}
		b = abmove;
		goto out;
	}
	a = n;
	while(mater(a)) {
		if(a == mdepth) {
			printf("Forced mate\n");
			matflg++;
		}
		b = abmove;
		if(a == 0)
			break;
		a--;
	}
out:
	mantom = !mantom;
	if(b) {
		abmove = b;
		return(1);
	}
	return(0);
}

mater(ns)
{
	int *p1, *p2, *p3, f;

	if(intrp || --ns < 0)
		return(0);
	p1 = lmp;
	p2 = p1;
	p3 = p1;
	mantom? wgen(): bgen();
	while(p2 != lmp) {
		p2++;
		mantom? wmove(*p2): bmove(*p2);
		if((!mantom && !battack(wkpos) && wattack(bkpos)) ||
		  (mantom && !wattack(bkpos) && battack(wkpos))) {
			*p3 = *p2;
			p3++;
		}
		mantom? wremove(): bremove();
		p2++;
	}
	lmp = p3;
	p2 = p1;
	while(p2 != lmp) {
		mantom? wmove(*p2): bmove(*p2);
		f = xmater(ns);
		mantom? wremove(): bremove();
		if(f) {
			abmove = *p2;
			lmp = p1;
			return(1);
		}
		p2++;
	}
	lmp = p1;
	return(0);
}

xmater(ns)
{
	int *p1, *p2, f;

	p1 = lmp;
	p2 = p1;
	mantom? bagen(): wagen();
	if(p2+2 == lmp && rept() == 0)
		ns++;
	while(p2 != lmp) {
		p2++;
		mantom? bmove(*p2): wmove(*p2);
		f = mater(ns);
		mantom? bremove(): wremove();
		if(!f) {
			lmp = p1;
			return(0);
		}
		p2++;
	}
	lmp = p1;
	return(1);
}
