#include "old.h"

wplay()
{
	int v1, v2, *p1, *p2, *p3, ab;

	if(value < ivalue)
		ivalue = value;
	ab = 0;
	v1 = 3000;
	ply = 0;
	p1 = statl();
	if(lmp == p1+2) {
		abmove = p1[1];
		lmp = p1;
		return(ivalue);
	}
	p2 = p1;
	mantom = !mantom;
	while(p2 != lmp) {
		p2++;
		wmove(*p2);
		if(testf) {
			mantom = !mantom;
			wstatic(1);
			mantom = !mantom;
		}
		if(rept())
			v2 = 0; else
			v2 = bplay1(v1);
		if(v2 < v1 && !mate(3, 0)) {
			ab = *p2;
			v1 = v2;
		}
		wremove();
		if(testf) {
			mantom = !mantom;
			printf("%6d ", v2);
			out(*p2);
			printf("\n");
			mantom = !mantom;
		}
		p2++;
	}
	if(ab == 0 && lmp != p1)
		ab = p1[1];
	mantom = !mantom;
	lmp = p1;
	abmove = ab;
	return(v1);
}

wplay1(ab)
int ab;
{
	int v1, v2, *p1, *p2;

	if(ply >= depth)
		return(wquies(ab));
	ply++;
	p1 = p2 = lmp;
	wgen();
	qsort(p1, lmp);
	v1 = 3000;
	while(p2 != lmp) {
		if(intrp)
			goto out;
		p2++;
		wmove(*p2);
		if(battack(wkpos)) {
			v2 = bplay1(v1);
			if(v2 < v1)
				v1 = v2;
		}
		wremove();
		if(v1 <= ab)
			goto out;
		p2++;
	}
out:
	ply--;
	lmp = p1;
	if(v1 == 3000) {
		v1--;
		if(!check())
			v1 = 0;
	}
	return(v1);
}

wquies(ab)
int ab;
{
	int *p1, *p2, *p3, v1, v2;

	if(ply >= qdepth)
		return(ivalue);
	p1 = p2 = p3 = lmp;
	wgen();
	while(p2 != lmp) {
		v1 = *p2++;
		if(v1 != value && v1 <= ivalue+50) {
			*p3++ = (((pval+6)[board[*p2>>8]]/100)<<8) |
				(-(pval+6)[board[*p2&0377]]/100);
			*p3++ = *p2;
		}
		p2++;
	}
	if(p3 == p1) {
		lmp = p1;
		return(value);
	}
	ply++;
	qsort(p1, p3);
	lmp = p3;
	p2 = p1;
	v1 = value;
	while(p2 != lmp) {
		p2++;
		wmove(*p2);
		if(battack(wkpos)) {
			v2 = bquies(v1);
		} else
			v2 = 3000;
		if(v2 < v1)
			v1 = v2;
		wremove();
		if(v1 <= ab)
			goto out;
		p2++;
	}
out:
	ply--;
	lmp = p1;
	return(v1);
}
