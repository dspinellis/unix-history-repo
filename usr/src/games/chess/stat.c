#include "old.h"

statl()
{
	int *p1, *p2, *p3;

	p1 = p2 = lmp;
	stage();
	mantom? bagen(): wagen();
	if(lmp == p1+1)
		return(p1);
	while(p2 != lmp) {
		p3 = p2++;
		if(mantom) {
			bmove(*p2++);
			*p3 = bstatic(0);
			bremove();
		} else {
			wmove(*p2++);
			*p3 = wstatic(0);
			wremove();
		}
	}
	qsort(p1, lmp);
	return(p1);
}

wstatic(f)
{
	int i, j, h, (*p)();

	h = i = 0;
	while(p = wheur[h++]) {
		j = (*p)();
		if(f)
			printf("%4d ", j);
		i =+ j;
	}
	if(f)
		printf("=%4d ", i);
	return(-i);
}

bstatic(f)
{
	int i, j, h, (*p)();

	h = i = 0;
	while(p = bheur[h++]) {
		j = (*p)();
		if(f)
			printf("%4d ", j);
		i =+ j;
	}
	if(f)
		printf("=%4d ", i);
	return(-i);
}

xheur(ploc)
int ploc;
{
	int *p1, *p2, from, to, pie;

	pie = board[ploc];
	p1 = lmp;
	p2 = p1;
	mantom? wgen(): bgen();
	while(p2 != lmp) {
		p2++;
		to = *p2++ & 0377;
		if(to == ploc) {
			from = p2[-1] >> 8;
			if(abs(board[from]) < abs(pie)) {
				lmp = p1;
				return((pval+6)[pie]/60);
			}
		}
	}
	lmp = p1;
	return(0);
}

srnd(p)
int p;
{

	srnd1(p, uleft, -9);
	srnd1(p, uright, -7);
	srnd1(p, dleft, 7);
	srnd1(p, dright, 9);
	srnd1(p, up, -8);
	srnd1(p, left, -1);
	srnd1(p, right, 1);
	srnd1(p, down, 8);
	srnd1(p, 0, 0);
}

srnd1(p, m, o)
int p, m, o;
{

	if((dir[p]&m) == 0)
		control[p+o] =+ 10;
}
