#include "old.h"

main()
{
	int i;

	printf("Chess\n");
	itinit();
	lmp = lmbuf;
	amp = ambuf;
	*amp++ = -1;
	*lmp++ = -1;		/* fence */
	bookf = open("/usr/lib/book", 0);
	if(bookf > 0)
		read(bookf, &bookp, 2);
	i = 64;
	while(i--)
		dir[i] = (edge[i/8]<<6) | edge[i%8];
	play(0);
}

ctime(s, t)
{

	printf("%s: %d:%d%d\n", s, t/60, (t/10)%6, t%10);
}

check()
{

	return((!wattack(bkpos) || !battack(wkpos))? 1: 0);
}

increm()
{

	clktim[mantom] =+ clock();
	if(mantom)
		moveno++;
	mantom = !mantom;
}

decrem()
{

	mantom = !mantom;
	if(mantom)
		moveno--;
}

stage()
{
	int i, a;

	qdepth = depth+8;
	for(i=0; i<13; i++)
		pval[i] = ipval[i];
	value = 0;
	for(i=0; i<64; i++) {
		a = board[i];
		value =+ (pval+6)[a];
	}
	if(value > 150)
		gval = 1; else
	if(value < -150)
		gval = -1; else
		gval = 0;
	i = -6;
	while(i <= 6) {
		a = (pval+6)[i];
		if(a < 0)
			a =- 50; else
			a =+ 50;
		if(a < 0)
			a = -((-a)/100); else
			a =/ 100;
		if(i)
			(pval+6)[i] = a*100-gval;
		i++;
	}
	a = 13800;
	i = 64;
	while(i--)
		a =- abs((pval+6)[board[i]]);
	if(a > 4000)
		game = 3; else
	if(a > 2000)
		game = 2; else
	if(moveno > 5)
		game = 1; else
		game = 0;
}

posit(f, p, a)
int (*f)();
int *p;
{
	int m;

	while(amp != p) {
		m = amp[3]<<8;
		m =| amp[4]&0377;
		(*f)(m, a);
		if(mantom) {
			bmove(m);
			moveno++;
			mantom = 0;
		} else {
			wmove(m);
			mantom = 1;
		}
	}
}

rept1(m, a)
int *a;
{
	int i;

	if(mantom != a[64])
		return;
	for(i=0; i<64; i++)
		if(board[i] != a[i])
			return;
	a[65]++;
}

rept()
{
	int a[66], i, *p;

	for(i=0; i<64; i++)
		a[i] = board[i];
	a[64] = mantom;
	a[65] = 0;
	p = amp;
	while(amp[-1] != -1) {
		if(amp[-2])
			break;
		i = board[amp[-3]];
		if(i == 1 || i == -1)
			break;
		mantom? wremove(): bremove();
		decrem();
	}
	posit(rept1, p, a);
	return(a[65]);
}
