#include "old.h"

play(f)
int f;
{
	int t, i, ts[9];

	clock();
	ts[8] = 0;
	if(f) goto first;
loop:
	intrp = 0;
	move();

first:
	if(manflg)
		goto loop;
	i = mantom;
	t = clktim[i];
	if(!bookm())
	if(!mate(mdepth, 1))
		xplay();
	if(intrp) {
		decrem();
		mantom? bremove(): wremove();
		goto loop;
	}
	if(!abmove) {
		printf("Resign\n");
		onhup();
	}
	makmov(abmove);
	i = clktim[i];
	t = i-t;
	times(ts);
	ts[8] = ts[1];
	if(i/moveno > 150) {
		if(depth > 1)
			goto decr;
		goto loop;
	}
	if(depth==3 && t>180)
		goto decr;
	if(depth==1 && t<60)
		goto incr;
	if(game==3 && t<60 && depth==2)
		goto incr;
	goto loop;

incr:
	depth++;
	goto loop;

decr:
	goto loop;
}

move()
{
	int a, *p, *p1;

loop:
	lmp = done();
	a = manual();
	p = done();
	p1 = p;
	while(p1 != lmp) {
		p1++;
		if(*p1++ == a) {
			lmp = p;
			makmov(a);
			return;
		}
	}
	printf("Illegal move\n");
	lmp = p;
	goto loop;
}

manual()
{
	int a, b, c;
	char *p1;
	extern out1;

loop:
	intrp = 0;
	stage();
	rline();
	sbufp = sbuf;
	if(match("save")) {
		save();
		goto loop;
	}
	if(match("test")) {
		testf = !testf;
		goto loop;
	}
	if(match("remove")) {
		if(amp[-1] != -1) {
			decrem();
			mantom? bremove(): wremove();
		}
		if(amp[-1] != -1) {
			decrem();
			mantom? bremove(): wremove();
		}
		goto loop;
	}
	if(match("exit"))
		exit();
	if(match("manual")) {
		manflg = !manflg;
		goto loop;
	}
	if(match("resign"))
		onhup();
	if(moveno == 1 && mantom == 0) {
		if(match("first"))
			play(1);
		if(match("alg")) {
			mfmt = 1;
			goto loop;
		}
		if(match("restore")) {
			restore();
			goto loop;
		}
	}
	if(match("clock")) {
		clktim[mantom] =+ clock();
		ctime("white", clktim[0]);
		ctime("black", clktim[1]);
		goto loop;
	}
	if(match("score")) {
		score();
		goto loop;
	}
	if(match("setup")) {
		setup();
		goto loop;
	}
	if(match("hint")) {
		a = xplay();
		out(abmove);
		printf(" %d\n", a);
		goto loop;
	}
	if(match("repeat")) {
		if(amp[-1] != -1) {
			a = amp;
			mantom? wremove(): bremove();
			decrem();
			posit(&out1, a);
		}
		goto loop;
	}
	if(*sbufp == '\0') {
		pboard();
		goto loop;
	}
	if((a=algin()) != 0) {
		mfmt = 1;
		return(a);
	}
	if((a=stdin()) != 0) {
		mfmt = 0;
		return(a);
	}
	printf("eh?\n");
	goto loop;
}

algin()
{
	int from, to;

	from = cooin();
	to = cooin();
	if(*sbufp != '\0') return(0);
	return((from<<8)|to);
}

cooin()
{
	int a, b;

	a = sbufp[0];
	if(a<'a' || a>'h') return(0);
	b = sbufp[1];
	if(b<'1' || b>'8') return(0);
	sbufp =+ 2;
	a = (a-'a')+8*('8'-b);
	return(a);
}

match(s)
char *s;
{
	char *p1;
	int c;

	p1 = sbufp;
	while((c = *s++) != '\0')
		if(*p1++ != c) return(0);
	sbufp = p1;
	return(1);
}

done()
{
	int *p;

	if(rept() > 3) {
		printf("Draw by repetition\n");
		onhup();
	}
	p = lmp;
	mantom? bagen(): wagen();
	if(p == lmp) {
		if(check())
			if(mantom)
				printf("White wins\n"); else
				printf("Black wins\n"); else
		printf("Stale mate\n");
		onhup();
	}
	return(p);
}

xplay()
{
	int a;

	stage();
	abmove = 0;
	a = mantom? bplay(): wplay();
	ivalue = a;
	return(a);
}

term()
{

	exit(0);
}
