#include "old.h"

rline()
{
	char *p1;
	int c;

loop0:
	p1 = sbuf;
loop:
	c = getchar();
	if(c <= 0)
		exit(0);
	if(c == '#')
		goto loop0;
	if(c != '*') {
		*p1++ = c;
		goto loop;
	}
	switch(getchar()) {

	case '#':
		goto loop0;

	case '*':
		if(p1 != sbuf+4) {
			printf("bad input\n");
			goto loop0;
		}
		sbuf[0] =+ 'a'-'1';
		sbuf[2] =+ 'a'-'1';
		*p1++ = '\0';
		return;

	case '0':
		exit(0);

	case '1':
		spread("");
		return;

	case '2':
		spread("first");
		return;

	case '3':
		spread("clock");
		return;

	case '4':
		spread("score");
		return;

	case '5':
		spread("remove");
		return;

	case '6':
		spread("repeat");
		return;

	case '7':
		spread("save");
		return;

	case '8':
		spread("restore");
		return;
	}
	printf("bad option\n");
	goto loop;
}

spread(s)
char *s;
{
	char *p;

	p = sbuf;
	while(*p++ = *s++) ;
}

pboard()
{
	int i, x, y, c, p;

	i = 0;
	x = 8;
	while(x--) {
		y = 8;
		while(y--) {
			p = board[i++];
			if(p == 0) {
				printf("space\n");
				continue;
			}
			if(p < 0)
				printf("white "); else
				printf("black ");
			putpiece("kqrbnp pnbrqk"[p+6]);
		}
		pause();
		printf("end\n");
		pause();
	}
}

putpiece(p)
{
	char *s;

	s = "god only knows";
	switch(p) {

	case 'p':
		s = "pawn";
		break;

	case 'n':
		s = "knight";
		break;

	case 'b':
		s = "bishop";
		break;

	case 'r':
		s = "rook";
		break;

	case 'q':
		s = "queen";
		break;

	case 'k':
		s = "king";
		break;
	}
	printf("%s\n", s);
}

out1(m)
{
	putnumb(moveno);
	pause();
	out(m);
	pause();
}

out(m)
int m;
{
	int from, to, epf, pmf;

	from = m>>8;
	to = m&0377;
	mantom? bmove(m): wmove(m);
	epf = pmf = 0;
	switch(amp[-1]) {

	case 0:
	case 1:
		stdp(board[to]);
	ed:
		printf("at\n");
		stdb(from);
		if(amp[-2]) {
			printf("takes\n");
			stdp(amp[-2]);
			printf("at\n");
		} else
			printf("to\n");
		stdb(to);
		break;

	case 3:
		printf("castle queen side\n");
		break;

	case 2:
		printf("castle king side\n");
		break;

	case 4:
		epf = 1;
		putpiece('p');
		goto ed;

	case 5:
		pmf = 1;
		putpiece('p');
		goto ed;
	}
	if(pmf) {
		printf("becomes\n");
		putpiece('q');
	}
	if(epf) {
		printf("en passent\n");
	}
	if(check())
		printf("check\n");
	mantom? bremove(): wremove();
}

stdp(p)
int p;
{

	if(p < 0)
		p = -p;
	p = "ppnbrqk"[p];
	putpiece(p);
}

stdb(b)
int b;
{
	int r, f;

	r = b/8;
	if((f = b%8) < 4)
		putpiece('q'); else {
		putpiece('k');
		f = 7-f;
	}
	f = "rnb\0"[f];
	if(f)
		putpiece(f);
	putnumb(mantom? r+1: 8-r);
}

prtime(a, b)
{

	printf("compute time is\n");
	putnumb(a);
	printf("real time is\n");
	putnumb(b);
	pause();
}

putnumb(n)
{

	if(n <= 12) {
		putdig(n);
		putchar('\n');
		return;
	}
	if(n <= 19) {
		putdig(n+1);
		printf("teen\n");
		return;
	}
	if(n >= 100) {
		putnumb(n/100);
		printf("hundred\n");
		n =% 100;
		if(n)
			putnumb(n);
		return;
	}
	putdig(n/10+11);
	printf("tee\n");
	n =% 10;
	if(n)
		putnumb(n);
}

putdig(n)
{
	char *s;

	s = "god only knows";
	switch(n) {

	case 0:
		s = "zero";
		break;

	case 1:
		s = "one";
		break;

	case 2:
		s = "two";
		break;

	case 3:
		s = "three";
		break;

	case 4:
	case 15:
		s = "four";
		break;

	case 5:
		s = "five";
		break;

	case 6:
	case 17:
		s = "six";
		break;

	case 7:
	case 18:
		s = "seven";
		break;

	case 8:
	case 19:
		s = "eight";
		break;

	case 9:
	case 20:
		s = "nine";
		break;

	case 10:
		s = "ten";
		break;

	case 11:
		s = "eleven";
		break;

	case 12:
		s = "twelve";
		break;

	case 13:
		s = "twen";
		break;

	case 14:
		s = "thir";
		break;

	case 16:
		s = "fif";
		break;
	}
	printf(s);
}

pause()
{

	printf("...\n");
}

score1(m)
{
	if(!mantom) {
		putnumb(moveno);
		pause();
	}
	out(m);
	pause();
}

score()
{
	int *p;

	p = amp;
	while(amp[-1] != -1) {
		mantom? wremove(): bremove();
		decrem();
	}
	posit(score1, p);
	printf("the end\n");
}
