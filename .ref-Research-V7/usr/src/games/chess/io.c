#include "old.h"

rline()
{
	char *p1;
	int c;

	p1 = sbuf;
	while((c = getchar()) != '\n')
		if(c <= 0)
			onhup(); else
			*p1++ = c;
	*p1++ = '\0';
}

getchar()
{
	int c;

loop:
	c = 0;
	read(0, &c, 1);
	if(c == 0 && intrp) {
		intrp = 0;
		goto loop;
	}
	return(c);
}

pboard()
{
	int i, x, y, c, p;

	c = 0;
	i = 0;
	x = 8;
	while(x--) {
		if(!mantom || mfmt)
			putchar('1'+x); else
			putchar('8'-x);
		putchar(' ');
		c++;
		y = 8;
		while(y--) {
			c++;
			putchar(' ');
			if(p = board[i++])
				putchar("kqrbnp PNBRQK"[p+6]); else
				if((c&1)!=0)
					putchar('*'); else
					putchar('-');
		}
		putchar('\n');
		if(intrp)
			return;
	}
	if(mfmt)
		printf("\n   a b c d e f g h"); else
		printf("\n   q q q q k k k k\n   r n b     b n r");
		printf("\n");
}

out1(m)
{
	printf("%d. ", moveno);
	if(mantom)
		printf("... ");
	out(m);
	putchar('\n');
}

out(m)
int m;
{
	int from, to, epf, pmf;

	from = m>>8;
	to = m&0377;
	if(mfmt) {
		algco(from);
		algco(to);
		return;
	}
	mantom? bmove(m): wmove(m);
	epf = pmf = 0;
	switch(amp[-1]) {

	case 0:
	case 1:
		stdp(board[to]);
	ed:
		putchar('/');
		stdb(from);
		if(amp[-2]) {
			putchar('x');
			stdp(amp[-2]);
			putchar('/');
		} else
			putchar('-');
		stdb(to);
		break;

	case 3:
		putchar('o');
		putchar('-');

	case 2:
		putchar('o');
		putchar('-');
		putchar('o');
		break;

	case 4:
		epf = 1;
		putchar('p');
		goto ed;

	case 5:
		pmf = 1;
		putchar('p');
		goto ed;
	}
	if(pmf) {
		putchar('(');
		putchar('q');
		putchar(')');
	}
	if(epf) {
		putchar('e');
		putchar('p');
	}
	if(check())
		putchar('+');
	mantom? bremove(): wremove();
}

stdp(p)
int p;
{

	if(p < 0)
		p = -p;
	p = "ppnbrqk"[p];
	putchar(p);
}

stdb(b)
int b;
{
	int r, f;

	r = b/8;
	if((f = b%8) < 4)
		putchar('q'); else {
		putchar('k');
		f = 7-f;
	}
	f = "rnb\0"[f];
	if(f)
		putchar(f);
	putchar(mantom? r+'1': '8'-r);
}

algco(p)
int p;
{
	putchar('a'+(p%8));
	putchar('8'-(p/8));
}

putchar(c)
{

	switch(c) {

	case '\t':
		do
			putchar(' ');
		while(column%8);
		return;

	case '\n':
		column = 0;
		break;

	default:
		column++;
	}
	write(1, &c, 1);
}

prtime(a, b)
{

	printf("time = %d/%d\n", a, b);
}

score1(m)
{
	if(intrp)
		return;
	if(!mantom) {
		if(moveno < 10)
			putchar(' '); else
			putchar(moveno/10 + '0');
		putchar(moveno%10 + '0');
		putchar('.');
		putchar(' ');
	} else
		while(column < 20)
			putchar(' ');
	out(m);
	if(mantom)
		putchar('\n');
}

score()
{
	int *p;

	putchar('\n');
	p = amp;
	while(amp[-1] != -1) {
		mantom? wremove(): bremove();
		decrem();
	}
	posit(score1, p);
	putchar('\n');
}
