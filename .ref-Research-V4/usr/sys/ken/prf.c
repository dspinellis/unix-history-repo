#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../seg.h"

#define	KL	0177560

struct	{
	int	rsr;
	int	rbr;
	int	xsr;
	int	xbr;
	int	csw;
};

char	*panicstr;
int	kisa6;

printf(fmt,x1,x2,x3,x4,x5,x6,x7,x8,x9,xa,xb,xc)
char fmt[];
{
	register char *s;
	register *adx, c;

	adx = &x1;
loop:
	while((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(c);
	}
	switch(c = *fmt++) {

	case 'd':
		if(*adx < 0) {
			putchar('-');
			*adx = -*adx;
		}

	case 'l':
	case 'o':
		printn(*adx, c=='o'? 8: 10);
		break;

	case 'c':
		putchar(*adx);
		break;

	case 's':
		s = *adx;
		while(c = *s++)
			putchar(c);
	}
	adx++;
	goto loop;
}

printn(n, b)
{
	register a;

	if(a = ldiv(n, b))
		printn(a, b);
	putchar(lrem(n, b) + '0');
}

putchar(c)
{
	register rc, s;

	rc = c;
	if(KL->csw == 0)
		return;
	while((KL->xsr&0200) == 0);
	if(rc == 0)
		return;
	s = KL->xsr;
	KL->xsr = 0;
	KL->xbr = rc;
	if(rc == '\n') {
		putchar('\r');
		putchar(0177);
		putchar(0177);
	}
	putchar(0);
	KL->xsr = s;
}

panic(s)
char *s;
{
	panicstr = s;
	kisa6 = KISA->r[6];
	update();
	printf("panic: %s\n", s);
	for(;;)
		idle();
}
