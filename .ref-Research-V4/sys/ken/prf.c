#define	KL	0177560

struct	{
	int	rsr;
	int	rbr;
	int	xsr;
	int	xbr;
	int	csw;
};

printf(fmt,x1,x2,x3,x4,x5,x6,x7,x8,x9,xa,xb,xc)
char fmt[];
{
	char *s;
	int *adx, x, c;

	adx = &x1;
loop:
	while((c = *fmt++) != '%') {
		if(c == '\0')
			return;
		putchar(c);
	}
	x = *adx++;
	switch(c = *fmt++) {

	case 'd':
		if(x<0) {
			putchar('-');
			x = -x;
		}

	case 'l':
	case 'o':
		printn(x, c=='o'? 8: 10);
		break;

	case 'c':
		putchar(x);
		putchar(x>>8);
		break;

	case 's':
		s = x;
		while(c = *s++)
			putchar(c);
	}
	goto loop;
}

printn(n,b) {
	int a;

	if(a = ldiv(0,n,b))
		printn(a, b);
	putchar(lrem(0,n,b) + '0');
}

putchar(c)
{
	int s;

	if(KL->csw == 0)
		return;
	while((KL->xsr&0200) == 0);
	if(c == 0) return;
	s = KL->xsr;
	KL->xsr = 0;
	KL->xbr = c;
	if(c == '\n')
		putchar('\r');
	if(c == '\r') {
		putchar(0177);
		putchar(0177);
	}
	putchar(0);
	KL->xsr = s;
}

panic(s)
char *s;
{

	prproc();
	update();
	printf("panic: %s\n", s);
	for(;;)
		idle();
}
