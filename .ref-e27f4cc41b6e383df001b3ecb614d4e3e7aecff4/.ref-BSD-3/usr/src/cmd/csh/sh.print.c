/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C Shell
 */

p60ths(l)
	long l;
{

	l += 3;
	printf("%d.%d", (int) (l / 60), (int) ((l % 60) / 6));
}

psecs(l)
	long l;
{
	register int i;

	i = l / 3600;
	if (i) {
		printf("%d:", i);
		i = l % 3600;
		p2dig(i / 60);
		goto minsec;
	}
	i = l;
	printf("%d", i / 60);
minsec:
	i %= 60;
	printf(":");
	p2dig(i);
}

p2dig(i)
	register int i;
{

	printf("%d%d", i / 10, i % 10);
}

char	linbuf[64];
char	*linp = linbuf;

putchar(c)
	register int c;
{

	if ((c & QUOTE) == 0 && (c == 0177 || c < ' ' && c != '\t' && c != '\n')) {
		putchar('^');
		if (c == 0177)
			c = '?';
		else
			c |= 'A' - 1;
	}
	c &= TRIM;
	*linp++ = c;
	if (c == '\n' || linp >= &linbuf[sizeof linbuf - 2])
		flush();
}

draino()
{

	linp = linbuf;
}

flush()
{
	register int unit;

	if (haderr)
		unit = didfds ? 2 : SHDIAG;
	else
		unit = didfds ? 1 : SHOUT;
	if (linp != linbuf) {
		write(unit, linbuf, linp - linbuf);
		linp = linbuf;
	}
}

plist(vp)
	register struct varent *vp;
{
	register int (*wasintr)();

	if (setintr)
		wasintr = signal(SIGINT, pintr);
	for (vp = vp->link; vp != 0; vp = vp->link) {
		int len = blklen(vp->vec);

		printf(vp->name);
		printf("\t");
		if (len != 1)
			putchar('(');
		blkpr(vp->vec);
		if (len != 1)
			putchar(')');
		printf("\n");
	}
	if (setintr)
		signal(SIGINT, wasintr);
}
