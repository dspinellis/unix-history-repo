
/*
 * Scaled down version of C Library printf and sprintf.
 * Only %c %s %u %d (==%u) %o %D %O %x %X are recognized.
 *
 * Frank Pronk
 * Copyright (c) 1983
 */

static	char *lp;

char *
sprint (buf, fmt, x1)
char *buf, *fmt;
unsigned x1;
{
	(void) doprint (buf, fmt, &x1);
}

fprint (fd, fmt, x1)
char *fmt;
unsigned x1;
{
	register int n;
	char line[128];

	n = doprint (line, fmt, &x1);
	return (write (fd, line, n));
}

print (fmt, x1)
char *fmt;
unsigned x1;
{
	register int n;
	char line[128];

	n = doprint (line, fmt, &x1);
	return (write (1, line, n));
}

static
doprint(buf, fmt, adx)
char *buf;
register char *fmt;
register unsigned *adx;
{
	register c;
	char *s;

	lp = buf;
loop:
	while((c = *fmt++) != '%') {
		if(c == '\0') {
			*lp = '\0';
			return (lp - buf);
		}
		putchar(c);
	}
	switch(c = *fmt++) {
	case 'c':
		putchar((char)*adx);
		break;

	case 'd':
	case 'u':
	case 'o':
	case 'x':
		itoa((unsigned long)*adx, c);
		break;

	case 's':
		s = (char *)*adx;
		while(c = *s++)
			putchar(c);
		break;

	case 'D':
	case 'O':
	case 'X':
		itoa(*(unsigned long *)adx, c);
		adx += (sizeof(long) / sizeof(int)) - 1;
		break;

	default:
		putchar(c);
	}
	adx++;
	goto loop;
}

/*
 * Print an unsigned integer in base base.
 */
itoa (n, base)
register unsigned long n;
register char base;
{
	char prbuf[11];
	register int b;
	register char *cp;

	if (base == 'o' || base == 'O')
		b = 8;
	else if (base == 'x' || base == 'X')
		b = 16;
	else
		b = 10;
	if (b == 10 && (int)n < 0) {
		putchar('-');
		n = (unsigned)(-(int)n);
	}
	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
	do
		putchar(*--cp);
	while (cp > prbuf);
}

static
putchar(c)
char c;
{
/*	if(lp < &line[128])*/
		*lp++ = c;
}
