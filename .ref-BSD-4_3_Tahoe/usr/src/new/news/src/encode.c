#include <stdio.h>

#ifdef SCCSID
static char	*SccsId = "@(#)encode.c	1.3	5/15/85";
#endif /* SCCSID */

/*
 * Produce a 7 bit printable encoding of stdin on stdout.
 *
 * Encoding uses acsii chars from ' ' .. 'z'
 * (040 .. 0172) (0x20 - 0x7a) inclusive
 *
 * Method is to expand 3 chars -> 4 6 bit ones.
 * Then collect 13 6 bit chars, and spread the 13th over
 * the preceding 12, so that each of the 12 chars is now
 * 6.5 bits.  These 2 6.5 bit chars are a little hard
 * to represent on most common machines (one of these days
 * sane hosts will have 1/2 bits just for this program)
 * so we take a pair of them, and represent that in 13 bits.
 * 13 bits (max value 8191) can be represented as
 *	A * 91 + B
 * where A < 91, B < 91  (91^2 == 8281, so it fits!)
 *
 * Each of A and B is encoded as a character by adding 32
 * to make it printable (ie: 0x20).
 *
 * The termination conditions are foul beyond belief.  Don't
 * monkey with them!
 *
 * If you think its a fluke that 040 .. 0171 just happen to
 * be the chars that Piet Beertema's uucp 'f' protocol transmits
 * as single bytes, you're insane.  0172 chars are produced
 * with lower frequency than any other (given random data)
 * so the doubling that occurs with that we will just suffer.
 * (A newer 'f' proto, sometime, will probably not use 0172)
 */

/*
 * the following pair of characters cannot legally occur
 * in normal output (since 90*91 + 90 == 8280, which > 2^13)
 * so we use them to indicate that the data that follows is the
 * terminator.  The character immediately following this
 * pair is the length of the (expanded) terminator (which
 * otherwise might be indeterminable)
 */
#define	ENDMARK1	((90*91 + 90) / 91 + ' ')
#define	ENDMARK2	((90*91 + 90) % 91 + ' ')

main()
{
	register char *p;
	register char *e;
	register c;
	char b3[3];

	p = b3;
	e = b3 + 3;
	while ((c = getchar()) != EOF) {
		*p++ = c;
		if (p == e) {
			encode(b3, 3);
			p = b3;
		}
	}
	encode(b3, p - b3);
	flushout();
	exit(0);
}

static char b13[13];
static int cnt = 0;

encode(c, n)
	register char *c;
	int n;
{
	register char *p;
	register i = cnt;
	register j;
	char b4[4];

	p = b4;

	p[0] = (c[0] >> 2) & 0x3f;
	p[1] = ((c[0] & 0x3) << 4) | ((c[1] >> 4) & 0xf);
	p[2] = ((c[1] & 0xF) << 2) | ((c[2] >> 6) & 0x3);
	if (n == 3)
		p[3] = c[2] & 0x3f;
	else
		p[3] = n;

	c = &b13[i];
	for (j = 4; --j >= 0; i++) {
		if (i == 13) {
			dumpcode(b13, 13);
			c = b13;
			i = 0;
		}
		*c++ = *p++;
	}
	cnt = i;
}

flushout()
{
	putchar(ENDMARK1);
	putchar(ENDMARK2);
	putchar(cnt + ' ');
	dumpcode(b13, cnt);
}

dumpcode(p, n)
	register char *p;
	register int n;
{
	register last;
	register c;

	if (n == 13)
		n--, last = p[12];
	else if (n & 1)
		last = (1 << (6-1));
	else
		last = 0;

	for ( ; n > 0; n -= 2) {
		c = *p++ << 6;
		c |= *p++;
		if (last & (1 << (6-1)))
			c |= (1 << 12);
		last <<= 1;

		/*
		 * note: 91^2 > 2^13, 90^2 < 2^13, (91 + ' ') is printable
		 */

		/* oh for a compiler that would only do one division... */
		putchar((c / 91) + ' ');
		putchar((c % 91) + ' ');
	}
}
