#include <stdio.h>

#ifdef SCCSID
static char	*SccsId = "@(#)decode.c	1.3	5/15/85";
#endif /* SCCSID */

/*
 * This program is the inverse of encode
 *
 * It collects runs of 12 characters, combines pairs of those
 * to form 6 13 bit numbers, extracts the top bit of each of
 * those to make a 13th 6 bit character, and splits each of
 * the remaining 6 12 bit numbers to form 12 6 bit ones.
 *
 * The strings of 6 bit numbers are collected into groups of
 * 4 and converted into 3 8 bit characters.
 *
 * Now all that would be trivial, if we didn't need to worry
 * about ending all this correctly.  About 1/2 of the following
 * program wouldn't be here if the ending didn't matter....
 */

/*
 * the following pair of characters can never occur as a pair
 * in legal input (since (90 * 91 + 90) > 2^13) - they are
 * noticed at the beginning of a 12 char block, and serve to
 * indicate that this block is the terminator.  The character
 * immediately following is the (expanded) terminator length.
 */
#define	ENDMARK1	((90*91 + 90) / 91)
#define	ENDMARK2	((90*91 + 90) % 91)

main()
{
	register c;
	register char *p;
	register i;
	register first = 1;
	register cnt = 0;
	int errcnt = 0;
	char b12[12];
	char c12[12];

	p = b12;
	i = 12;

	while ((c = getchar()) != EOF) {
		if (c < ' ' || c >= (' ' + 91)) {
			if (errcnt++ == 0)
				fprintf(stderr, "decode: Bad data\n");
			continue;
		}
		if (i == 10 && p[-1] == ENDMARK1 && p[-2] == ENDMARK2) {
			cnt = c - ' ';
			i = 12;
			p -= 2;
			continue;
		}
		*p++ = c - ' ';
		if (--i == 0) {
			if (p == &b12[12]) {
				if (!first)
					pack12(c12, 12, 0);
				else
					first = 0;
				p = c12;
			} else {
				pack12(b12, 12, 0);
				p = b12;
			}
			i = 12;
		}
	}

	if (p >= &b12[0] && p < &b12[12]) {
		if (!first)
			pack12(c12, 12, i == 12 ? cnt : 0);
	} else
		pack12(b12, 12, i == 12 ? cnt : 0);

	if (i != 12) {
		if (p >= &b12[0] && p < &b12[12])
			pack12(b12, 12-i, cnt);
		else
			pack12(c12, 12-i, cnt);
	}

	exit(0);
}

static char b4[4];
static int cnt = 0;

pack12(p, n, last)
	register char *p;
	register n;
	int last;
{
	register i;
	register char *q;
	char b13[13];

	{
		register c;
		register c13;

		q = b13;
		c13 = 0;

		for (i = 0; i < n; i += 2) {
			c = *p++ * 91;
			c += *p++;
			c13 <<= 1;
			if (c & (1 << 12))
				c13 |= 1;
			*q++ = (c >> 6) & 0x3f;
			*q++ = c & 0x3f;
		}
		*q++ = c13;
		if (last)
			q = &b13[last];
	}

	p = b13;
	n = q - p;
	i = cnt;
	q = &b4[cnt];

	while (--n > 0) {
		*q++ = *p++;
		if (++i == 4) {
			char b3[3];
			register char *b = b4;

			/* inline expansion of pack6bit, to save calls ... */

			q = b3;
			*q++ = (b[0] << 2) | ((b[1] >> 4) & 0x3);
			*q++ = (b[1] << 4) | ((b[2] >> 2) & 0xf);
			*q = (b[2] << 6) | (b[3] & 0x3f);

			q = b3;
			while (--i > 0)
				putchar(*q++);

			q = b4;
		}
	}

	*q++ = *p++;	/* the last octet */
	++i;

	if (last || i == 4) {
		pack6bit(b4, i, last);
		i = 0;
	}

	cnt = i;
}

pack6bit(p, n, last)
	register char *p;
	register int n;
	int last;
{
	register char *q;
	register i = 3;
	char b3[3];

	if (last) {
		i = p[n-1];
		if (i >= 3) {
			fprintf(stderr, "Badly encoded file\n");
			i = 3;		/* do the best we can */
		}
	}

	q = b3;
	*q++ = (p[0] << 2) | ((p[1] >> 4) & 0x3);
	*q++ = (p[1] << 4) | ((p[2] >> 2) & 0xf);
	*q = (p[2] << 6) | (p[3] & 0x3f);

	q = b3;

	while (--i >= 0)
		putchar(*q++);
}
