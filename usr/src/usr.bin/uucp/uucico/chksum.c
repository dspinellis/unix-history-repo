#ifndef lint
static char sccsid[] = "@(#)chksum.c	4.1 (Berkeley) %G%";
#endif

long
chksum (s, n)
register char *s;
register n;
{
	register long sum, x, t;

	sum = 0xffff;
	x = 0;
	do {
		/* Rotate left, copying bit 15 to bit 0 */
		sum <<= 1;
		if (sum & 0x10000)
			sum ^= 0x10001;
		t = sum;
		sum = (sum + (*s++ & 0377)) & 0xffff;
		x += sum ^ n;
		if (sum <= t)
			sum = (sum ^ x) & 0xffff;
	} while (--n > 0);

	return (long) (short) sum;
}
