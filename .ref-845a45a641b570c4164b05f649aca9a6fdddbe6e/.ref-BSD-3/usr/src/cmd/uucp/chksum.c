main()
{
	int i, j;
	char buf[300];

	for (i = 0; i < 300; i++)
		buf[i] = i;
	printf("%o\n", chksum(buf, 300));
}

chksum(s,n)
register char *s;
register n;
{
	register short sum;
	register unsigned t;
	register x;

	sum = -1;
	x = 0;

	do {
		if (sum<0) {
			sum <<= 1;
			sum++;
		} else
			sum <<= 1;
		t = sum;
		sum += (unsigned)*s++;
		x += sum^n;
		if ((unsigned)sum <= t) {
			sum ^= x;
		}
	} while (--n > 0);

	return(sum);
}
