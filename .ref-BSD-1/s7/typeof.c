/*
 * typeof - return a 2 character teletype type code from /etc/ttywhere
 * Bill Joy UCB September 24, 1977
 */
char	ttytype[]	"/etc/ttytype";

typeof(tty)
	char tty;
{
	register int c, c2;
	int ibuf[259];

	if (fopen(ttytype, ibuf) < 0)
		goto unknown;
	for (;;) {
		c = getc(ibuf);
		if (c == '\n' || c == -1) {
unknown:
			close(ibuf[0]);
			return('uk');
		}
		if (c == tty) {
			c = getc(ibuf);
			if (c == '\n' || c == -1)
				goto unknown;
			c2 = getc(ibuf);
			if (c2 == '\n' || c2 == -1)
				goto unknown;
			close(ibuf[0]);
			return (c2 << 8 | c);
		}
		do
			c = getc(ibuf);
		while (c != '\n' && c != -1);
	}
}

stypeof(tty)
	int tty;
{
	static char stype[3];

	tty = typeof(tty);
	stype[0] = tty& 0377;
	stype[1] = (tty >> 8) & 0377;
	return (stype);
}
