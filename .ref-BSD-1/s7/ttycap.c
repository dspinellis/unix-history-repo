/*
 * ttycap - routines for dealing with the teletype capability data base
 * Bill Joy UCB September 25, 1977
 */

char	ttycap[]	"/etc/ttycap";

extern	char	*tbuf;

tgetent(bp, name)
	char *bp, *name;
{
	register char *cp;
	register int cap, c;
	int ibuf[259];

	tbuf = bp;
	cap = fopen(ttycap, ibuf);
	if (cap < 0)
		return (-1);
	for (;;) {
		for (cp = bp, c = getc(ibuf); c != -1; c = getc(ibuf)) {
			if (c == '\n') {
				if (cp > bp && cp[-1] == '\\'){
					cp--;
					continue;
				}
				break;
			}
			*cp++ = c;
		}
		*cp = 0;
		if (c == -1) {
			close(ibuf[0]);
			return (0);
		}
		if (tnamatch(name)) {
			close(ibuf[0]);
			return (1);
		}
	}
}

tnamatch(np)
	char *np;
{
	register char *Np, *Bp;

	for (Bp = tbuf; *Bp && *Bp != ':'; Bp++) {
		for (Np = np; *Np && *Bp != '|' && *Bp != ':' && *Bp == *Np; Bp++, Np++)
			continue;
		if (*Np == 0 && (*Bp == '|' || *Bp == ':' || *Bp == 0))
			return (1);
		while (*Bp && *Bp != ':' && *Bp != '|')
			Bp++;
		if (*Bp == 0)
			break;
	}
	return (0);
}

tskip(bp, i)
	register char *bp;
	register int i;
{

	for (; i > 0; i--) {
		while (*bp && *bp != ':')
			bp++;
		if (*bp == ':')
			bp++;
	}
	return (bp);
}

tgetnum(id)
	char *id;
{
	register int i, base;
	register char *bp;

	for (bp = tskip(tbuf, 3); *bp != 0; bp = tskip(bp, 1)) {
		if (*bp++ != id[0] || *bp == 0 || *bp++ != id[1])
			continue;
		if (*bp != '#')
			continue;
		bp++;
		base = 10;
		if (*bp == '0')
			base = 8;
		i = 0;
		while (*bp >= '0' && *bp <= '9')
			i =* base, i =+ *bp++ - '0';
		return (i);
	}
	return (-1);
}

tgetflag(id)
	char *id;
{
	register char *bp;

	for (bp = tskip(tbuf, 3); *bp; bp = tskip(bp, 1))
		if (*bp++ == id[0] && *bp != 0 && *bp == id[1])
			return (1);
	return (0);
}

tgetstr(id, area)
	char *id, **area;
{
	register char *cp;
	register int i;
	register char *bp;

	for (bp = tskip(tbuf, 3); *bp != 0; bp = tskip(bp, 1)) {
		if (*bp++ != id[0] || *bp == 0 || *bp++ != id[1])
			continue;
		if (*bp != '=')
			continue;
		bp++;
		return (tdecode(bp, area));
	}
	return (0);
}

tdecode(str, area)
	register char *str;
	char **area;
{
	register char *cp;
	register int c;
	int i;

	cp = *area;
	while ((c = *str++) && c != ':') {
		switch (c) {
			case '^':
				if (*str >= 'a' && *str <= 'z')
					c = *str++ & 037;
				break;
			case '\\':
				switch (*str) {
					case 'E':
						str++;
						c = 033;
						break;
					case '^':
					case '\\':
					case ':':
						c = *str++;
						break;
					case 'n':
						c = '\n';
						str++;
						break;
					case 'r':
						c = '\r';
						str++;
						break;
					case 't':
						c = '\t';
						str++;
						break;
					case 'b':
						c = '\b';
						str++;
						break;
					default:
						if (*str >= '0' && *str <= '9') {
							c = 0, i = 3;
							do
								c =<< 3, c=| *str++ - '0';
							while (--i && *str >= '0' && *str <= '9');
						} else if (*str)
							c = *str++;
						break;
				}
		}
		*cp++ = c;
	}
	*cp++ = 0;
	str = *area;
	*area = cp;
	return (str);
}
