#include <stdio.h>
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11

#define	STAR	01

#define LBSIZE  1024
#define	ESIZE	256

char    ibuf[BUFSIZ];
char	expbuf[ESIZE];
char	linebuf[LBSIZE+1];
int	circf;

char cc[] = {
	0000,0001,0002,0003,0004,0005,0006,0007,
	0010,0011,0012,0013,0014,0015,0016,0017,
	0020,0021,0022,0023,0024,0025,0026,0027,
	0030,0031,0032,0033,0034,0035,0036,0037,
	0040,0041,0042,0043,0044,0045,0046,0047,
	0050,0051,0052,0053,0054,0055,0056,0057,
	0060,0061,0062,0063,0064,0065,0066,0067,
	0070,0071,0072,0073,0074,0075,0076,0077,
	0100,0141,0142,0143,0144,0145,0146,0147,
	0150,0151,0152,0153,0154,0155,0156,0157,
	0160,0161,0162,0163,0164,0165,0166,0167,
	0170,0171,0172,0133,0134,0135,0136,0137,
	0140,0141,0142,0143,0144,0145,0146,0147,
	0150,0151,0152,0153,0154,0155,0156,0157,
	0160,0161,0162,0163,0164,0165,0166,0167,
	0170,0171,0172,0173,0174,0175,0176,0177,
};


compile(astr)
char *astr;
{
	register c;
	register char *ep, *sp;
	char *lastep;
	int cclcnt;

	ep = expbuf;
	sp = astr;
	if (*sp == '^') {
		circf++;
		sp++;
	}
	for (;;) {
		if (ep >= &expbuf[ESIZE])
			goto cerror;
		if ((c = *sp++) != '*')
			lastep = ep;
		switch (c) {

		case '\0':
			*ep++ = CEOF;
			return(1);

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if (lastep==0)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if (*sp != '\0')
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			*ep++ = CCL;
			*ep++ = 0;
			cclcnt = 1;
			if ((c = *sp++) == '^') {
				c = *sp++;
				ep[-2] = NCCL;
			}
			do {
				*ep++ = c;
				cclcnt++;
				if (c=='\0' || ep >= &expbuf[ESIZE])
					goto cerror;
			} while ((c = *sp++) != ']');
			lastep[1] = cclcnt;
			continue;

		case '\\':
			if ((c = *sp++) == '\0')
				goto cerror;
		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
    cerror:
	return(0);
}


execute(file)
char *file;
{
	register char *p1, *p2;
	register c;
	int f, body, lf;
	char *ebp, *cbp;

	if ((f = open(file, 0)) < 0) {
		fprintf(stderr, "Grep: Can't open %s\n", file);
		return(0);
	}
	body = 0;
	ebp = ibuf;
	cbp = ibuf;
	for (;;) {
		p1 = linebuf;
		p2 = cbp;
		lf = 0;
		for (;;) {
			if (p2 >= ebp) {
				if ((c = read(f, ibuf, sizeof ibuf)) <= 0) {
					close(f);
					if(lf) break; /* bodyless comp! */
					return(0);
				}
				p2 = ibuf;
				ebp = ibuf+c;
			}
			c = *p2++;
			if(lf) if(c != ' ' && c != '\t') {
				--p2;
				break;
			} else
				lf = 0;
			if (c == '\n')
				if(body)
					break;
				else {
					if(lf) {
						body++;
						break;
					}
					lf++;
					c = ' ';
				}
			if(c && p1 < &linebuf[LBSIZE-1])
				*p1++ = c;
		}
		*p1++ = 0;
		cbp = p2;
		p1 = linebuf;
		p2 = expbuf;
		if (circf) {
			if (advance(p1, p2))
				goto found;
			continue;
		}
		/* fast check for first character */
		if (*p2==CCHR) {
			c = p2[1];
			do {
				if(*p1==c || cc[*p1]==c)
					if (advance(p1, p2))
						goto found;
			} while (*p1++);
			continue;
		}
		/* regular algorithm */
		do {
			if (advance(p1, p2))
				goto found;
		} while (*p1++);
		continue;
	found:
		close(f);
		return(1);
	}
}


advance(alp, aep)
char *alp, *aep;
{
	register char *lp, *ep, *curlp;
	char *nextep;

	lp = alp;
	ep = aep;
	for (;;) switch (*ep++) {

	case CCHR:
		if (*ep++ == *lp++ || ep[-1] == cc[lp[-1]])
			continue;
		return(0);

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp==0)
			continue;
		return(0);

	case CEOF:
		return(1);

	case CCL:
		if (cclass(ep, *lp++, 1)) {
			ep += *ep;
			continue;
		}
		return(0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep += *ep;
			continue;
		}
		return(0);

	case CDOT|STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (*lp++ == *ep || cc[lp[-1]] == *ep) ;
		ep++;
		goto star;

	case CCL|STAR:
	case NCCL|STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1]==(CCL|STAR)));
		ep += *ep;
		goto star;

	star:
		do {
			lp--;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		return(0);

	default:
		printf("RE botch\n");
		return(0);
	}
}


cclass(aset, ac, af)
char *aset;
{
	register char *set, c;
	register n;

	set = aset;
	if ((c = ac) == 0)
		return(0);
	n = *set++;
	while (--n)
		if (*set++ == c)
			return(af);
	return(!af);
}
