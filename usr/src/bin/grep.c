/*	grep.c	4.1	82/05/07	*/

/*
 * grep -- print lines matching (or not matching) a pattern
 *
 *	status returns:
 *		0 - ok, and some matches
 *		1 - ok, but no matches
 *		2 - some error
 */

#include <stdio.h>
#include <ctype.h>

#define	CBRA	1
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11
#define	CKET	12
#define	CBACK	18

#define	STAR	01

#define	LBSIZE	BUFSIZ
#define	ESIZE	256
#define	NBRA	9

char	expbuf[ESIZE];
long	lnum;
char	linebuf[LBSIZE+1];
char	ybuf[ESIZE];
int	bflag;
int	lflag;
int	nflag;
int	cflag;
int	vflag;
int	nfile;
int	hflag	= 1;
int	sflag;
int	yflag;
int	circf;
int	blkno;
long	tln;
int	nsucc;
char	*braslist[NBRA];
char	*braelist[NBRA];
char	bittab[] = {
	1,
	2,
	4,
	8,
	16,
	32,
	64,
	128
};

main(argc, argv)
char **argv;
{
	extern char _sobuf[];
	setbuf(stdout, _sobuf);
	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'y':
			yflag++;
			continue;

		case 'h':
			hflag = 0;
			continue;

		case 's':
			sflag++;
			continue;

		case 'v':
			vflag++;
			continue;

		case 'b':
			bflag++;
			continue;

		case 'l':
			lflag++;
			continue;

		case 'c':
			cflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'e':
			--argc;
			++argv;
			goto out;

		default:
			errexit("grep: unknown flag\n", (char *)NULL);
			continue;
		}
out:
	if (argc<=0)
		exit(2);
	if (yflag) {
		register char *p, *s;
		for (s = ybuf, p = *argv; *p; ) {
			if (*p == '\\') {
				*s++ = *p++;
				if (*p)
					*s++ = *p++;
			} else if (*p == '[') {
				while (*p != '\0' && *p != ']')
					*s++ = *p++;
			} else if (islower(*p)) {
				*s++ = '[';
				*s++ = toupper(*p);
				*s++ = *p++;
				*s++ = ']';
			} else
				*s++ = *p++;
			if (s >= ybuf+ESIZE-5)
				errexit("grep: argument too long\n", (char *)NULL);
		}
		*s = '\0';
		*argv = ybuf;
	}
	compile(*argv);
	nfile = --argc;
	if (argc<=0) {
		if (lflag)
			exit(1);
		execute((char *)NULL);
	} else while (--argc >= 0) {
		argv++;
		execute(*argv);
	}
	exit(nsucc == 0);
}

compile(astr)
char *astr;
{
	register c;
	register char *ep, *sp;
	char *cstart;
	char *lastep;
	int cclcnt;
	char bracket[NBRA], *bracketp;
	int closed;
	char numbra;
	char neg;

	ep = expbuf;
	sp = astr;
	lastep = 0;
	bracketp = bracket;
	closed = numbra = 0;
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
			return;

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if (*sp != '\0')
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if(&ep[17] >= &expbuf[ESIZE])
				goto cerror;
			*ep++ = CCL;
			neg = 0;
			if((c = *sp++) == '^') {
				neg = 1;
				c = *sp++;
			}
			cstart = sp;
			do {
				if (c=='\0')
					goto cerror;
				if (c=='-' && sp>cstart && *sp!=']') {
					for (c = sp[-2]; c<*sp; c++)
						ep[c>>3] |= bittab[c&07];
					sp++;
				}
				ep[c>>3] |= bittab[c&07];
			} while((c = *sp++) != ']');
			if(neg) {
				for(cclcnt = 0; cclcnt < 16; cclcnt++)
					ep[cclcnt] ^= -1;
				ep[0] &= 0376;
			}

			ep += 16;

			continue;

		case '\\':
			if((c = *sp++) == '(') {
				if(numbra >= NBRA) {
					goto cerror;
				}
				*bracketp++ = numbra;
				*ep++ = CBRA;
				*ep++ = numbra++;
				continue;
			}
			if(c == ')') {
				if(bracketp <= bracket) {
					goto cerror;
				}
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;
			}

			if(c >= '1' && c <= '9') {
				if((c -= '1') >= closed)
					goto cerror;
				*ep++ = CBACK;
				*ep++ = c;
				continue;
			}

		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
    cerror:
	errexit("grep: RE error\n", (char *)NULL);
}

execute(file)
char *file;
{
	register char *p1, *p2;
	register c;

	if (file) {
		if (freopen(file, "r", stdin) == NULL)
			errexit("grep: can't open %s\n", file);
	}
	lnum = 0;
	tln = 0;
	for (;;) {
		lnum++;
		p1 = linebuf;
		while ((c = getchar()) != '\n') {
			if (c == EOF) {
				if (cflag) {
					if (nfile>1)
						printf("%s:", file);
					printf("%D\n", tln);
					fflush(stdout);
				}
				return;
			}
			*p1++ = c;
			if (p1 >= &linebuf[LBSIZE-1])
				break;
		}
		*p1++ = '\0';
		p1 = linebuf;
		p2 = expbuf;
		if (circf) {
			if (advance(p1, p2))
				goto found;
			goto nfound;
		}
		/* fast check for first character */
		if (*p2==CCHR) {
			c = p2[1];
			do {
				if (*p1!=c)
					continue;
				if (advance(p1, p2))
					goto found;
			} while (*p1++);
			goto nfound;
		}
		/* regular algorithm */
		do {
			if (advance(p1, p2))
				goto found;
		} while (*p1++);
	nfound:
		if (vflag)
			succeed(file);
		continue;
	found:
		if (vflag==0)
			succeed(file);
	}
}

advance(lp, ep)
register char *lp, *ep;
{
	register char *curlp;
	char c;
	char *bbeg;
	int ct;

	for (;;) switch (*ep++) {

	case CCHR:
		if (*ep++ == *lp++)
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
		c = *lp++ & 0177;
		if(ep[c>>3] & bittab[c & 07]) {
			ep += 16;
			continue;
		}
		return(0);
	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CBACK:
		bbeg = braslist[*ep];
		if (braelist[*ep]==0)
			return(0);
		ct = braelist[*ep++] - bbeg;
		if(ecmp(bbeg, lp, ct)) {
			lp += ct;
			continue;
		}
		return(0);

	case CBACK|STAR:
		bbeg = braslist[*ep];
		if (braelist[*ep]==0)
			return(0);
		ct = braelist[*ep++] - bbeg;
		curlp = lp;
		while(ecmp(bbeg, lp, ct))
			lp += ct;
		while(lp >= curlp) {
			if(advance(lp, ep))	return(1);
			lp -= ct;
		}
		return(0);


	case CDOT|STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (*lp++ == *ep);
		ep++;
		goto star;

	case CCL|STAR:
		curlp = lp;
		do {
			c = *lp++ & 0177;
		} while(ep[c>>3] & bittab[c & 07]);
		ep += 16;
		goto star;

	star:
		if(--lp == curlp) {
			continue;
		}

		if(*ep == CCHR) {
			c = ep[1];
			do {
				if(*lp != c)
					continue;
				if(advance(lp, ep))
					return(1);
			} while(lp-- > curlp);
			return(0);
		}

		do {
			if (advance(lp, ep))
				return(1);
		} while (lp-- > curlp);
		return(0);

	default:
		errexit("grep RE botch\n", (char *)NULL);
	}
}

succeed(f)
char *f;
{
	nsucc = 1;
	if (sflag)
		return;
	if (cflag) {
		tln++;
		return;
	}
	if (lflag) {
		printf("%s\n", f);
		fflush(stdout);
		fseek(stdin, 0l, 2);
		return;
	}
	if (nfile > 1 && hflag)
		printf("%s:", f);
	if (bflag)
		printf("%u:", blkno);
	if (nflag)
		printf("%ld:", lnum);
	printf("%s\n", linebuf);
	fflush(stdout);
}

ecmp(a, b, count)
char	*a, *b;
{
	register cc = count;
	while(cc--)
		if(*a++ != *b++)	return(0);
	return(1);
}

errexit(s, f)
char *s, *f;
{
	fprintf(stderr, s, f);
	exit(2);
}
