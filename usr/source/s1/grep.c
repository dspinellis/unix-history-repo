#
/*
 * grep -- print lines matching (or not matching) a pattern
 *
 */

#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11

#define	STAR	01

#define	LBSIZE	256
#define	ESIZE	256

char	ibuf[512];
char	expbuf[ESIZE];
int	lnum[2];
char	linebuf[LBSIZE+1];
int	bflag;
int	nflag;
int	cflag;
int	vflag;
int	nfile;
int	circf;
int	blkno;
int	tln[2];

main(argc, argv)
char **argv;
{
	extern fout;

	fout = dup(1);
	flush();
	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'v':
			vflag++;
			continue;

		case 'b':
			bflag++;
			continue;

		case 'c':
			cflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		default:
			printf2("Unknown flag\n");
			continue;
		}
	if (argc<=0)
		exit(2);
	compile(*argv);
	nfile = --argc;
	if (argc<=0)
		execute(0);
	else while (--argc >= 0) {
		argv++;
		execute(*argv);
	}
	flush();
	exit(0);
}

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
			return;

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if (lastep==0)
				goto defchar;
			*lastep =| STAR;
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
	printf2("RE error\n");
}

execute(file)
{
	register char *p1, *p2;
	register c;
	int f;
	char *ebp, *cbp;

	if (file) {
		if ((f = open(file, 0)) < 0) {
			printf2("Can't open %s\n", file);
		}
	} else
		f = 0;
	ebp = ibuf;
	cbp = ibuf;
	lnum[0] = 0;
	lnum[1] = 0;
	tln[0] = 0;
	tln[1] = 0;
	blkno = -1;
	for (;;) {
		if ((++lnum[1])==0)
			lnum[1]++;
		if((lnum[1]&0377) == 0)
			flush();
		p1 = linebuf;
		p2 = cbp;
		for (;;) {
			if (p2 >= ebp) {
				if ((c = read(f, ibuf, 512)) <= 0) {
					close(f);
					if (cflag) {
						if (nfile > 1)
							printf("%s:", file);
						p1 = locv(tln[0],tln[1]);
						printf("%s\n", p1);
					}
					return;
				}
				blkno++;
				p2 = ibuf;
				ebp = ibuf+c;
			}
			if ((c = *p2++) == '\n')
				break;
			if(c)
			if (p1 < &linebuf[LBSIZE-1])
				*p1++ = c;
		}
		*p1++ = 0;
		cbp = p2;
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

advance(alp, aep)
{
	register char *lp, *ep, *curlp;
	char *nextep;

	lp = alp;
	ep = aep;
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
		if (cclass(ep, *lp++, 1)) {
			ep =+ *ep;
			continue;
		}
		return(0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep =+ *ep;
			continue;
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
	case NCCL|STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1]==(CCL|STAR)));
		ep =+ *ep;
		goto star;

	star:
		do {
			lp--;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		return(0);

	default:
		printf2("RE botch\n");
	}
}

cclass(aset, ac, af)
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

printf2(s, a)
{
	extern fout;
	flush();
	fout = 2;
	printf(s, a);
	flush();
	exit(2);
}

succeed(f)
{
	if (cflag) {
		if (++tln[1]==0)
			tln[0]++;
		return;
	}
	if (nfile > 1)
		printf("%s:", f);
	if (bflag)
		printf("%l:", blkno);
	if (nflag)
		printf("%s:", locv(lnum[0], lnum[1]));
	printf("%s\n", linebuf);
}
