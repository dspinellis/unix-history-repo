#
/*
 * gres -- substitute in lines matching (or not) a pattern
 *
 *
 * Owner: lem
 */

#define CBRA	1
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11
#define CKET	12

#define	STAR	01

#define PTRSIZE	100
#define RESIZE	5000
#define	LBSIZE	256
#define	ESIZE	256
#define NBRA	5

char	ibuf[512];
char	genbuf[LBSIZE];
char	*loc1;
char	*loc2;
char	*locs;
int	lnum;
char	linebuf[LBSIZE+1];
int	gflag;
int	vflag;
char	*braelist[NBRA];
char	*braslist[NBRA];

struct	reptr {
	char	*re1;
	char	*re2;
	char	*rhs;
	char	gcirc;
	char	scirc;
}	ptrspace[PTRSIZE];
struct reptr	*rep;
struct reptr	*ptrend;

char	*bufend;
char	*reend;

char	respace[RESIZE];

main(argc, argv)
char **argv;
{
	extern fout, fin;
	register char	*p1, *p2;
	char	*cp;
	int p;

	fin = dup(0);
	fout = dup(1);
	flush();
	rep = ptrspace;
	rep->re1 = respace;
	bufend = &linebuf[LBSIZE];
	ptrend = &rep[PTRSIZE];
	reend = &respace[RESIZE];
	lnum = 0;

	while (--argc > 0 && (++argv)[0][0]=='-')
		switch (argv[0][1]) {

		case 'v':
			vflag++;
			continue;

		case 'g':
			gflag++;
			continue;

		case 'f':
			fcomp(argc, argv);
			argc--;
			argv++;
			continue;

		case 'e':
			ecomp(argc,argv);
			argc =- 3;
			argv =+ 3;
			continue;

		default:
			printf2("Unknown flag: %c\n", argv[0][1]);
			continue;
		}


	if(rep == ptrspace) {
		argv--;
		argc++;
		ecomp(argc, argv);
		argc =- 4;
		argv =+ 4;
	}

/*	abort();	/*DEBUG*/

	if(argc <= 0)
		execute(0);
	else while(--argc >= 0) {
		execute(*argv++);
	}
	flush();
	exit(0);
}

fcomp(argc, argv)
char	*argv[];
{
	register int p;
	register char	*cp;
	extern	fin;

	if(argc-- <= 0)	exit(2);

	if(fin)	close(fin);
	if((fin = open(*++argv, 0)) < 0) {
		printf2("Cannot open pattern file: %s\n",
			*argv);
		exit(1);
	}

	for(;;) {
		lnum++;
		cp = linebuf - 1;
		while(*++cp = getchar()) {
			if(cp >= bufend) {
				printf2("RE too long, line %d\n", lnum);
			}
			if(*cp == '\n')	break;
		}

		if(*cp == 0)	break;

		p = compile(linebuf, rep->re1, &rep->gcirc);
		if(p < 1)
			printf2("RE error, line %d\n", lnum);
		if(p == 1 && rep->gcirc == 0) {
			if(rep > ptrspace) {
				rep->re2 = rep->re1;
				rep->re1 = rep[-1].re2;
				rep->gcirc = rep[-1].scirc;
			} else
				printf2("RE error, line %d\n",
					lnum);
		} else
			if((rep->re2 = rep->re1 + p) > reend)
			printf2("RE space exhausted; line %d\n", lnum);

		lnum++;
		cp = linebuf - 1;
		while(*++cp = getchar()) {
			if(cp >= bufend)
				printf("RE too long, line %d\n", lnum);
			if(*cp == '\n')	break;
		}
		if(*cp == 0)	break;

		p = compile(linebuf, rep->re2, &rep->scirc);
		if(p < 1)
			printf2("RE error, line %d\n", lnum);
		if(p == 1 && rep->scirc == 0) {
			rep->rhs = rep->re2;
			rep->re2 = rep->re1;
			rep->scirc = rep->gcirc;
		} else

			if((rep->rhs = rep->re2 + p) > reend)
			printf2("RE space exhausted, line %d\n", lnum);

		lnum++;
		cp = linebuf - 1;
		while(*++cp = getchar()) {

			if(cp >= bufend)
				printf2("RE too long, line %d\n", lnum);
			if(*cp == '\n')	break;
		}
		if(*cp == 0)	break;

		if((p = compsub(linebuf, rep->rhs)) < 1)
			printf2("RHS error, line %d\n", lnum);


		if(++rep >= ptrend)
			printf2("Too many lines in pattern file, %d\n",
				lnum);

		if ((rep->re1 = rep[-1].rhs + p) > reend)
			printf2("RE space exhausted, line %d\n", lnum);
	}
}


ecomp(argc, argv)
char	*argv[];
{

	register int	p;

	if(argc-- <= 0)
		exit(2);
	p = compile(*++argv, rep->re1, &rep->gcirc);
	if(p < 1)
		printf2("RE error, re%d\n", 1);

	if(p == 1 && rep->gcirc == 0) {
		if(rep > ptrspace) {
			rep->re2 = rep->re1;
			rep->re1 = rep[-1].re2;
			rep->gcirc = rep[-1].scirc;
		} else
			printf2("RE error: re1\n");
	}

	rep->re2 = rep->re1 + p;

	if(argc-- <= 0)
		exit(2);

	if((p = compile(*++argv, rep->re2, &rep->scirc)) < 1)
		printf2("RE error, re%d\n", 2);
	if(p == 1 && rep->scirc == 0) {
		rep->rhs = rep->re2;
		rep->re2 = rep->re1;
		rep->scirc = rep->gcirc;
	} else
		rep->rhs = rep->re2 + p;

	if((p = compsub(*++argv, rep->rhs)) < 1)
		printf2("RHS error\n");


	if(argc-- <= 0)
		exit(2);

	if(++rep >= ptrend)
		printf2("Too many patterns\n");

	if((rep->re1 = rep[-1].rhs + p) > reend)
		printf2("RE space exhausted\n");

}

compile(astr, expbuf, circf)
char *astr;
char	*expbuf;
char	*circf;
{
	register c;
	register char *ep, *sp;
	char *lastep;
	int cclcnt;
	char	bracket[NBRA], *bracketp;
	int	nbra;

	ep = expbuf;
	bracketp = bracket;
	nbra = 0;
	sp = astr;
	if (*sp == '^') {
		(*circf)++;
		sp++;
	}
	for (;;) {
		if (ep >= &expbuf[ESIZE])
			return(-1);
		if ((c = *sp++) != '*')
			lastep = ep;
		switch (c) {

		case '\\':
			if((c = *sp++) == '(') {
				if(nbra >= NBRA)
					return(-1);
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;
			}
			if(c == ')') {
				if(bracketp <= bracket)
					return(-1);
				*ep++ = CKET;
				*ep++ = *--bracketp;
				continue;
			}
			if(c == '\n')	return(-1);
			goto defchar;

		case '\0':
		case '\n':
			if(ep >= expbuf[ESIZE] - 1)	return(-1);
			*ep++ = CEOF;
			return(ep - expbuf);

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if (lastep==0)
				goto defchar;
			*lastep =| STAR;
			continue;

		case '$':
			if (*sp != '\0' && *sp != '\n')
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
					return(-1);
			} while ((c = *sp++) != ']');
			lastep[1] = cclcnt;
			continue;

		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}
execute(file)
{
	register char *p1, *p2;
	register c;
	int count;
	int f;
	char *ebp, *cbp;

	rep->re1 = 0;

	if (file) {
		if ((f = open(file, 0)) < 0) {
			printf2("Can't open %s\n", file);
		}
	} else
		f = 0;
	ebp = ibuf;
	cbp = ibuf;
	for (;;) {
		p1 = linebuf;
		p2 = cbp;
		for (;;) {
			if (p2 >= ebp) {
				if ((count = c = read(f, ibuf, 512)) <= 0) {
					close(f);
					return;
				}
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
		for(rep = ptrspace; rep->re1; rep++) {
			p1 = linebuf;
			p2 = rep->re1;
			if (rep->gcirc) {
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
			continue;
		}
	printf("%s\n", linebuf);
	if (count != 512) flush();
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
		loc2 = lp;
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

	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

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
			if(lp == locs)	break;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		return(0);

	default:
		printf2("RE botch\n");
	}
}
succeed()
{
	if(sexec(0) == 0)	return;

	dosub(rep->rhs);

	if(gflag) {
		while(*loc2) {
			if(sexec(1) == 0)	break;
			dosub(rep->rhs);
		}
	}
}
sexec(gf)
{
	register char	*p1, *p2, c;

	if(gf) {
		if(rep->scirc)	return(0);
		p1 = linebuf;
		p2 = genbuf;
		while(*p1++ = *p2++);
		locs = p1 = loc2;
	} else {
		p1 = linebuf;
		locs = 0;
	}

	p2 = rep->re2;
	if(rep->scirc) {
		loc1 = p1;
		return(advance(p1, p2));
	}

	/* fast check for first character */

	if(*p2 == CCHR) {
		c = p2[1];
		do {
			if(*p1 != c)
				continue;
			if(advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while(*p1++);
	}

	do {
		if(advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while(*p1++);
	return(0);
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
compsub(arg, rhsbuf)
char	*arg, *rhsbuf;
{
	register char	*rhsend;
	register char	*p, *q;

	p = rhsbuf;
	q = arg;
	rhsend = &rhsbuf[LBSIZE];
	for(;;) {
		if((*p = *q++) == '\\')
			*p = *q++ | 0200;
		if(*p == '\n') {
			*p++ = '\0';
			return(p - rhsbuf);
		}
		if(*p++ == '\0')
			return(p - rhsbuf);

		if(p >= rhsend) {
			return(-1);
		}
	}
}

dosub(rhsbuf)
char	*rhsbuf;
{
	register char *lp, *sp, *rp;
	int c;

	lp = linebuf;
	sp = genbuf;
	rp = rhsbuf;
	while (lp < loc1)
		*sp++ = *lp++;
	while(c = *rp++) {
		if (c=='&') {
			sp = place(sp, loc1, loc2);
			continue;
		} else if (c<0 && (c =& 0177) >='1' && c < NBRA+'1') {
			sp = place(sp, braslist[c-'1'], braelist[c-'1']);
			continue;
		}
		*sp++ = c&0177;
		if (sp >= &genbuf[LBSIZE])
			printf2("output line too long.\n");
	}
	lp = loc2;
	loc2 = sp + linebuf - genbuf;
	while (*sp++ = *lp++)
		if (sp >= &genbuf[LBSIZE]) {
			printf2("Output line too long.\n");
		}
	lp = linebuf;
	sp = genbuf;
	while (*lp++ = *sp++);
}
place(asp, al1, al2)
{
	register char *sp, *l1, *l2;

	sp = asp;
	l1 = al1;
	l2 = al2;
	while (l1 < l2) {
		*sp++ = *l1++;
		if (sp >= &genbuf[LBSIZE])
			printf2("Output line too long.\n");
	}
	return(sp);
}
