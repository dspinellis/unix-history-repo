#define	L	512
#define	N	7
#define	C	20
#define	MEM	(16*2048)

int	ibuf[259];
int	obuf[259];
char	*file;
char	*filep;
int	nfiles;
int	nlines;
int	ntext;
int	*lspace;
char	*tspace;
int	aflg;
int	dflg;
int	mflg;
int	nflg;
int	rflg 1;
int	sfield	-1;
int	schar;
int	cmp();
char	map[256];
int	term();
char	*outfil;
int 	eargc;
char	**eargv;

main(argc, argv)
char **argv;
{
	extern char end;
	register a;
	char *arg;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if(arg[-1] == '-')
					eargv[eargc++] = "-";
				break;

			case 'a':
				aflg++;
				continue;

			case 'd':
				dflg++;
				continue;

			case 'm':
				mflg++;
				continue;

			case 'n':
				nflg++;
				continue;

			case 'o':
				if(--argc > 0)
					outfil = *++argv;
				continue;

			case 'r':
				rflg = -1;
				continue;

			default:
				sfield = number(++*argv);
				break;
			}
			break;
		} else if (**argv == '+')
			schar = number(++*argv);
		else
			eargv[eargc++] = *argv;
	}
	if(eargc == 0)
		eargv[eargc++] = "-";
	for(a=0; a<128; a++)
		(map+128)[a] = a;
	if(!aflg)
		for(a='a'; a<='z'; a++)
			(map+128)[a] = (map+128)[a-'a'+'A'];
	(map+128)['\n'] = 0;
	if(rflg < 0)
		for(a=0; a<128; a++)
			(map+128)[a] = 127-(map+128)[a];
	if(dflg) {
		for(a=0; a<'0'; a++)
			if(a!='\n' && a!=' ' && a!='\t')
				(map+128)[a] = -1;
		ignore('9'+1,'A'-1);
		ignore('Z'+1,'a'-1);
		ignore('z'+1,127);
	}
	ignore(-128,0);
	(map+128)[127] = -1;

	a = MEM;
	while(brk(a) == -1);
		a =- 512;
	brk(a =- 512);	/* for recursion */
	lspace = &end;
	a =- &end;
	nlines = ((a-L)>>1) & 077777;
	nlines =/ 5;
	ntext = nlines*8;
	tspace = lspace+nlines;
	file = "/usr/tmp/stmXaa";
loop:
	filep = file;
	while(*filep != 'X')
		filep++;
	for(*filep = 'a';;(*filep)++) {
		if(stat(file, lspace) < 0) {
			a = creat(file, 0600);
			if(a >= 0)
				break;
		}
		if(*filep == 'z') {
			if(file[1] != 't') {
				file = "/tmp/stmXaa";
				goto loop;
			}
			mess("Cannot locate temp\n");
			exit();
		}
	}
	close(a);
	filep++;
	if ((signal(2, 1) & 01) == 0)
		signal(2, term);
	nfiles = eargc;
	if(!mflg) {
		ibuf[0] = -1;
		sort();
		close(0);
	}
	for(a = mflg?0:eargc; a+N < nfiles; a=+N) {
		newfile();
		merge(a, a+N);
	}
	if(a != nfiles) {
		oldfile();
		merge(a, nfiles);
	}
	term();
}

sort()
{
	register char *cp;
	register *lp, c;
	int done;
	int i;
	int f;

	done = 0;
	i = 0;
	do {
		cp = tspace;
		lp = lspace;
		while(lp < lspace+nlines && cp < tspace+ntext) {
			*lp++ = cp;
			while((*cp++ = c = getc(ibuf)) != '\n') {
				if(c >= 0) continue;
				cp--;
					close(ibuf[0]);
				if(i < eargc) {
					if((f = setfil(i++)) == 0)
						ibuf[0] = 0;
					else if(fopen(f, ibuf) < 0)
						cant(f);
				} else
					break;
			}
			if(c < 0) {
				done++;
				lp--;
				break;
			}
		}
		qsort(lspace, lp-lspace, 2, cmp);
		if(done == 0 || nfiles != 0)
			newfile(); else
			oldfile();
		while(lp > lspace) {
			cp = *--lp;
			do
				putc(*cp, obuf);
			while(*cp++ != '\n');
		}
		fflush(obuf);
		close(obuf[0]);
	} while(done == 0);
}

struct merg
{
	char	l[L];
	int	b[259];
};

merge(a, b)
{
	register struct merg *p;
	register char *cp;
	register i;
	struct { int *ip;};
	int f;
	int j;

	p = lspace;
	j = 0;
	for(i=a; i<b; i++) {
		f = setfil(i);
		if(f == 0)
			p->b[0] = dup(0);
		else if(fopen(f, p->b) < 0)
			cant(f);
		ibuf[j] = p;
		if(!rline(p)) j++;
		p++;
	}
	i = j;
	qsort(ibuf, i, 2, cmp);
	if(i > 0) for(;;) {
		cp = ibuf[i-1];
		do
			putc(*cp, obuf);
		while(*cp++ != '\n');
		if(rline(ibuf[i-1])) {
			i--;
			if(i == 0)
				break;
		}
		cp = &ibuf[i];
		while (--cp.ip > ibuf && cmp(cp.ip, cp.ip-1) < 0) {
			p = *cp.ip;
			*cp.ip = *(cp.ip-1);
			*(cp.ip-1) = p;
		}
	}
	p = lspace;
	for(i=a; i<b; i++) {
		close(p->b[0]);
		p++;
		if(i >= eargc)
			close(creat(setfil(i)));
	}
	fflush(obuf);
	close(obuf[0]);
}

rline(mp)
struct merg *mp;
{
	register char *cp;
	register *bp, c;

	bp = mp->b;
	cp = mp->l;
	do {
		c = getc(bp);
		if(c < 0)
			return(1);
		*cp++ = c;
	} while(c != '\n');
	return(0);
}

newfile()
{

	if(fcreat(setfil(nfiles), obuf) < 0) {
		mess("Can't create temp\n");
		term();
	}
	nfiles++;
}

char *
setfil(i)
{

	if(i < eargc)
		if(eargv[i][0] == '-' && eargv[i][1] == '\0')
			return(0);
		else
			return(eargv[i]);
	i =- eargc;
	filep[0] = i/26 + 'a';
	filep[1] = i%26 + 'a';
	return(file);
}

oldfile()
{

	if(outfil) {
		if(fcreat(outfil, obuf) < 0) {
			mess("Can't create output\n");
			term();
		}
	} else
		obuf[0] = 1;
}

cant(f)
{
	mess("Can't open ");
	mess(f);
	mess("\n");
	term();
}

term()
{
	register i;

	if(nfiles == 0)
		nfiles++;
	for(i=eargc; i<nfiles; i++) {
		unlink(setfil(i));
	}
	exit();
}

cmp(i, j)
int *i, *j;
{
	register a, b;
	register char *pa;
	char *pb, *ipa, *ipb;
	int sa, sb;
	int jpa, jpb;
	int sign;

	pa = *i;
	pb = *j;
	if(sfield >= 0) {
		pa = skip(pa);
		pb = skip(pb);
	}
	if(schar) {
		pa = slip(pa);
		pb = slip(pb);
	}
	if(nflg) {
		pa = snip(pa, &sa);
		pb = snip(pb, &sb);
		sign = sa*(sa==sb);
		for(ipa = pa; digit(*ipa); ipa++);
		for(ipb = pb; digit(*ipb); ipb++);
		jpa = ipa;
		jpb = ipb;
		a = 0;
		if(sign) while(ipa > pa && ipb > pb)
			if(b = *--ipb - *--ipa)
				a = b;
		while(ipa > pa)
			if(*--ipa != '0')
				return(sign ? -sign : -sa);
		while(ipb > pb)
			if(*--ipb != '0')
				return(sign ? sign : sb);
		if(a) return(a*sign);
		if(*(pa=jpa) == '.')
			pa++;
		if(*(pb=jpb) == '.')
			pb++;
		while(digit(*pa) && digit(*pb)) 
			if(a = *pb++ - *pa++)
				return(sign ? a*sign : sb);
		while(digit(*pa))
			if(*pa++ != '0')
				return(sign ? -sign : -sa);
		while(digit(*pb))
			if(*pb++ != '0')
				return(sign ? sign : sb);
	}
loop : 
	while((a = (map+128)[*pa]) < 0)
		pa++;
	while((b = (map+128)[*pb]) < 0)
		pb++;
	if(a == b) {
		if(*pa++ != '\n') {
			pb++;
			goto loop;
		}
		pa = *i;
		pb = *j;
		while(*pa == *pb) {
			if(*pa++ == '\n')
				return(0);
			pb++;
		}
		return((*pb - *pa) * rflg);
	}
	return(b - a);
}

skip(pp)
char *pp;
{
	register i;
	register char *p;

	p = pp;
	if(i = sfield)
	do {
		while(*p == ' ' || *p == '\t')
			p++;
		while(*p != ' ' && *p != '\t' && *p != '\n')
			p++;
	} while(--i);
	while(*p == ' ' || *p == '\t')
		p++;
	return(p);
}

slip(pp)
char *pp;
{
	register i;
	register char *p;

	p = pp;
	i = schar;
	do {
		if(*p != '\n')
			p++;
	} while(--i);
	return(p);
}

snip(pp,ls)
char *pp;
int *ls;
{
	register char *p;
	register int s;

	p = pp;
	while(*p == ' ' || *p == '\t')
		p++;
	s = rflg;
	if(*p == '-') {
		s = -s;
		p++;
	}
	*ls = s;
	return(p);
}

number(ap)
char *ap;
{
	register int n;
	register char *p;

	p = ap;
	n = 0;
	while(digit(*p))
		n = n*10 + *p++ - '0';
	return(n);
}

digit(c)
{

	if(c <= '9' && c >= '0')
		return(1);
	return(0);
}

mess(s)
char *s;
{
	while(*s)
		write(2, s++, 1);
}

ignore(a,b)
{
	register c;
	for(c = a; c <= b; c++)
		(map+128)[c] = -1;
}
