/* C command */

char *tmp0;
char *tmp1;
char *tmp2;
char *tmp3;
char *tmp4;
char *tmp5;
char ts[1000];
char *tsp ts;
char *av[50];
char *clist[50];
char *llist[50];
int instring;
int pflag;
int sflag;
int cflag;
int oflag;
int proflag;
int *ibuf;
int *ibuf1;
int *ibuf2;
int *obuf;
char *lp;
char *line;
int lineno;
int exfail;
struct symtab {
	char name[8];
	char *value;
} *symtab;
int symsiz 200;
struct symtab *defloc;
struct symtab *incloc;
char *stringbuf;
char *pass0 "/lib/c0";
char *pass1 "/lib/c1";
char *pass2 "/lib/c2";
char *pref "/lib/crt0.o";

main(argc, argv)
char *argv[]; {
	char *t;
	int nc, nl, i, j, c, f20, nxo;
	int dexit();

	i = nc = nl = f20 = nxo = 0;
	while(++i < argc) {
		if(*argv[i] == '-')
			switch (argv[i][1]) {
				default:
					goto passa;
				case 'S':
					sflag++;
					cflag++;
					break;
				case 'O':
					oflag++;
					break;
				case 'p':
					proflag++;
					pref = "/lib/mcrt0.o";
					break;
				case 'P':
					pflag++;
				case 'c':
					cflag++;
					break;

				case 'f':
					pref = "/lib/fcrt0.o";
					pass0 = "/lib/fc0";
					break;

				case '2':
					if(argv[i][2] == '\0')
						pref = "/lib/crt2.o";
					else {
						pref = "/lib/crt20.o";
						f20 = 1;
					}
					break;
				case 't':
					if (argv[i][2]=='0')
						pass0 = "/usr/c/c0";
					if (argv[i][2]=='1')
						pass1 = "/usr/c/c1";
					if (argv[i][2]=='2')
						pass2 = "/usr/c/c2";
					break;
			}
		else {
		passa:
			t = argv[i];
			if(getsuf(t)=='c') {
				clist[nc++] = t;
				t = setsuf(t, 'o');
			}
			if (nodup(llist, t)) {
				llist[nl++] = t;
				if (getsuf(t)=='o')
					nxo++;
			}
		}
	}
	if(nc==0)
		goto nocom;
	if (pflag==0) {
		tmp0 = copy("/tmp/ctm0a");
		while((c=open(tmp0, 0))>=0) {
			close(c);
			tmp0[9]++;
		}
		while((creat(tmp0, 0400))<0)
			tmp0[9]++;
	}
	if ((signal(2, 1) & 01) == 0)
		signal(2, &dexit);
	(tmp1 = copy(tmp0))[8] = '1';
	(tmp2 = copy(tmp0))[8] = '2';
	(tmp3 = copy(tmp0))[8] = '3';
	if (oflag)
		(tmp5 = copy(tmp0))[8] = '5';
	if (pflag==0)
		(tmp4 = copy(tmp0))[8] = '4';
	for (i=0; i<nc; i++) {
		if (nc>1)
			printf("%s:\n", clist[i]);
		av[0] = "c0";
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		av[1] = expand(clist[i]);
		if (pflag || exfail)
			continue;
		if (av[1] == 0) {
			cflag++;
			continue;
		}
		av[2] = tmp1;
		av[3] = tmp2;
		if (proflag) {
			av[4] = "-P";
			av[5] = 0;
		} else
			av[4] = 0;
		if (callsys(pass0, av)) {
			cflag++;
			continue;
		}
		av[0] = "c1";
		av[1] = tmp1;
		av[2] = tmp2;
		if (sflag)
			tmp3 = setsuf(clist[i], 's');
		av[3] = tmp3;
		if (oflag)
			av[3] = tmp5;
		av[4] = 0;
		if(callsys(pass1, av)) {
			cflag++;
			continue;
		}
		if (oflag) {
			av[0] = "c2";
			av[1] = tmp5;
			av[2] = tmp3;
			av[3] = 0;
			callsys(pass2, av);
			unlink(tmp5);
		}
		if (sflag)
			continue;
		av[0] = "as";
		av[1] = "-";
		av[2] = tmp3;
		av[3] = 0;
		cunlink(tmp1);
		cunlink(tmp2);
		cunlink(tmp4);
		callsys("/bin/as", av);
		t = setsuf(clist[i], 'o');
		cunlink(t);
		if(link("a.out", t) || cunlink("a.out")) {
			printf("move failed: %s\n", t);
			cflag++;
		}
	}
nocom:
	if (cflag==0 && nl!=0) {
		i = 0;
		av[0] = "ld";
		av[1] = "-x";
		av[2] = pref;
		j = 3;
		while(i<nl)
			av[j++] = llist[i++];
		if(f20)
			av[j++] = "-l2";
		else {
			av[j++] = "-lc";
			av[j++] = "-l";
		}
		av[j++] = 0;
		callsys("/bin/ld", av);
		if (nc==1 && nxo==1)
			cunlink(setsuf(clist[0], 'o'));
	}
	dexit();
}

dexit()
{
	if (!pflag) {
		cunlink(tmp1);
		cunlink(tmp2);
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
		cunlink(tmp5);
		cunlink(tmp0);
	}
	exit();
}

expand(file)
char *file;
{
	int ib1[259], ib2[259], ob[259];
	struct symtab stab[200];
	char ln[196], sbf[1024];
	register int c;
	register char *rlp;

	exfail = 0;
	ibuf = ibuf1 = ib1;
	ibuf2 = ib2;
	if (fopen(file, ibuf1)<0)
		return(file);
	if (getc(ibuf1) != '#') {
		close(ibuf1[0]);
		return(file);
	}
	ibuf1[1]++;
	ibuf1[2]--;
	obuf = ob;
	symtab = stab;
	for (c=0; c<200; c++) {
		stab[c].name[0] = '\0';
		stab[c].value = 0;
	}
	defloc = lookup("define", 1);
	defloc->value = defloc->name;
	incloc = lookup("include", 1);
	incloc->value = incloc->name;
	stringbuf = sbf;
	line  = ln;
	lineno = 0;
	if (fcreat(tmp4, obuf) < 0) {
		printf("Can't creat %s\n", tmp4);
		dexit();
	}
	while(getline()) {
		if (ibuf==ibuf2 && pflag==0)
			putc(001, obuf);	/*SOH: insert */
		if (ln[0] != '#')
			for (rlp = line; c = *rlp++;)
				putc(c, obuf);
		putc('\n', obuf);
	}
	fflush(obuf);
	close(obuf[0]);
	close(ibuf1[0]);
	return(tmp4);
}

getline()
{
	register int c, sc, state;
	struct symtab *np;
	char *namep, *filname;

	if (ibuf==ibuf1)
		lineno++;
	lp = line;
	*lp = '\0';
	state = 0;
	if ((c=getch()) == '#')
		state = 1;
	while (c!='\n' && c!='\0') {
		if ('a'<=c && c<='z' || 'A'<=c && c<='Z' || c=='_') {
			namep = lp;
			sch(c);
			while ('a'<=(c=getch()) && c<='z'
			      ||'A'<=c && c<='Z'
			      ||'0'<=c && c<='9' 
			      ||c=='_')
				sch(c);
			sch('\0');
			lp--;
			np = lookup(namep, state);
			if (state==1) {
				if (np==defloc)
					state = 2;
				else if (np==incloc)
					state = 3;
				else {
					error("Undefined control");
					while (c!='\n' && c!='\0')
						c = getch();
					return(c);
				}
			} else if (state==2) {
				np->value = stringbuf;
				while ((c=getch())!='\n' && c!='\0')
					savch(c);
				savch('\0');
				return(1);
			}
			continue;
		} else if ((sc=c)=='\'' || sc=='"') {
			sch(sc);
			filname = lp;
			instring++;
			while ((c=getch())!=sc && c!='\n' && c!='\0') {
				sch(c);
				if (c=='\\')
					sch(getch());
			}
			instring = 0;
			if (state==3) {
				*lp = '\0';
				while ((c=getch())!='\n' && c!='\0');
				if (ibuf==ibuf2)
					error("Nested 'include'");
				if (fopen(filname, ibuf2)<0)
					error("Missing file %s", filname);
				else
					ibuf = ibuf2;
				return(c);
			}
		}
		sch(c);
		c = getch();
	}
	sch('\0');
	if (state>1)
		error("Control syntax");
	return(c);
}

error(s, x)
{
	printf("%d: ", lineno);
	printf(s, x);
	putchar('\n');
	exfail++;
	cflag++;
}

sch(c)
{
	register char *rlp;

	rlp = lp;
	if (rlp==line+194)
		error("Line overflow");
	*rlp++ = c;
	if (rlp>line+195)
		rlp = line+195;
	lp = rlp;
}

savch(c)
{
	*stringbuf++ = c;
}

getch()
{
	static peekc;
	register int c;

	if (peekc) {
		c = peekc;
		peekc = 0;
		return(c);
	}
loop:
	if ((c=getc1())=='/' && !instring) {
		if ((peekc=getc1())!='*')
			return('/');
		peekc = 0;
		for(;;) {
			c = getc1();
		cloop:
			switch (c) {

			case '\0':
				return('\0');

			case '*':
				if ((c=getc1())=='/')
					goto loop;
				goto cloop;

			case '\n':
				if (ibuf==ibuf1) {
					putc('\n', obuf);
					lineno++;
				}
				continue;
			}
		}
	}
	return(c);
}

getc1()
{
	register c;

	if ((c = getc(ibuf)) < 0 && ibuf==ibuf2) {
		close(ibuf2[0]);
		ibuf = ibuf1;
		putc('\n', obuf);
		c = getc1();
	}
	if (c<0)
		return(0);
	return(c);
}

lookup(namep, enterf)
char *namep;
{
	register char *np, *snp;
	register struct symtab *sp;
	int i, c;

	np = namep;
	i = 0;
	while (c = *np++)
		i =+ c;
	i =% symsiz;
	sp = &symtab[i];
	while (sp->name[0]) {
		snp = sp;
		np = namep;
		while (*snp++ == *np)
			if (*np++ == '\0' || np==namep+8) {
				if (!enterf)
					subst(namep, sp);
				return(sp);
			}
		if (++sp >= &symtab[symsiz])
			sp = symtab;
	}
	if (enterf) {
		snp = namep;
		for (np = &sp->name[0]; np < &sp->name[8];)
			if (*np++ = *snp)
				snp++;
	}
	return(sp);
}

subst(np, sp)
char *np;
struct symtab *sp;
{
	register char *vp;

	lp = np;
	if ((vp = sp->value) == 0)
		return;
	sch(' ');
	while (*vp)
		sch(*vp++);
	sch(' ');
}

getsuf(as)
char as[];
{
	register int c;
	register char *s;
	register int t;

	s = as;
	c = 0;
	while(t = *s++)
		if (t=='/')
			c = 0;
		else
			c++;
	s =- 3;
	if (c<=14 && c>2 && *s++=='.')
		return(*s);
	return(0);
}

setsuf(as, ch)
char as[];
{
	register char *s, *s1;

	s = s1 = copy(as);
	while(*s)
		if (*s++ == '/')
			s1 = s;
	s[-1] = ch;
	return(s1);
}

callsys(f, v)
char f[], *v[]; {
	int t, status;

	if ((t=fork())==0) {
		execv(f, v);
		printf("Can't find %s\n", f);
		exit(1);
	} else
		if (t == -1) {
			printf("Try again\n");
			return(1);
		}
	while(t!=wait(&status));
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2)		/* interrupt */
			printf("Fatal error in %s\n", f);
		dexit();
	}
	return((status>>8) & 0377);
}

copy(as)
char as[];
{
	register char *otsp, *s;

	otsp = tsp;
	s = as;
	while(*tsp++ = *s++);
	return(otsp);
}

nodup(l, os)
char **l, *os;
{
	register char *t, *s;
	register int c;

	s = os;
	if (getsuf(s) != 'o')
		return(1);
	while(t = *l++) {
		while(c = *s++)
			if (c != *t++)
				break;
		if (*t++ == '\0')
			return(0);
		s = os;
	}
	return(1);
}

cunlink(f)
char *f;
{
	if (f==0)
		return(0);
	return(unlink(f));
}
