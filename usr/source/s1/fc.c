/* Fortran command */

char	*tmp;
char ts[1000];
char *tsp ts;
char *av[50];
char *clist[50];
char *llist[50];
int instring;
int pflag;
int cflag;
char	*complr;
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

main(argc, argv)
char *argv[]; {
	char *t;
	int nc, nl, i, j, c, nxo;
	int dexit();

	complr = "/usr/fort/fc1";
	i = nc = nl = nxo = 0;
	while(++i < argc) {
		if(*argv[i] == '-')
			switch (argv[i][1]) {
				default:
					goto passa;
				case 'p':
					pflag++;
				case 'c':
					cflag++;
					break;
				case '2':
					complr = "/usr/fort/fc2";
					break;
			}
		else {
		passa:
			t = argv[i];
			if(getsuf(t)=='f') {
				clist[nc++] = t;
				t = setsuf(copy(t), 'o');
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
	if ((signal(2, 1) & 01) == 0)
		signal(2, &dexit);
	for (i=0; i<nc; i++) {
		if (nc>1)
			printf("%s:\n", clist[i]);
		tmp = 0;
		av[0] = complr;
		av[1] = expand(clist[i]);
		if (pflag || exfail)
			continue;
		if (av[1] == 0) {
			cflag++;
			continue;
		}
		av[2] = 0;
		t = callsys(complr, av);
		if(tmp)
			cunlink(tmp);
		if(t) {
			cflag++;
			continue;
		}
		av[0] = "as";
		av[1] = "-";
		av[2] = "f.tmp1";
		av[3] = 0;
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
		av[2] = "/lib/fr0.o";
		j = 3;
		while(i<nl)
			av[j++] = llist[i++];
		av[j++] = "-lf";
		av[j++] = "/lib/filib.a";
		av[j++] = "-l";
		av[j++] = 0;
		callsys("/bin/ld", av);
		if (nc==1 && nxo==1)
			cunlink(setsuf(clist[0], 'o'));
	}
	dexit();
}

dexit()
{
	unlink("f.tmp1");
	exit();
}

expand(file)
char *file;
{
	int ib1[259], ib2[259], ob[259];
	struct symtab stab[200];
	char ln[196], sbf[1024];
	int c;

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
	tmp = setsuf(copy(file), 'i');
	if (fcreat(tmp, obuf) < 0) {
		printf("Can't creat %s\n", tmp);
		dexit();
	}
	while(getline()) {
/*
		if (ibuf==ibuf2)
			putc(001, obuf);	/*SOH: insert */
		if (ln[0] != '#')
			for (lp=line; *lp!='\0'; lp++)
				putc(*lp, obuf);
		putc('\n', obuf);
	}
	fflush(obuf);
	close(obuf[0]);
	close(ibuf1[0]);
	return(tmp);
}

getline()
{
	int c, sc, state;
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
	if (lp==line+194)
		error("Line overflow");
	*lp++ = c;
	if (lp>line+195)
		lp = line+195;
}

savch(c)
{
	*stringbuf++ = c;
}

getch()
{
	static peekc;
	int c;

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
	int c;

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
	char *np, *snp;
	struct symtab *sp;
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
		if (sp++ > &symtab[symsiz])
			sp = symtab;
	}
	if (enterf) {
		for (i=0; i<8; i++)
			if (sp->name[i] = *namep)
				namep++;
		while (*namep)
			namep++;
	}
	return(sp);
}

subst(np, sp)
char *np;
struct symtab *sp;
{
	char *vp;

	lp = np;
	if ((vp = sp->value) == 0)
		return;
	sch(' ');
	while (*vp)
		sch(*vp++);
	sch(' ');
}

getsuf(s)
char s[];
{
	int c;
	char t, *os;

	c = 0;
	os = s;
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

setsuf(s, ch)
char s[];
{
	char *os;

	os = s;
	while(*s++);
	s[-2] = ch;
	return(os);
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

copy(s)
char s[]; {
	char *otsp;

	otsp = tsp;
	while(*tsp++ = *s++);
	return(otsp);
}

nodup(l, s)
char **l, s[]; {
	char *t, *os, c;

	if (getsuf(s) != 'o')
		return(1);
	os = s;
	while(t = *l++) {
		s = os;
		while(c = *s++)
			if (c != *t++)
				break;
		if (*t++ == '\0')
			return(0);
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
