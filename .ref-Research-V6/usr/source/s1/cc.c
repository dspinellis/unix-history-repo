#define	SBSIZE	2000
char	sbf[SBSIZE];
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
int depth;
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
struct symtab *eifloc;
struct symtab *ifdloc;
struct symtab *ifnloc;
struct symtab *unxloc;
int	trulvl;
int	flslvl;
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
					pass1 = "/lib/fc1";
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
		av[1] = "-X";
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
	char ln[196];
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
	insym(&defloc, "define");
	insym(&incloc, "include");
	insym(&eifloc, "endif");
	insym(&ifdloc, "ifdef");
	insym(&ifnloc, "ifndef");
	insym(&unxloc, "unix");
	stringbuf = sbf;
	trulvl = 0;
	flslvl = 0;
	line  = ln;
	lineno = 0;
	if (fcreat(tmp4, obuf) < 0) {
		printf("Can't creat %s\n", tmp4);
		dexit();
	}
	while(getline()) {
		if (ibuf==ibuf2 && pflag==0)
			putc(001, obuf);	/*SOH: insert */
		if (ln[0] != '#' && flslvl==0)
			for (rlp = line; c = *rlp++;)
				putc(c, obuf);
		putc('\n', obuf);
	}
	for(rlp=line; c = *rlp++;)
			putc(c,obuf);
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
			if (state>3) {
				if (flslvl==0 &&(state+!lookup(namep,-1)->name[0])==5)
					trulvl++;
				else
					flslvl++;
		out:
				while (c!='\n' && c!= '\0')
					c = getch();
				return(c);
			}
			if (state!=2 || flslvl==0)
				{
				ungetc(c);
				np = lookup(namep, state);
				c = getch();
				}
			if (state==1) {
				if (np==defloc)
					state = 2;
				else if (np==incloc)
					state = 3;
				else if (np==ifnloc)
					state = 4;
				else if (np==ifdloc)
					state = 5;
				else if (np==eifloc) {
					if (flslvl)
						--flslvl;
					else if (trulvl)
						--trulvl;
					else error("If-less endif");
					goto out;
				}
				else {
					error("Undefined control");
					while (c!='\n' && c!='\0')
						c = getch();
					return(c);
				}
			} else if (state==2) {
				if (flslvl)
					goto out;
				np->value = stringbuf;
				savch(c);
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
			if (flslvl)
				goto out;
			if (state==3) {
				if (flslvl)
					goto out;
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

insym(sp, namep)
struct symtab **sp;
char *namep;
{
	register struct symtab *np;

	*sp = np = lookup(namep, 1);
	np->value = np->name;
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
	if (stringbuf-sbf < SBSIZE)
		return;
	error("Too much defining");
	dexit();
}

getch()
{
	register int c;

loop:
	if ((c=getc1())=='/' && !instring) {
		if ((c=getc1())!='*')
			{
			ungetc(c);
			return('/');
			}
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
char pushbuff[300];
char *pushp pushbuff;
ungetc(c)
	{
	*++pushp = c;
	}

getc1()
{
	register c;

	if (*pushp !=0)
		return(*pushp--);
	depth=0;
	if ((c = getc(ibuf)) < 0 && ibuf==ibuf2) {
		close(ibuf2[0]);
		ibuf = ibuf1;
		putc('\n', obuf);
		lineno++;
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
	int i, c, around;
	np = namep;
	around = i = 0;
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
			if (around++)
				{
				error("too many defines");
				dexit();
				}
			else
			sp = symtab;
	}
	if (enterf>0) {
		snp = namep;
		for (np = &sp->name[0]; np < &sp->name[8];)
			if (*np++ = *snp)
				snp++;
	}
	return(sp);
}
char revbuff[200];
char	*bp;
backsch(c)
	{
	if (bp-revbuff > 200)
		error("Excessive define looping", bp--);
	*bp++ = c;
	}

subst(np, sp)
char *np;
struct symtab *sp;
{
	register char *vp;

	lp = np;
	bp = revbuff;
	if (depth++>100)
		{
		error("define recursion loop\n");
		return;
		}
	if ((vp = sp->value) == 0)
		return;
	/* arrange that define unix unix still
	has no effect, avoiding rescanning */
	if (streq(sp->name,sp->value))
		{
		while (*vp)
			sch(*vp++);
		return;
		}
	backsch(' ');
	if (*vp == '(')
		expdef(vp);
	else
	while (*vp)
		backsch(*vp++);
	backsch(' ');
	while (bp>revbuff)
		ungetc(*--bp);
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
		if (*t=='\0' && c=='\0')
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
expdef(proto)
  char *proto;
{
char buffer[100], *parg[20], *pval[20], name[20], *cspace, *wp;
char protcop[100], *pr;
int narg, k, i, c;
pr = protcop;
while (*pr++ = *proto++);
proto= protcop;
for (narg=0; (parg[narg] = token(&proto)) != 0; narg++)
	;
/* now scan input */
cspace = buffer;
while ((c=getch()) == ' ');
if (c != '(')
	{
	error("defined function requires arguments");
	return;
	}
ungetc(c);
for(k=0; pval[k] = coptok(&cspace); k++);
if (k!=narg)
 {
  error("define argument mismatch");
  return;
 }
while (c= *proto++)
   {
   if (!letter(c))
      backsch(c);
   else
      {
      wp = name;
      *wp++ = c;
      while (letnum(*proto))
        *wp++ = *proto++;
      *wp = 0;
      for (k=0; k<narg; k++)
      if(streq(name,parg[k]))
        break;
      wp = k <narg ? pval[k] : name;
      while (*wp) backsch(*wp++);
      }
   }
}
token(cpp) char **cpp;
{
char *val;
int stc;
stc = **cpp;
*(*cpp)++ = '\0';
if (stc==')') return(0);
while (**cpp == ' ') (*cpp)++;
for (val = *cpp; (stc= **cpp) != ',' && stc!= ')'; (*cpp)++)
  {
  if (!letnum(stc) || (val == *cpp && !letter(stc)))
    {
    error("define prototype argument error");
    break;
    }
  }
return(val);
}
coptok (cpp) char **cpp; {
char *val;
int stc, stop,paren;
paren = 0;
val = *cpp;
if (getch() == ')')
  return(0);
while (((stc = getch()) != ',' && stc != ')') || paren > 0)
  {
  if (stc == '"' || stc == '\'')
    {
    stop = stc;
    if (stop == '\'')
      *(*cpp)++ = '\'';
    while ( (stc = getch()) != stop)
      {
      if (stc == '\n')
        {
        error ("non-terminated string");
        break;
        }
      if (stc == '\\')
        if ((stc= getch()) != stop && stc != '\\')
          *(*cpp)++ = '\\';
      *(*cpp)++ = stc;
      }
    if (stop == '\'') 
      *(*cpp)++ = '\'';
    }
  else if (stc == '\\')
      {
      stc = getch();
      if (stc != '"' && stc != '\\')
        *(*cpp)++ = '\\';
      *(*cpp)++ = stc;
      }
  else
    {
    *(*cpp)++ = stc;
    if (stc == '(')
        paren++;
    if (stc == ')')
        paren--;
    }
  }
*(*cpp)++ = 0;
ungetc(stc);
return(val);
}
letter(c)
{
if ((c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c == '_'))
    return (1);
else
    return(0);
}
letnum(c)
{
if (letter(c) || (c >= '0' && c <= '9'))
  return(1);
else
  return(0);
}
streq(s,t) char *s, *t;
{
int c;
while ( (c= *s++) == *t++)
   if (c==0) return(1);
return(0);
}
