#
/*
 * copyright 1973 bell telephone laboratories inc.
 */

#define	intr	2
#define	quit	3
#define linesiz 1000
#define argsiz 50
#define tresiz 100

#define quote 0200
#define fand 1
#define fcat 2
#define fpin 4
#define fpou 8
#define fpar 16
#define fint 32
#define tcom 1
#define tpar 2
#define tfil 3
#define tlst 4
#define dtyp 0
#define dlef 1
#define drit 2
#define dflg 3
#define dspr 4
#define dcom 5
#define	ENOEXEC 8

char	*dolp;
char	**dolv;
int	dolc;
char	*promp;
char	*linep;
char	*elinep;
char	**argp;
char	**eargp;
int	*treep;
int	*treeend;
char	peekc;
char	gflg;
char	error;
char	acctf;
char	uid;
char	setintr;
char	*arginp;
int	onelflg;

char	*mesg[] {
	0,
	"Hangup",
	0,
	"Quit",
	"Illegal instruction",
	"Trace/BPT trap",
	"IOT trap",
	"EMT trap",
	"Floating exception",
	"Killed",
	"Bus error",
	"Memory fault",
	"Bad system call"
};

struct stime {
	int proct[2];
	int cputim[2];
	int systim[2];
} timeb;

main(c, av)
int c;
char **av;
{
	register f;
	register char *acname, **v;

	close(2);
	if((f=dup(1)) != 2)
		close(f);
	v = av;
	acname = "/usr/adm/sh_acct";
	promp = "% ";
	if(((uid = getuid())&0377) == 0) {
		promp = "# ";
		acname = "/usr/adm/su_acct";
	}
	acctf = open(acname, 1);
	if(c > 1) {
		promp = 0;
		for (f=3; f<=15; f++)
			close(f);
		if (*v[1]=='-') {
			**v = '-';
			if (v[1][1]=='c' && c>2)
				arginp = v[2];
			else if (v[1][1]=='t')
				onelflg = 2;
		} else {
			close(0);
			f = open(v[1], 0);
			if(f < 0) {
				prs(v[1]);
				err(": cannot open");
			}
		}
	}
	if(**v == '-') {
		setintr++;
		signal(quit, 1);
		signal(intr, 1);
	}
	dolv = v+1;
	dolc = c-1;

loop:
	if(promp != 0)
		prs(promp);
	peekc = getc();
	main1();
	goto loop;
}

main1()
{
	char line[linesiz];
	char *args[argsiz];
	int trebuf[tresiz];
	register char c, *cp;
	register *t;

	argp = args;
	eargp = args+argsiz-5;
	linep = line;
	elinep = line+linesiz-5;
	error = 0;
	gflg = 0;
	do {
		cp = linep;
		word();
	} while(*cp != '\n');
	treep = trebuf;
	treeend = &trebuf[tresiz];
	if(gflg == 0) {
		if(error == 0) {
			setexit();
			if (error)
				return;
			t = syntax(args, argp);
		}
		if(error != 0)
			err("syntax error"); else
			execute(t);
	}
}

word()
{
	register char c, c1;

	*argp++ = linep;

loop:
	switch(c = getc()) {

	case ' ':
	case '\t':
		goto loop;

	case '\'':
	case '"':
		c1 = c;
		while((c=readc()) != c1) {
			if(c == '\n') {
				error++;
				peekc = c;
				return;
			}
			*linep++ = c|quote;
		}
		goto pack;

	case '&':
	case ';':
	case '<':
	case '>':
	case '(':
	case ')':
	case '|':
	case '^':
	case '\n':
		*linep++ = c;
		*linep++ = '\0';
		return;
	}

	peekc = c;

pack:
	for(;;) {
		c = getc();
		if(any(c, " '\"\t;&<>()|^\n")) {
			peekc = c;
			if(any(c, "\"'"))
				goto loop;
			*linep++ = '\0';
			return;
		}
		*linep++ = c;
	}
}

tree(n)
int n;
{
	register *t;

	t = treep;
	treep =+ n;
	if (treep>treeend) {
		prs("Command line overflow\n");
		error++;
		reset();
	}
	return(t);
}

getc()
{
	register char c;

	if(peekc) {
		c = peekc;
		peekc = 0;
		return(c);
	}
	if(argp > eargp) {
		argp =- 10;
		while((c=getc()) != '\n');
		argp =+ 10;
		err("Too many args");
		gflg++;
		return(c);
	}
	if(linep > elinep) {
		linep =- 10;
		while((c=getc()) != '\n');
		linep =+ 10;
		err("Too many characters");
		gflg++;
		return(c);
	}
getd:
	if(dolp) {
		c = *dolp++;
		if(c != '\0')
			return(c);
		dolp = 0;
	}
	c = readc();
	if(c == '\\') {
		c = readc();
		if(c == '\n')
			return(' ');
		return(c|quote);
	}
	if(c == '$') {
		c = getc();
		if(c>='0' && c<='9') {
			if(c-'0' < dolc)
				dolp = dolv[c-'0'];
			goto getd;
		}
	}
	return(c&0177);
}

readc()
{
	char cc;
	register c;

	if (arginp) {
		if (arginp == 1)
			exit();
		if ((c = *arginp++) == 0) {
			arginp = 1;
			c = '\n';
		}
		return(c);
	}
	if (onelflg==1)
		exit();
	if(read(0, &cc, 1) != 1)
		exit();
	if (cc=='\n' && onelflg)
		onelflg--;
	return(cc);
}

/*
 * syntax
 *	empty
 *	syn1
 */

syntax(p1, p2)
char **p1, **p2;
{

	while(p1 != p2) {
		if(any(**p1, ";&\n"))
			p1++; else
			return(syn1(p1, p2));
	}
	return(0);
}

/*
 * syn1
 *	syn2
 *	syn2 & syntax
 *	syn2 ; syntax
 */

syn1(p1, p2)
char **p1, **p2;
{
	register char **p;
	register *t, *t1;
	int l;

	l = 0;
	for(p=p1; p!=p2; p++)
	switch(**p) {

	case '(':
		l++;
		continue;

	case ')':
		l--;
		if(l < 0)
			error++;
		continue;

	case '&':
	case ';':
	case '\n':
		if(l == 0) {
			t = tree(4);
			t[dtyp] = tlst;
			t[dlef] = syn2(p1, p);
			t[dflg] = 0;
			if(**p == '&') {
				t1 = t[dlef];
				t1[dflg] =| fand|fint;
			}
			t[drit] = syntax(p+1, p2);
			return(t);
		}
	}
	if(l == 0)
		return(syn2(p1, p2));
	error++;
}

/*
 * syn2
 *	syn3
 *	syn3 | syn2
 */

syn2(p1, p2)
char **p1, **p2;
{
	register char **p;
	register int l, *t;

	l = 0;
	for(p=p1; p!=p2; p++)
	switch(**p) {

	case '(':
		l++;
		continue;

	case ')':
		l--;
		continue;

	case '|':
	case '^':
		if(l == 0) {
			t = tree(4);
			t[dtyp] = tfil;
			t[dlef] = syn3(p1, p);
			t[drit] = syn2(p+1, p2);
			t[dflg] = 0;
			return(t);
		}
	}
	return(syn3(p1, p2));
}

/*
 * syn3
 *	( syn1 ) [ < in  ] [ > out ]
 *	word word* [ < in ] [ > out ]
 */

syn3(p1, p2)
char **p1, **p2;
{
	register char **p;
	char **lp, **rp;
	register *t;
	int b[100], n, l, i, o, c, flg;

	flg = 0;
	if(**p2 == ')')
		flg =| fpar;
	lp = 0;
	rp = 0;
	i = 0;
	o = 0;
	n = 0;
	l = 0;
	for(p=p1; p!=p2; p++)
	switch(c = **p) {

	case '(':
		if(l == 0) {
			if(lp != 0)
				error++;
			lp = p+1;
		}
		l++;
		continue;

	case ')':
		l--;
		if(l == 0)
			rp = p;
		continue;

	case '>':
		p++;
		if(p!=p2 && **p=='>')
			flg =| fcat; else
			p--;

	case '<':
		if(l == 0) {
			p++;
			if(p == p2) {
				error++;
				p--;
			}
			if(any(**p, "<>("))
				error++;
			if(c == '<') {
				if(i != 0)
					error++;
				i = *p;
				continue;
			}
			if(o != 0)
				error++;
			o = *p;
		}
		continue;

	default:
		if(l == 0)
			b[n++] = *p;
	}
	if(lp != 0) {
		if(n != 0)
			error++;
		t = tree(5);
		t[dtyp] = tpar;
		t[dspr] = syn1(lp, rp);
		goto out;
	}
	if(n == 0)
		error++;
	b[n++] = 0;
	t = tree(n+5);
	t[dtyp] = tcom;
	for(l=0; l<n; l++)
		t[l+dcom] = b[l];
out:
	t[dflg] = flg;
	t[dlef] = i;
	t[drit] = o;
	return(t);
}

scan(at, f)
int *at;
int (*f)();
{
	register char *p, c;
	register *t;

	t = at+dcom;
	while(p = *t++)
		while(c = *p)
			*p++ = (*f)(c);
}

tglob(c)
int c;
{

	if(any(c, "[?*"))
		gflg = 1;
	return(c);
}

trim(c)
int c;
{

	return(c&0177);
}

execute(t, pf1, pf2)
int *t, *pf1, *pf2;
{
	int i, f, pv[2];
	register *t1;
	register char *cp1, *cp2;
	extern errno;

	if(t != 0)
	switch(t[dtyp]) {

	case tcom:
		cp1 = t[dcom];
		if(equal(cp1, "chdir")) {
			if(t[dcom+1] != 0) {
				if(chdir(t[dcom+1]) < 0)
					err("chdir: bad directory");
			} else
				err("chdir: arg count");
			return;
		}
		if(equal(cp1, "shift")) {
			if(dolc < 1) {
				prs("shift: no args\n");
				return;
			}
			dolv[1] = dolv[0];
			dolv++;
			dolc--;
			return;
		}
		if(equal(cp1, "login")) {
			if(promp != 0) {
				close(acctf);
				execv("/bin/login", t+dcom);
			}
			prs("login: cannot execute\n");
			return;
		}
		if(equal(cp1, "wait")) {
			pwait(-1, 0);
			return;
		}
		if(equal(cp1, ":"))
			return;

	case tpar:
		f = t[dflg];
		i = 0;
		if((f&fpar) == 0)
			i = fork();
		if(i == -1) {
			err("try again");
			return;
		}
		if(i != 0) {
			if((f&fpin) != 0) {
				close(pf1[0]);
				close(pf1[1]);
			}
			if((f&fand) != 0) {
				prn(i);
				prs("\n");
				return;
			}
			if((f&fpou) == 0)
				pwait(i, t);
			return;
		}
		if(t[dlef] != 0) {
			close(0);
			i = open(t[dlef], 0);
			if(i < 0) {
				prs(t[dlef]);
				err(": cannot open");
				exit();
			}
		}
		if(t[drit] != 0) {
			if((f&fcat) != 0) {
				i = open(t[drit], 1);
				if(i >= 0) {
					seek(i, 0, 2);
					goto f1;
				}
			}
			i = creat(t[drit], 0666);
			if(i < 0) {
				prs(t[drit]);
				err(": cannot create");
				exit();
			}
		f1:
			close(1);
			dup(i);
			close(i);
		}
		if((f&fpin) != 0) {
			close(0);
			dup(pf1[0]);
			close(pf1[0]);
			close(pf1[1]);
		}
		if((f&fpou) != 0) {
			close(1);
			dup(pf2[1]);
			close(pf2[0]);
			close(pf2[1]);
		}
		if((f&fint)!=0 && t[dlef]==0 && (f&fpin)==0) {
			close(0);
			open("/dev/null", 0);
		}
		if((f&fint) == 0 && setintr) {
			signal(intr, 0);
			signal(quit, 0);
		}
		if(t[dtyp] == tpar) {
			if(t1 = t[dspr])
				t1[dflg] =| f&fint;
			execute(t1);
			exit();
		}
		close(acctf);
		gflg = 0;
		scan(t, &tglob);
		if(gflg) {
			t[dspr] = "/etc/glob";
			execv(t[dspr], t+dspr);
			prs("glob: cannot execute\n");
			exit();
		}
		scan(t, &trim);
		*linep = 0;
		execv(t[dcom], t+dcom);
		if (errno==ENOEXEC)
			goto runcom;
		cp1 = linep;
		cp2 = "/usr/bin/";
		while(*cp1 = *cp2++)
			cp1++;
		cp2 = t[dcom];
		while(*cp1++ = *cp2++);
		execv(linep+4, t+dcom);
		if (errno==ENOEXEC)
			goto runcom;
		execv(linep, t+dcom);
		if (errno==ENOEXEC)
			goto runcom;
		prs(t[dcom]);
		err(": not found");
		exit();
	runcom:
		if (*linep)
			t[dcom] = linep;
		t[dspr] = "/bin/sh";
		execv(t[dspr], t+dspr);
		prs("No shell!\n");
		exit();

	case tfil:
		f = t[dflg];
		pipe(pv);
		t1 = t[dlef];
		t1[dflg] =| fpou | (f&(fpin|fint));
		execute(t1, pf1, pv);
		t1 = t[drit];
		t1[dflg] =| fpin | (f&(fpou|fint|fand));
		execute(t1, pv, pf2);
		return;

	case tlst:
		f = t[dflg]&fint;
		if(t1 = t[dlef])
			t1[dflg] =| f;
		execute(t1);
		if(t1 = t[drit])
			t1[dflg] =| f;
		execute(t1);
		return;

	}
}

err(s)
char *s;
{

	prs(s);
	prs("\n");
	if(promp == 0) {
		seek(0, 0, 2);
		exit();
	}
}

prs(as)
char *as;
{
	register char *s;

	s = as;
	while(*s)
		putc(*s++);
}

putc(c)
{

	write(2, &c, 1);
}

prn(n)
int n;
{
	register a;

	if(a=ldiv(0,n,10))
		prn(a);
	putc(lrem(0,n,10)+'0');
}

any(c, as)
int c;
char *as;
{
	register char *s;

	s = as;
	while(*s)
		if(*s++ == c)
			return(1);
	return(0);
}

equal(as1, as2)
char *as1, *as2;
{
	register char *s1, *s2;

	s1 = as1;
	s2 = as2;
	while(*s1++ == *s2)
		if(*s2++ == '\0')
			return(1);
	return(0);
}

pwait(i, t)
int i, *t;
{
	register p, e;
	int s;

	if(i != 0)
	for(;;) {
		times(&timeb);
		time(timeb.proct);
		p = wait(&s);
		if(p == -1)
			break;
		e = s&0177;
		if(mesg[e] != 0) {
			if(p != i) {
				prn(p);
				prs(": ");
			}
			prs(mesg[e]);
			if(s&0200)
				prs(" -- Core dumped");
		}
		if(e != 0)
			err("");
		if(i == p) {
			acct(t);
			break;
		} else
			acct(0);
	}
}

acct(t)
int *t;
{
	if(t == 0)
		enacct("**gok"); else
	if(*t == tpar)
		enacct("()"); else
	enacct(t[dcom]);
}

enacct(as)
char *as;
{
	struct stime timbuf;
	struct {
		char cname[14];
		char fill;
		char uid;
		int datet[2];
		int realt[2];
		int bcput[2];
		int bsyst[2];
	} tbuf;
	register i;
	register char *np, *s;

	s = as;
	times(&timbuf);
	time(timbuf.proct);
	lsub(tbuf.realt, timbuf.proct, timeb.proct);
	lsub(tbuf.bcput, timbuf.cputim, timeb.cputim);
	lsub(tbuf.bsyst, timbuf.systim, timeb.systim);
	do {
		np = s;
		while (*s != '\0' && *s != '/')
			s++;
	} while (*s++ != '\0');
	for (i=0; i<14; i++) {
		tbuf.cname[i] = *np;
		if (*np)
			np++;
	}
	tbuf.datet[0] = timbuf.proct[0];
	tbuf.datet[1] = timbuf.proct[1];
	tbuf.uid = uid;
	seek(acctf, 0, 2);
	write(acctf, &tbuf, sizeof(tbuf));
}
