#
/*
 */

#define	INTR	2
#define	QUIT	3
#define LINSIZ 1000
#define ARGSIZ 50
#define TRESIZ 100

#define QUOTE 0200
#define FAND 1
#define FCAT 2
#define FPIN 4
#define FPOU 8
#define FPAR 16
#define FINT 32
#define FPRS 64
#define TCOM 1
#define TPAR 2
#define TFIL 3
#define TLST 4
#define DTYP 0
#define DLEF 1
#define DRIT 2
#define DFLG 3
#define DSPR 4
#define DCOM 5
#define	ENOMEM	12
#define	ENOEXEC 8

char	*dolp;
char	pidp[6];
int	ldivr;
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
	"Bad system call",
	0,
	"Sig 14",
	"Sig 15",
	"Sig 16",
	"Sig 17",
	"Sig 18",
	"Sig 19",
};

struct stime {
	int proct[2];
	int cputim[2];
	int systim[2];
} timeb;

char	line[LINSIZ];
char	*args[ARGSIZ];
int	trebuf[TRESIZ];

main(c, av)
int c;
char **av;
{
	register f;
	register char *acname, **v;

	for(f=2; f<15; f++)
		close(f);
	if((f=dup(1)) != 2)
		close(f);
	dolc = getpid();
	for(f=4; f>=0; f--) {
		dolc = ldiv(0, dolc, 10);
		pidp[f] = ldivr+'0';
	}
	v = av;
	acname = "/usr/adm/sha";
	promp = "% ";
	if(((uid = getuid())&0377) == 0)
		promp = "# ";
	acctf = open(acname, 1);
	if(c > 1) {
		promp = 0;
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
		signal(QUIT, 1);
		signal(INTR, 1);
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
	register char c, *cp;
	register *t;

	argp = args;
	eargp = args+ARGSIZ-5;
	linep = line;
	elinep = line+LINSIZ-5;
	error = 0;
	gflg = 0;
	do {
		cp = linep;
		word();
	} while(*cp != '\n');
	treep = trebuf;
	treeend = &trebuf[TRESIZ];
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
			*linep++ = c|QUOTE;
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
		return(c|QUOTE);
	}
	if(c == '$') {
		c = readc();
		if(c>='0' && c<='9') {
			if(c-'0' < dolc)
				dolp = dolv[c-'0'];
			goto getd;
		}
		if(c == '$') {
			dolp = pidp;
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
			l = **p;
			t = tree(4);
			t[DTYP] = TLST;
			t[DLEF] = syn2(p1, p);
			t[DFLG] = 0;
			if(l == '&') {
				t1 = t[DLEF];
				t1[DFLG] =| FAND|FPRS|FINT;
			}
			t[DRIT] = syntax(p+1, p2);
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
			t[DTYP] = TFIL;
			t[DLEF] = syn3(p1, p);
			t[DRIT] = syn2(p+1, p2);
			t[DFLG] = 0;
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
	int n, l, i, o, c, flg;

	flg = 0;
	if(**p2 == ')')
		flg =| FPAR;
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
			flg =| FCAT; else
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
			p1[n++] = *p;
	}
	if(lp != 0) {
		if(n != 0)
			error++;
		t = tree(5);
		t[DTYP] = TPAR;
		t[DSPR] = syn1(lp, rp);
		goto out;
	}
	if(n == 0)
		error++;
	p1[n++] = 0;
	t = tree(n+5);
	t[DTYP] = TCOM;
	for(l=0; l<n; l++)
		t[l+DCOM] = p1[l];
out:
	t[DFLG] = flg;
	t[DLEF] = i;
	t[DRIT] = o;
	return(t);
}

scan(at, f)
int *at;
int (*f)();
{
	register char *p, c;
	register *t;

	t = at+DCOM;
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
	switch(t[DTYP]) {

	case TCOM:
		cp1 = t[DCOM];
		if(equal(cp1, "chdir")) {
			if(t[DCOM+1] != 0) {
				if(chdir(t[DCOM+1]) < 0)
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
				execv("/bin/login", t+DCOM);
			}
			prs("login: cannot execute\n");
			return;
		}
		if(equal(cp1, "newgrp")) {
			if(promp != 0) {
				close(acctf);
				execv("/bin/newgrp", t+DCOM);
			}
			prs("newgrp: cannot execute\n");
			return;
		}
		if(equal(cp1, "wait")) {
			pwait(-1, 0);
			return;
		}
		if(equal(cp1, ":"))
			return;

	case TPAR:
		f = t[DFLG];
		i = 0;
		if((f&FPAR) == 0)
			i = fork();
		if(i == -1) {
			err("try again");
			return;
		}
		if(i != 0) {
			if((f&FPIN) != 0) {
				close(pf1[0]);
				close(pf1[1]);
			}
			if((f&FPRS) != 0) {
				prn(i);
				prs("\n");
			}
			if((f&FAND) != 0)
				return;
			if((f&FPOU) == 0)
				pwait(i, t);
			return;
		}
		if(t[DLEF] != 0) {
			close(0);
			i = open(t[DLEF], 0);
			if(i < 0) {
				prs(t[DLEF]);
				err(": cannot open");
				exit();
			}
		}
		if(t[DRIT] != 0) {
			if((f&FCAT) != 0) {
				i = open(t[DRIT], 1);
				if(i >= 0) {
					seek(i, 0, 2);
					goto f1;
				}
			}
			i = creat(t[DRIT], 0666);
			if(i < 0) {
				prs(t[DRIT]);
				err(": cannot create");
				exit();
			}
		f1:
			close(1);
			dup(i);
			close(i);
		}
		if((f&FPIN) != 0) {
			close(0);
			dup(pf1[0]);
			close(pf1[0]);
			close(pf1[1]);
		}
		if((f&FPOU) != 0) {
			close(1);
			dup(pf2[1]);
			close(pf2[0]);
			close(pf2[1]);
		}
		if((f&FINT)!=0 && t[DLEF]==0 && (f&FPIN)==0) {
			close(0);
			open("/dev/null", 0);
		}
		if((f&FINT) == 0 && setintr) {
			signal(INTR, 0);
			signal(QUIT, 0);
		}
		if(t[DTYP] == TPAR) {
			if(t1 = t[DSPR])
				t1[DFLG] =| f&FINT;
			execute(t1);
			exit();
		}
		close(acctf);
		gflg = 0;
		scan(t, &tglob);
		if(gflg) {
			t[DSPR] = "/etc/glob";
			execv(t[DSPR], t+DSPR);
			prs("glob: cannot execute\n");
			exit();
		}
		scan(t, &trim);
		*linep = 0;
		texec(t[DCOM], t);
		cp1 = linep;
		cp2 = "/usr/bin/";
		while(*cp1 = *cp2++)
			cp1++;
		cp2 = t[DCOM];
		while(*cp1++ = *cp2++);
		texec(linep+4, t);
		texec(linep, t);
		prs(t[DCOM]);
		err(": not found");
		exit();

	case TFIL:
		f = t[DFLG];
		pipe(pv);
		t1 = t[DLEF];
		t1[DFLG] =| FPOU | (f&(FPIN|FINT|FPRS));
		execute(t1, pf1, pv);
		t1 = t[DRIT];
		t1[DFLG] =| FPIN | (f&(FPOU|FINT|FAND|FPRS));
		execute(t1, pv, pf2);
		return;

	case TLST:
		f = t[DFLG]&FINT;
		if(t1 = t[DLEF])
			t1[DFLG] =| f;
		execute(t1);
		if(t1 = t[DRIT])
			t1[DFLG] =| f;
		execute(t1);
		return;

	}
}

texec(f, at)
int *at;
{
	extern errno;
	register int *t;

	t = at;
	execv(f, t+DCOM);
	if (errno==ENOEXEC) {
		if (*linep)
			t[DCOM] = linep;
		t[DSPR] = "/bin/sh";
		execv(t[DSPR], t+DSPR);
		prs("No shell!\n");
		exit();
	}
	if (errno==ENOMEM) {
		prs(t[DCOM]);
		err(": too large");
		exit();
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
	if(*t == TPAR)
		enacct("()"); else
	enacct(t[DCOM]);
}

enacct(as)
char *as;
{
	struct stime timbuf;
	struct {
		char cname[14];
		char shf;
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
	tbuf.shf = 0;
	if (promp==0)
		tbuf.shf = 1;
	seek(acctf, 0, 2);
	write(acctf, &tbuf, sizeof(tbuf));
}
