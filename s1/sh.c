#
/*
 * UC berkeley shell
 *
 * Modified by Bill Joy Oct 14, 1976
 *
 *  library "lib"
 *  cd for "chdir"
 *  interruptible waits
 *  new accounting scheme
 *  interactive (login shell) option "-i"
 *  shells with input and output tty's ignore interrupts
 *  verbose option "-v" causes input commands to be echoed
 *  $* for all arguments $1 to end
 *  >* >>* and |* to put unit 2 there also
 *  `cmd1 ; ... ; cmdn &*' means `( cmd1 ; ... ; cmdn ) &'
 *  pascal objects are recognized and px's are forked
 *  gimme and normal
 *  working directory not searched for root
 */

char	shell[]		"/bin/sh";
char	glob[]		"/etc/glob2";
char	px[]		"/bin/px";

#define	PCXN	0404
#define	INTR	2
#define	QUIT	3

#define LINSIZ 	1000
#define ARGSIZ 	50
#define TRESIZ 	100
#define LIBSIZ 	100

#define QUOTE 	0200

#define FAND 	1
#define FCAT 	2
#define FPIN 	4
#define FPOU 	8
#define FPAR 	16
#define FINT 	32
#define FPRS 	64
#define FDIAG 	128

#define	TCOM	1
#define	TPAR	2
#define	TFIL	3
#define	TLST	4

#define	DTYP	0
#define	DLEF	1
#define	DRIT	2
#define	DFLG	3
#define	DSPR	4
#define	DCOM	5

#define	ENOEXEC	8
#define	ENOMEM	12

int	niceness;
char	*dolp;
int	*dolnxt;
char	pidp[7];
int	dolc;
char	**dolv;
char	*promp		"% ";
char	*linep;
char	**argp;
int	*treep;
char	peekc;
char	gflg;
char	nofile;
char	error;
char	acctf;
char	*accpt;
char	*accbuf;
int	acct;
char	acnm[]		"/usr/adm/sha\0\0\0";
char	ty;
char	tyed;
char	echo;
int	uid;
char	setintr;
char	*arginp;
char	onelflg;

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
	"Alarm clock",
	"Sig 15",
	"Sig 16",
};

char	line[LINSIZ];
char	*args[ARGSIZ];
int	trebuf[TRESIZ];
char	*lib;
char	*numpt		&pidp[sizeof pidp - 1];

main(c, av)
int c;
char **av;
{
	register f;
	register char **v, *cp;

	for(f=3; f<15; f++)
		close(f);
	numb(numb(getpid()));
	if ((uid = getuid()) == 0)
		promp[0] = '#';
	v = av;
	dolnxt = &v[c];
	if (c > 1) {
		if (*(cp = v[1]) == '-') {
			do
				switch(*cp++) {
					case 'v':
						echo++;
						break;
					case 'c':
						if (c > 2)
							arginp = v[2];
						goto l1;
					case 't':
						onelflg = 2;
					case '\0':

l1:
						*promp = '\0';
					case 'i':
						**v = '-';
						nofile++;
						break;
				}
			while (*cp);
			v++;
			c--;
		}
		if (nofile == 0 && c > 1) {
			close(0);
			if (open(cp = v[1], 0) < 0) {
				prs(v[1]);
				err(": cannot open");
			}
			*promp = '\0';
		}
	}
	if ((ty = ttyn(0)) != 'x' && ttyn(1) != 'x')
		**v = '-';
	if (**av == '-') {
		setintr++;
		signal(QUIT, 1);
		signal(INTR, 1);
	}
	dolv = v+1;
	dolc = c-1;
	numpt = &acnm[sizeof acnm - 1];
	numb((uid>>8) & 0377);
	numb(uid & 0377);
	if ((acctf = open(acnm, 1)) >= 0 || echo)
		accbuf = sbrk(256);
	setexit();

	for(;;) {
		prs(promp);
		if ((cp = accbuf) != 0) {
			*cp++ = ty;
			tyed++;
			*cp++ = ' ';
			accpt = cp;
			acct = 254;
		}
		peekc = getc();
		main1();
	}
}

numb(a)
{
	register i, j;

	i = a;
	j = 3;
	do {
		*--numpt = (i % 10) | '0';
		i =/ 10;
	} while(--j);
	return(i);
}

acflush()
{
	register f, c;
	register char *b;

	if ((b = accbuf) != 0) {
		c = 256 - acct;
		if ((f = acctf)>= 0) {
			seek(f, 0, 2);
			write(f, b, c);
		}
		if (echo) {
			if (tyed)
				b[0] = uid ? '%' : '#';
			seek(2, 0, 2);
			write(2, b, c);
		}
		accpt = b;
		acct = 256;
	}
}

main1()
{
	register char c, *cp;
	register *t;

	argp = args;
	linep = line;
	error = 0;
	gflg = 0;
	do {
		cp = linep;
		word();
	} while(*cp != '\n');
	treep = trebuf;
	if (gflg == 0) {
		if (error == 0)
			t = syntax(args, argp);
		if (error != 0)
			err("syntax error");
		else {
			acflush();
			execute(t);
		}
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
		while((c = readc()) != c1) {
			if (c == '\n') {
				error++;
				peekc = c;
				return;
			}
			*linep++ = c|QUOTE;
		}
		goto pack;

	case '&':
		*linep++ = c;
		if ((c = getc()) == '*')
			*linep++ = c;
		else
			peekc = c;
		*linep++ = '\0';
		return;

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
		if (any(c, " '\"\t;&<>()|^\n")) {
			peekc = c;
			if (any(c, "\"'"))
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
	if (treep > &trebuf[TRESIZ]) {
		prs("Command line overflow\n");
		error++;
		reset();	/* UGH */
	}
	return(t);
}

getc()
{
	register c;

	if (peekc) {
		c = peekc;
		peekc = 0;
		return(c);
	}
	if (argp > &args[ARGSIZ-5]) {
		argp =- 10;
		while((c=getc()) != '\n');
		argp =+ 10;
		err("Too many args");
		gflg++;
		return(c);
	}
	if (linep > &line[LINSIZ-5]) {
		linep =- 10;
		while((c=getc()) != '\n');
		linep =+ 10;
		err("Too many characters");
		gflg++;
		return(c);
	}
	for(;;) {
		if (dolp) {
			if ((c = *dolp++) != '\0')
				return(c);
			dolp = *dolnxt;
			c = dolp;
			if (c != -1) {
				dolnxt++;
				return(' ');
			}
			dolp = 0;
		}
		if ((c = readc()) == '\\')
			if ((c = readc()) == '\n')
				return(' ');
			else
				return(c | QUOTE);
		else if (c == '$') {
			c = readc();
			if (c >= '0' && c <= '9') {
				if (c - '0' < dolc)
					dolp = dolv[c - '0'];
				continue;
			}
			else if (c == '$') {
				dolp = &pidp[1];
				continue;
			}
			else if (c == '*') {
				if (dolc > 1) {
					dolp = dolv[1];
					dolnxt = &dolv[2];
				}
				continue;
			}
		}
		return(c&0177);
	}
}

readc()
{
	char cc;
	register c;
	register char *cp;

	if (arginp) {
		if (arginp == 1)
			exit(0);
		else if ((c = *arginp++) == '\0') {
			arginp = 1;
			c = '\n';
		}
	} else if (onelflg == 1 || read(0, &cc, 1) != 1)
		exit(0);
	else if ((c = cc) == '\n' && onelflg)
		onelflg--;
	if ((cp = accpt) != 0) {
		*cp++ = c;
		if (--acct == 0)
			acflush();
		else
			accpt = cp;
	}
	return(c);
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
		if (any(**p1, ";&\n"))
			p1++;
		else
			return(syn1(p1, p2, 1));
	}
	return(0);
}

/*
 * syn1@fl=0
 *	syn2
 *	syn2 ; syntax
 *	syn2 &
 *
 * When fl=1 syn1 handles the vagarities of &*
 */

syn1(p1, p2, fl)
char **p1, **p2;
{
	register char **p;
	register *t, *t1;
	int l;

top:
	l = 0;
	for(p=p1; p!=p2; p++)
	switch(**p) {

	case '(':
		l++;
		continue;

	case ')':
		l--;
		if (l < 0)
			error++;	/* too many ')'s */
		continue;

	case ';':
	case '\n':
		if (fl)
			continue;
	case '&':
		if (l == 0) {
			if (fl) {
				l = (*p)[1];
				if (l == '*')
					**p = ';';
				t1 = syn1(p1, p+1, 0);
				if (l == '*')
					if (t1[DTYP] == TLST) {
						t = tree(5);
						t[DTYP] = TPAR;
						t[DLEF] = 0;
						t[DRIT] = 0;
						t[DFLG] = FAND|FPRS|FINT;
						t[DSPR] = t1;
						t1 = t;
					} else {
						t = t1[DLEF];
						t[DFLG] =| FAND|FPRS|FINT;
					}
			} else {
				l = **p;
				t1 = syn2(p1, p);
				if (l == '&')
					t1[DFLG] =| FAND|FPRS|FINT;
			}
			t = tree(4);
			t[DTYP] = TLST;
			t[DFLG] = 0;
			t[DLEF] = t1;
			t[DRIT] = syntax(p+1, p2);
			return(t);
		}
	}
	if (l == 0)
		if (fl) {
			fl = 0;
			goto top;
		} else
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
		if (l == 0) {
			t = tree(4);
			t[DTYP] = TFIL;
			t[DLEF] = syn3(p1, p);
			p++;
			if (p != p2 && **p == '*') {
				t[DFLG] = FDIAG;
				if ((*p)[1] != '\0') {
					(*p)++;
					p--;
				}
			} else {
				t[DFLG] = 0;
				p--;
			}
			t[DRIT] = syn2(p+1, p2);
			return(t);
		}
	}
	return(syn3(p1, p2));
}

/*
 * syn3
 *	( syn1@1 ) [ < in  ] [ > out ]
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
	if (**p2 == ')')
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
		if (l == 0) {
			if (lp != 0)
				error++;
			lp = p+1;
		}
		l++;
		continue;

	case ')':
		l--;
		if (l == 0)
			rp = p;
		continue;

	case '>':
		p++;
		if (p!=p2 && **p=='>')
			flg =| FCAT; else
			p--;
		p++;
		if (p != p2 && **p == '*') {
			flg =| FDIAG;
			if ((*p)[1] != '\0') {
				(*p)++;
				p--;
			}
		} else
			p--;

	case '<':
		if (l == 0) {
			p++;
			if (p == p2) {
				error++;
				p--;
			}
			if (any(**p, "<>("))
				error++;
			if (c == '<') {
				if (i != 0)
					error++;
				i = *p;
			} else {
				if (o != 0)
					error++;
				o = *p;
			}
		}
		continue;

	default:
		if (l == 0)
			p1[n++] = *p;
	}
	if (lp != 0) {
		if (n != 0)
			error++;
		t = tree(5);
		t[DTYP] = TPAR;
		t[DSPR] = syn1(lp, rp, 1);
	} else {
		if (n == 0)
			error++;
		p1[n++] = 0;
		t = tree(n+5);
		t[DTYP] = TCOM;
		for(l=0; l<n; l++)
			t[l+DCOM] = p1[l];
	}
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

	if (any(c, "[?*"))
		gflg = 1;
	return(c);
}

trim(c)
int c;
{

	return(c&0177);
}

endwait()
{
	signal(INTR, 1);
	prs("wait: interrupted\n");
	reset();		/* URK! */
}

execute(t, pf1, pf2)
int *t, *pf1, *pf2;
{
	int i, f, pv[2];
	register *t1;
	register char *cp1, *cp2;
	char *scp;
	extern errno;

	if (t != 0)
	switch(t[DTYP]) {

	case TCOM:
		cp1 = t[DCOM];
		if (uid == 0) {
			if (equal(cp1, "gimme")) {
				nice(-20);
				return;
			}
			if (equal(cp1, "normal")) {
				nice(0);
				return;
			}
		}
		if (equal(cp1, "chdir") || equal(cp1, "cd")) {
			if (t[DCOM+1] != 0) {
				scan(t, &trim);
				if (chdir(t[DCOM+1]) < 0) {
					prs(cp1);
					err(": bad directory");
				}
			} else {
				prs(cp1);
				err(": arg count");
			}
			return;
		}
		if (equal(cp1, "shift")) {
			if (dolc < 1) {
				prs("shift: no args\n");
				return;
			}
			dolv[1] = dolv[0];
			dolv++;
			dolc--;
			return;
		}
		if (equal(cp1, "lib")) {
			if (cp2 = *(t1 = &t[DCOM+1])) {
				if (!(cp1 = lib))
					cp1 = lib = sbrk(LIBSIZ+1);
				i = LIBSIZ;
				for(;;) {
					while(*cp1++ = (*cp2++ & 0177))
						if (--i == 0) {
							err("lib: path too long");
							lib[0] = 0;
							return;
						}
					if (!(cp2 = *++t1))
						return;
					*(cp1-1) = ' ';
				}
			} else {
				prs(lib);
				prs("\n");
			}
			return;
		}
		if (equal(cp1, "wait")) {
			if (setintr && *promp) {
				signal(INTR, endwait);
				pwait(-1);
				signal(INTR, 1);
			} else
				pwait(-1);
			return;
		}
		if (equal(cp1, ":"))
			return;

	case TPAR:
		t1 = t;
		f = t1[DFLG];
		i = 0;
		if ((f&FPAR) == 0)
			i = fork();
		if (i == -1) {
			err("try again");
			return;
		}
		if (i != 0) {
			if ((f&FPIN) != 0) {
				close(pf1[0]);
				close(pf1[1]);
			}
			if ((f&FPRS) != 0) {
				prn(i);
				prs("\n");
			}
			if ((f&FAND) != 0)
				return;
			if ((f&FPOU) == 0)
				pwait(i);
			return;
		}
		if ((cp1 = t1[DLEF]) != 0) {
			close(0);
			if (open(cp1, 0) < 0) {
				prs(cp1);
				err(": cannot open");
				exit(1);
			}
		}
		if ((cp2 = t1[DRIT]) != 0) {
			close(1);
			if ((f&FCAT) != 0 && open(cp2, 1) >= 0)
				seek(1, 0, 2);
			else if (creat(cp2, 0644) < 0) {
				prs(t1[DRIT]);
				err(": cannot create");
				exit(1);
			}
		}
		if ((f&FPIN) != 0) {
			close(0);
			dup(pf1[0]);
			close(pf1[0]);
			close(pf1[1]);
		}
		if ((f&FPOU) != 0) {
			close(1);
			dup(pf2[1]);
			close(pf2[0]);
			close(pf2[1]);
		}
		if (f&FDIAG) {
			close(2);
			dup(1);
		}
		if ((f&FINT)!=0 && t1[DLEF]==0 && (f&FPIN)==0) {
			close(0);
			open("/dev/null", 0);
		}
		if ((f&FINT) == 0 && setintr) {
			signal(INTR, 0);
			signal(QUIT, 0);
		}
		if (t1[DTYP] == TPAR) {
			if (t1 = t1[DSPR])
				t1[DFLG] =| f&FINT;
			execute(t1);
			exit(1);
		}
		close(acctf);
		gflg = 0;
		scan(t1+1, &tglob);
		if (gflg) {
			t1 = &t1[DSPR-1];
			*t1++ = glob;
			if ((*t1 = lib) == 0)
				*t1 = &"";
			t1--;
			execv(*t1, t1);
			prs("glob: cannot execute\n");
			exit(1);
		}
		scan(t1, &trim);
		cp1 = t1[DCOM];
		while(*cp1 && *cp1 != '/')
			cp1++;
		if (*cp1 || uid)
			texec(t1[DCOM], t1);
		if (!*cp1) {
			if (cp2 = lib)
				for(;;) {
					while (*cp2 == ' ')
						cp2++;
					if (!*cp2)
						break;
					cp1 = linep;
					while(*cp2)
						if (*cp2 == ' ')
							break;
						else
							*cp1++ = *cp2++;
					scp = cp2;
					*cp1++ = '/';
					cp2 = t[DCOM];
					while(*cp1++ = *cp2++)
						continue;
					texec(linep, t1);
					cp2 = scp;
				}
			cp1 = linep;
			cp2 = "/usr/bin/";
			while(*cp1 = *cp2++)
				cp1++;
			cp2 = t1[DCOM];
			while(*cp1++ = *cp2++);
			texec(linep+4, t1);
			texec(linep, t1);
		}
		prs(t1[DCOM]);
		err(": not found");
		exit(1);

	case TFIL:
		f = t[DFLG];
		pipe(pv);
		t1 = t[DLEF];
		t1[DFLG] =| FPOU | (f&(FPIN|FINT|FPRS|FDIAG));
		execute(t1, pf1, pv);
		t1 = t[DRIT];
		t1[DFLG] =| FPIN | (f&(FPOU|FINT|FAND|FPRS));
		execute(t1, pv, pf2);
		return;

	case TLST:
		f = t[DFLG]&FINT;
		if (t1 = t[DLEF])
			t1[DFLG] =| f;
		execute(t1);
		if (t1 = t[DRIT])
			t1[DFLG] =| f;
		execute(t1);
		return;

	}
}

texec(f, at)
int *at;
{
	extern errno;
	register int *t, *t1;
	register i;
	int w;

	t = at;
	execv(f, (t1 = &t[DCOM]));
	if (errno == ENOEXEC) {
		*t1 = f;
		*--t1 = shell;
		i = open(f, 0);
		if (i >= 0) {
			if (read(i, &w, 2) == 2 && w == PCXN)
				*t1 = px;
			close(i);
		}
		execv(*t1, t1);
		prs("No ");
		prs(*t1);
		err("!!\n");
		exit(1);
	} else if (errno==ENOMEM) {
		prs(*t1);
		err(": too large");
		exit(1);
	}
}

err(s)
char *s;
{

	prs(s);
	prs("\n");
	if (*promp == '\0') {
		seek(0, 0, 2);
		exit();
	}
}

prs(as)
char *as;
{
	register char *s;

	if (s = as) {
		while(*s)
			s++;
		write(2, as, s-as);
	}
}

prn(n)
int n;
{
	register a;

	if (a = n/10)
		prn(a);
	n = n % 10 + '0';
	write(2, &n, 1);
}

getn(acp)
char *acp;
{
	register i;
	register char c, *cp;

	cp = acp;
	i = 0;
	while ((c = *cp++) != '\0')
		if (c >= '0' && c <= '9')
			i = 10 * i + c - '0';
	return(i);
}

any(c, as)
int c;
char *as;
{
	register char *s;

	s = as;
	while(*s)
		if (*s++ == c)
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
		if (*s2++ == '\0')
			return(1);
	return(0);
}

pwait(ai)
int ai;
{
	register p, e, i;
	int s;

	if ((i = ai) != 0)
		do {
			p = wait(&s);
			if (p == -1)
				break;
			e = s & 0177;
			if (mesg[e] != 0) {
				if (p != i) {
					prn(p);
					prs(": ");
				}
				prs(mesg[e]);
				if (s & 0200)
					prs(" -- Core dumped");
			}
			if (e != 0)
				err("");
		} while (i != p);
}
