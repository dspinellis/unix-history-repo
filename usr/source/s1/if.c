/* if command */

int	ap;
int	ac;
char	**av;

main(argc, argv)
char *argv[];
{

	argv[argc] = 0;
	ac = argc; av = argv; ap = 1;
	if (argc<2) return;
	if (exp()) 
		if(doex(0)) {
			write(1, "no command\n", 11);
			seek(0, 0, 2);
		}
}

char *nxtarg() {

	if (ap>ac) return(0*ap++);
	return(av[ap++]);
}

exp(s) {
	int p1;

	p1 = e1();
	if (eq(nxtarg(), "-o")) return(p1 | exp());
	ap--;
	return(p1);
}

e1() {
	int p1;

	p1 = e2();
	if (eq(nxtarg(), "-a")) return (p1 & e1());
	ap--;
	return(p1);
}

e2() {
	if (eq(nxtarg(), "!"))
		return(!e3());
	ap--;
	return(e3());
}

e3() {
	int p1, ccode;
	register char *a;

	ccode = 0;
	if ((a=nxtarg())==0) goto err;
	if(eq(a, "(")) {
		p1 = exp();
		if(!eq(nxtarg(), ")")) goto err;
		return(p1);
	}

	if(eq(a, "-r"))
		return(tio(nxtarg(), 0));

	if(eq(a, "-w"))
		return(tio(nxtarg(), 1));

	if(eq(a, "-c"))
		return(tcreat(nxtarg()));

	if(eq(a, "{")) { /* execute a command for exit code */
		if(fork()) /*parent*/ wait(&ccode);
		else { /*child*/
			doex(1);
			goto err;
		}
		while((a=nxtarg()) && (!eq(a,"}")));
		return(ccode? 0 : 1);
	}

	p1 = nxtarg();
	if (p1==0) goto err;
	if(eq(p1, "="))
		return(eq(a, nxtarg()));

	if(eq(p1, "!="))
		return(!eq(a, nxtarg()));
err:
	write(1, "if error\n", 9);
	exit(9);
}

tio(a, f) {

	a = open(a, f);
	if (a>=0) {
		close(a);
		return(1);
	}
	return(0);
}

tcreat(a) {
	return(1);
}

eq(a, b)
char *a, *b;
{
	register int i;

	i = 0;
l:
	if(a[i] != b[i])
		return(0);
	if(a[i++] == '\0')
		return(1);
	goto l;
}

doex(earg) {

	register int np, i, c;
	char *nargv[50], *ncom, *na;

	np = 0;
	while (na=nxtarg()) {
		if(earg && eq(na,"}")) break;
		nargv[np++] = na;
	}
	if(earg && (!eq(na, "}"))) return(9);
	nargv[np] = 0;
	if (np==0) return(earg);
	execv(nargv[0], nargv, np);
	i = 0;
	ncom = "/usr/bin/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
	while(c=nargv[0][i])  {
		ncom[9+i++] = c;
	}
	ncom[9+i] = '\0';
	execv(ncom+4, nargv, np);
	execv(ncom, nargv, np);
	return(1);
}
