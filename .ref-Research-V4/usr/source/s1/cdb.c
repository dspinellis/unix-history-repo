/*

	C debugger

*/

int	fcore;
int	fsym;
int	symoff;
char	*lp;
int	errflg;
int	symlen;
int	symct;
char	symbol[8];
int	symflg;
int	symval;
char	ssymbol[8];
int	ssymflg;
int	ssymval;
char	line[128];
int	regbuf[512];
char	**uuusp;
char	*usize;
char	**uregs;
char	*textsize;
int	pc -2;
int	r5 -9;
int	dot;
int	tdot;
int	dotinc 2;
int	lastcom '/';


main(argc, argv)
char **argv;
{
	char *corfil, *symfil;

	if (argc<2)
		corfil = "core";
	else
		corfil = argv[1];
	if (argc<3)
		symfil = "a.out";
	else
		symfil = argv[2];
	if ((fcore = open(corfil, 0)) < 0) {
		printf("%s not found\n", corfil);
		return;
	}
	if ((fsym = open(symfil, 0)) < 0) {
		printf("%s not found\n", symfil);
		return;
	}
	read(fsym, regbuf, 020);
	if (regbuf[0]==0410)
		textsize = regbuf[1];
	else if (regbuf[0] != 0407) {	/* magic */
		printf("Bad format: %s\n", symfil);
		return;
	}
	symoff = regbuf[1] + regbuf[2];
	symlen = regbuf[4];
	if (regbuf[7] != 1)
		symoff =<< 1;
	symoff =+ 020;
	fstat(fcore, regbuf);
	usize = regbuf[5]-1024;
	read(fcore, regbuf, 1024);
	uuusp = &regbuf[512-6];
	uregs = &regbuf[512];
	setexit();
loop:
	if (errflg) {
		printf("?\n");
		errflg = 0;
	}
	lp = line;
	while ((*lp = getchar()) != '\n')
		if (*lp++ == '\0')
			return;
	lp = line;
	command();
	goto loop;
}

command()
{
	int adrflg, n;

	adrflg = expr();
	if (errflg)
		return;
	n = getcnt();
	if (*lp == '\n') {
		if (!adrflg)
			dot =+ dotinc;
	} else
		lastcom = *lp++;
	if (*lp != '\n') {
		errflg++;
		return;
	}
	if (adrflg)
		dot = tdot;
	while(n) {
		scommand();
		if (--n)
			dot =+ dotinc;
	}
}

scommand()
{
	int w, c;

	switch(lastcom) {

	case '/':
		printf("%o\n", cget(dot));
		dotinc = 2;
		return;

	case '=':
		printf("%o\n", dot);
		return;

	case '\'':
		printc(cget(dot) & 0377);
		putchar('\n');
		dotinc = 1;
		return;

	case '"':
		w = cget(dot);
		while(c = cget(w++)&0377)
			printc(c);
		putchar('\n');
		return;

	case '&':
		vallook(cget(dot));
		if (errflg)
			reset();
		for (c=0; c<8; c++)
			printf("%c", ssymbol[c]);
		return;

	case '$':
		printtrace();
		return;

	}
}

getcnt()
{
	int t1, t2;

	if (*lp != ',')
		return(1);
	lp++;
	t1 = tdot;
	if (expr() == 0) {
		tdot = t1;
		return(1);
	}
	t2 = tdot;
	tdot = t1;
	return(t2);
}

cget(n)
{
	int w;

	w = get(n);
	if (errflg)
		reset();
	return(w);
}

printc(c)
{
	if (c<' ' || c>'}')
		printf("\\%o", c);
	else
		printf("%c", c);
}

expr()
{
	char tsym[10];
	int i, t1, t2, donef, adrflg, lastop, b;

	tdot = 0;
	adrflg = 0;
	lastop = '+';
	ssymval = 0;
	donef = 0;
loop:
	if (*lp >= 'a' && *lp <= 'z' || *lp=='_') {
		i = 0;
		tsym[i++] = '_';
		adrflg++;
		while(*lp>='a'&&*lp<='z' || *lp>='0'&&*lp<='9' || *lp=='_') {
			if (i < 8)
				tsym[i++] = *lp;
			lp++;
		}
		while (i<8)
			tsym[i++] = '\0';
		if (symlook(tsym) == 0) {
			errflg++;
			reset();
		}
		goto loop;
	}
	if (*lp>='0' && *lp<='9') {
		adrflg++;
		ssymval = 0;
		if (*lp == '0')
			b = 8;
		else
			b = 10;
		while (*lp>='0' && *lp<='9') {
			ssymval =* b;
			ssymval =+ *lp++ -'0';
		}
		goto loop;
	}
	switch (*lp) {

	default:
		donef++;

	case '+':
	case '-':
		switch(lastop) {

		case '+':
			tdot =+ ssymval;
			goto op;

		case '-':
			tdot =- ssymval;

		op:
			if (donef)
				return(adrflg);
			else
				lastop = *lp++;
		}
		goto loop;

	case ' ':
	case '\t':
		lp++;
		goto loop;

	case '[':
		lp++;
		t1 = ssymval;
		t2 = tdot;
		if (expr() == 0)
			tdot = 0;
		ssymval = get(t1 + (tdot<<1));
		if (errflg)
			reset();
		tdot = t2;
		if (*lp == ']')
			lp++;
		goto loop;
	}
}

printtrace()
{
	int tpc, tr5, narg, argp, i;

	tpc = uregs[pc];
	tr5 = uregs[r5];
	if (symlook("savr5"))
		if (narg = get(ssymval))
			tr5 = narg;
	while (errflg == 0) {
		narg = findroutine(tpc, tr5);
		for (i=0; i<8; i++)
			printf("%c", ssymbol[i]);
		printf("(");
		if (--narg >= 0)
			printf("%o", get(tr5+4));
		argp = tr5+4;
		while(--narg >= 0)
			printf(",%o", get(argp =+ 2));
		printf(")\n");
		tpc = get(tr5+2);
		if ((tr5 = get(tr5)) == 0)
			break;
	}
}

findroutine(rpc, rr5)
{
	int callpt, inst, narg;

	callpt = get(rr5+2);
	if ((inst=get(callpt-4)) == 04737)	/* jsr pc,*$... */
		narg = 1;
	else if ((inst&~077)==04700)		/* jsr pc,... */
		narg = 0;
	else {
		errflg++;
		return(0);
	}
	vallook((inst==04767?callpt:0) + get(callpt-2));
	inst = get(callpt);
	if (inst == 05726)		/* tst (sp)+ */
		return(narg+1);
	if (inst == 022626)		/* cmp (sp)+,(sp)+ */
		return(narg+2);
	if (inst == 062706)		/* add $n,sp */
		return(narg+get(callpt+2)/2);
	return(narg);
}

symlook(symstr)
char *symstr;
{
	symset();
	while(symget()) {
		if (eqstr(symbol, symstr)) {
			savsym();
			return(1);
		}
	}
	return(0);
}

eqstr(as1, as2)
int *as1, *as2;
{
	register int *s1, *s2, *es1;

	s1 = as1;
	s2 = as2;
	for (es1 = s1+4; s1 < es1; )
		if (*s1++ != *s2++)
			return(0);
	return(1);
}

vallook(value)
{
	symset();
	while(symget())
		if (symval == value && (symflg&037) == 2) {
			savsym('_');
			return;
		}
	errflg++;
}

get(addr)
char *addr;
{
	int w;

	w = 0;
	if (addr < textsize) {
		seek(fsym, addr+020, 0);
		if (read(fsym, &w, 2) != 2)
			errflg++;
		return(w);
	}
	if (addr >= *uuusp)
		addr =+ usize;
	else
		addr =- (textsize+017777) & ~017777;
	seek(fcore, addr+1024, 0);
	if (read(fcore, &w, 2) < 2)
		errflg++;
	return(w);
}

symset()
{
	symct = symlen;
	seek(fsym, symoff, 0);
}

symget()
{
	if ((symct =- 12) < 0)
		return(0);
	return(read(fsym, symbol, 12) == 12);
}

savsym(skip)
{
	int ch;
	char *p, *q;

	p = symbol;
	q = ssymbol;
	while (p<symbol+8 && (ch = *p++)) {
		if (ch == skip)
			continue;
		*q++ = ch;
	}
	while (q < ssymbol+8)
		*q++ = '\0';
	ssymflg = symflg;
	ssymval = symval;
}

