#
/*

	C debugger

*/

#include "/usr/sys/param.h"
#include "/usr/sys/user.h"

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
char	**uregs	&regbuf[512];
char	*rtsize;
int	pc -2;
int	r5 -9;
int	dot;
int	tdot;
int	dotinc 2;
int	lastcom '/';
int	lastype	'o';
char	*symfil	"a.out";
char	*corfil	"core";
int	callist[50];
int	*callp	&callist[0];

main(argc, argv)
char **argv;
{
	int onintr();

	if (argc==2)
		symfil = argv[1];
	if (argc>2) {
		corfil = argv[1];
		symfil = argv[2];
	}
	if ((fcore = open(corfil, 0)) < 0) {
		printf("%s not found\n", corfil);
		return;
	}
	if ((fsym = open(symfil, 0)) < 0) {
		printf("%s not found\n", symfil);
		return;
	}
	read(fsym, regbuf, 020);
	if (regbuf[0]!=0410 && regbuf[0]!=0407) {	/* magic */
		printf("Bad format: %s\n", symfil);
		return;
	}
	symoff = regbuf[1] + regbuf[2];
	symlen = regbuf[4];
	if (regbuf[7] != 1)
		symoff =<< 1;
	symoff =+ 020;
	read(fcore, regbuf, 1024);
	regbuf->u_tsize =<< 6;
	regbuf->u_dsize =<< 6;
	regbuf->u_ssize =<< 6;
	rtsize = (regbuf->u_tsize+017777) & ~017777;
	setexit();
	signal(2, onintr);
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
	register adrflg, n;

	adrflg = expr();
	if (errflg)
		return;
	n = getcnt();
	if (lastcom=='$')
		lastcom = '/';
	if (*lp == '\n') {
		if (!adrflg)
			dot =+ dotinc;
	} else
		lastcom = *lp++;
	if (*lp != '\n')
		lastype = *lp++;
	if (*lp != '\n') {
		errflg++;
		return;
	}
	if (adrflg)
		dot = tdot;
	while(n) {
		scommand(n);
		if (errflg)
			return;
		if (--n)
			dot =+ dotinc;
	}
}

scommand(n)
{
	register w, c;
	double fw;
	struct { int i[4]; };

	switch(lastcom) {

	case '/':
		w = cget(dot);
		switch(lastype) {

		case 'o':
			printf("%.1o\n", w);
			dotinc = 2;
			return;

		case 'i':
			printf("%d\n", w);
			dotinc = 2;
			return;

		case 'f':
			fw = 0.0;
			fw.i[0] = w;
			fw.i[1] = cget(dot+2);
			printf("%e\n", fw);
			dotinc = 4;
			return;

		case 'd':
			fw.i[0] = w;
			fw.i[1] = cget(dot+2);
			fw.i[2] = cget(dot+4);
			fw.i[3] = cget(dot+6);
			printf("%e\n", fw);
			dotinc = 8;
			return;
		}
		errflg++;
		return;

	case '\\':
		printf("%.1o\n", cget(dot) & 0377);
		dotinc = 1;
		return;

	case '=':
		printf("%.1o\n", dot);
		return;

	case '\'':
		printc(cget(dot) & 0377);
		if (n<=1)
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
		printf("%.8s\n", ssymbol);
		return;

	case '$':
		printtrace();
		return;
	}
	errflg++;
}

getcnt()
{
	register t1, t2;

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
	register w;

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

	case '(':
		lp++;
		adrflg++;
		t1 = tdot;
		ssymval = getarg();
		tdot = t1;
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

getarg()
{
	register level, arg, t1;

	t1 = tdot;
	expr();
	level = tdot;
	if (*lp++ != ',')
		error();
	expr();
	arg = tdot;
	if (*lp++ != ')')
		error();
	if (level >= callp-callist)
		error();
	ssymval = callist[level] - 8 - 2*arg;
	tdot = t1;
}
error()
{
	errflg++;
	reset();
}

printtrace()
{
	int tpc, tr5, narg, argp, i;

	tpc = uregs[pc];
	tr5 = uregs[r5];
	if (symlook("savr5"))
		if (narg = get(ssymval))
			tr5 = narg;
	callp = &callist[0];
	while (errflg == 0) {
		narg = findroutine(tpc, tr5);
		printf("%2d: %.8s(", callp-callist, ssymbol);
		if (--narg >= 0)
			printf("%.1o", get(tr5+4));
		argp = tr5+4;
		while(--narg >= 0)
			printf(",%.1o", get(argp =+ 2));
		printf(")\n");
		tpc = get(tr5+2);
		if (callp < &callist[50])
			*callp++ = tr5;
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

get(aaddr)
char *aaddr;
{
	int w;
	register char *addr;

	addr = aaddr;
	w = 0;
	if (addr < regbuf->u_tsize) {
		seek(fsym, addr+020, 0);
		if (read(fsym, &w, 2) != 2)
			errflg++;
		return(w);
	}
	if (addr < rtsize+regbuf->u_dsize)
		addr =- rtsize;
	else if (-addr < regbuf->u_ssize)
		addr =+ regbuf->u_dsize + regbuf->u_ssize;
	else
		errflg++;
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

onintr()
{
	putchar('\n');
	errflg++;
	reset();
}
