#define lv yylval
#define v yyval


yylex()
{
	register c;
	register struct tab *tp;
	extern TERMtype;

	if(nlexsym != -1) {
		if (TERMtype == 1) dofix();		/* convert iline */
		c = nlexsym;
		nlexsym = -1;
		return(c);

	}
	while(litflag > 0) {
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
		if(c == '\n') {
			nlexsym = 0;
			return(eol);
		}
	}
	do
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
	while(c == ' ');
	if(c == '\n') {
		nlexsym = 0;
		return(eol);
	}
	if(alpha(c))
		return(getnam(c));
	if(digit(c) || c == '@' ||
	    (c=='.' && digit(*iline)))
		return(getnum(c));
	c &= 0377;
	for(tp = tab; tp->input; tp++)
		if(tp->input == c) {
			lv = tp->lexval;
			return(tp->retval);
		}
	return(unk);
}

getnam(ic)
{
	char name[NAMS];
	register c;
	register char *cp;
	register struct nlist *np;

	cp = name;
	do {
		*cp++ = c;
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
	} while(alpha(c) || digit(c));
	*cp++ = 0;
	iline--;
	if(litflag < 0) {	/* commands */
		litflag = 0;
		for(c=0; comtab[c]; c+=3)
			if(equal(name, comtab[c]))
				break;
		lv = comtab[c+2];
		return(comtab[c+1]);
	}
	if(a_label(name)){
		return(numb);
	}
	for(np=nlist; np->namep; np++)
	if(equal(np->namep, name)) {
		lv = np;
		switch(np->use) {

		case NF:
			return(nfun);

		case MF:
			return(mfun);

		case DF:
			return(dfun);
		}
		return(nam);
	}
	np->namep = alloc(cp-name);
	copy(CH, name, np->namep, cp-name);
	np->type = LV;
	lv = np;
	return(nam);
}

a_label(x)
register char *x;
{
register struct lablist *lblthru;

	lblthru = labldefs.nextll;
	while(lblthru) {
		if(equal(lblthru->lname,x)) {
			datum = (double) lblthru->lno;
			return(1);
		}
		lblthru = lblthru->nextll;
	}
	return(0);
}

getnum(ic)
{
	double d1, d2;
	register c, n, n1;
	int s, s1;

	s = 0;
	n = 0;
	d1 = 0.;
	c = ic;
	if(c == '@') {
		s++;
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
	}
	while(digit(c)) {
		d1 = d1*10. + c - '0';
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
	}
	if(c == '.') {
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
		while(digit(c)) {
			d1 = d1*10. + c - '0';
			if (TERMtype == 0)c = (int)*iline++;
			else c = asciich();
			n--;
		}
	}
	if(c == 'e') {
		s1 = 0;
		n1 = 0;
		if (TERMtype == 0)c = (int)*iline++;
		else c = asciich();
		if(c == '@') {
			s1++;
			if (TERMtype == 0)c = (int)*iline++;
			else c = asciich();
		}
		while(digit(c)) {
			n1 = n1*10 + c - '0';
			if (TERMtype == 0)c = (int)*iline++;
			else c = asciich();
		}
		if(s1)
			n -= n1; else
			n += n1;
	}
	n1 = n;
	if(n1 < 0)
		n1 = -n1;
	d2 = 1.;
	while(n1--)
		d2 *= 10.;
	if(n < 0)
		d1 /= d2; else
		d1 *= d2;
	if(s)
		d1 = -d1;
	iline--;
	datum = d1;
	return(numb);
}

alpha(s)
{
	register c;

	c = s & 0377;
	if(c >= 'a' && c <= 'z')
		return(1);
        if(c == 'H')
		return(1);
	if(c == 'F')
		return(1);
	if(c >= 0220 && c<=0252)
		return(1);
	return(0);
}

digit(s)
{
	register c;

	c = s;
	if(c >='0' && c <= '9')
		return(1);
	return(0);
}

/*
 * s is statement
 * f is execution flag:
 *	0 compile immediate
 *	1 compile L
 *	2 function definition
 *	3 function prolog
 *	4 function epilog
 *	5 function body
 */
int	ilex[]
{
	lex0, lex1, lex2, lex3, lex4, lex5, lex6
};
compile(s, f)
{
	register char *p, *q;
	char oline[OBJS];

	iline = s;
	ccharp = oline;
	litflag = 0;
	nlexsym = ilex[f];
	context = nlexsym;
	if(yyparse()) {
		pline(s, iline-s);
		return(0);
	}
	*ccharp++ = EOF;
	p = alloc(ccharp-oline);
	iline = p;
	for(q=oline; q<ccharp;)
		*p++ = *q++;
	return(iline);
}

yyerror()
{
}

name(np, c2)
{
	register char *p;

	p = ccharp;
	*ccharp++ = c2;
	*ccharp++ = np.c[0];
	*ccharp++ = np.c[1];
	*ccharp++ = np.c[2];
	*ccharp++ = np.c[3]; 
	return(p);
}

equal(a, b)
char *a, *b;
{
	register char *c1, *c2;

	c1 = a;
	c2 = b;
	while(*c1++ == *c2)
		if(*c2++ == 0)
			return(1);
	return(0);
}

invert(a, b)
{

	flop(a, b);
	flop(b, ccharp);
	flop(a, ccharp);
}

flop(a, b)
char *a, *b;
{
	register char *a1, *a2;
	register c;

	a1 = a;
	a2 = b;
	while(a1 < a2) {
		c = *a1;
		*a1++ = *--a2;
		*a2 = c; 
	}
}
