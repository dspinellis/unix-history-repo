static char lex_c_Sccsid[] = "lex.c @(#)lex.c	1.3	10/5/82 Berkeley ";
#define lv yylval
#define v yyval

int xxpeek[2] = {0,0};

yylex()
{
	register c, rval;
	register struct tab *tp;

	if(nlexsym != -1) {	/* first token is lexical context */
		c = nlexsym;
		nlexsym = -1;
		return(c);
	}
	while(litflag > 0) {	/* comment */
		c = *iline++;
		if(c == '\n') {
			nlexsym = 0;
			return(eol);
		}
	}
	if(xxpeek[0] != 0){
		lv.charval = xxpeek[0];		/* may want charptr here */
		xxpeek[0] = 0;
		return(xxpeek[1]);
	}
	do
		c = *iline++;
	while(c == ' ');
	if(c == '\n') {
		nlexsym = 0;
		return(eol);
	}
	if(alpha(c))
		return(getnam(c));
	if(digit(c) || c == '`' ||	/* '`' was '"' */
		    (c=='.' && digit(*iline)))
			return(getnum(c));
		c &= 0377;
	rval = unk;
	for(tp = tab; tp->input; tp++)
		if(tp->input == c) {
			lv.charval = tp->lexval;
			rval = tp->retval;
			break;
		}
	if(lv.charval == QUAD)
		return(getquad());
	if(lv.charval == ISP){
		lv.charval = ISP2;
		xxpeek[0] = ISP1;
		xxpeek[1] = m;
		return(d);
	}
	if(lv.charval == PSI){
		lv.charval = PSI2;
		xxpeek[0] = PSI1;
		xxpeek[1] = m;
		return(d);
	}
	return(rval);
}

getquad()
{
	register char c, *p1;
	register struct qtab *p2;
	char qbuf[10];

	p1 = qbuf;
	while(alpha(*iline))
		if (p1 < qbuf + sizeof qbuf)
			*p1++ = *iline++;
		else
			iline++;
	*p1++ = 0;
	if(*qbuf == 0)
		return(Quad);		/* ordinary quad */
	for(p2 = qtab; p2->qname; p2++){
		if(equal(p2->qname, qbuf)){
			lv.charval = p2->qtype;
			return(p2->rtype);
		}
	}
	return(unk);
}

getnam(ic)
{
	char name[NAMS];
	register c;
	register char *cp;
	register struct nlist *np;

	c = ic;
	cp = name;
	do {
		if(cp >= &name[NAMS])
			error("var name D");
		*cp++ = c;
		c = *iline++;
	} while(alpha(c) || digit(c));
	*cp++ = 0;
	iline--;
	if(litflag == -1) {	/* commands */
		litflag = -2;
		for(c=0; comtab[c].ct_name; c++)
			if(equal(name, comtab[c].ct_name))
				break;
		immedcmd = lv.charval = comtab[c].ct_ylval;
		return(comtab[c].ct_ytype);
	}
	for(np=nlist; np->namep; np++)
	if(equal(np->namep, name)) {
		lv.charptr = (char *)np;
		switch(np->use) {

		case NF:
			if (context == lex2) sichk(np);
			return(nfun);

		case MF:
			if (context == lex2) sichk(np);
			return(mfun);

		case DF:
			if (context == lex2) sichk(np);
			return(dfun);
		}
		return(nam);
	}
	np->namep = alloc(cp-name);
	copy(CH, name, np->namep, cp-name);
	np->type = LV;
	lv.charptr = (char *)np;
	return(nam);
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
	if(c == '`') {	/* '`' was '"' */
		s++;
		c = *iline++;
	}
	while(digit(c)) {
		d1 = d1*10. + c - '0';
		c = *iline++;
	}
	if(c == '.') {
		c = *iline++;
		while(digit(c)) {
			d1 = d1*10. + c - '0';
			c = *iline++;
			n--;
		}
	}
	if(c == 'e') {
		s1 = 0;
		n1 = 0;
		c = *iline++;
		if(c == '`') {	/* '`' was '"' */
			s1++;
			c = *iline++;
		}
		while(digit(c)) {
			n1 = n1*10 + c - '0';
			c = *iline++;
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
	return(
		(c >= 'a' && c <= 'z')
		|| (c == 'F')
		|| (c >= 0243)
		|| (litflag == -2 && (
			   c == '/'
			|| c == '.'
		   ))
	);
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
int     ilex[] =
{
	lex0, lex1, lex2, lex3, lex4, lex5, lex6
};

char *
compile(s, f)
char *s;
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
		if(iline-s > 1)
			printf("syntax error\n");
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

char *
name(np, c)
{
	register char *p, *npp;

	p = ccharp;
	npp = (char *)&np;
	*ccharp++ = c;
	*ccharp++ = *npp++;
#ifdef vax
	*ccharp++ = *npp++;
	*ccharp++ = *npp++;
#endif
	*ccharp++ = *npp;
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

/*
 * genlab -- generates label code onto label stacks.
 *
 * prologue:	AUTO-lab, CONST-linenum, NAME-lab LABEL
 *
 * epilog:	REST-lab
 */
genlab(np)
struct nlist *np;
{

	/* label prologue */

	*labcpp++ = AUTO;
	labcpp += copy(IN, &np, labcpp, 1);
	*labcpp++ = CONST;
	*labcpp++ = 1;
	labcpp += copy(DA, &lnumb, labcpp, 1);
	*labcpp++ = NAME;
	labcpp += copy(IN, &np, labcpp, 1);
	*labcpp++ = LABEL;
	*labcpp = EOF;

	/* label epilog */

	*labcpe++ = REST;
	labcpe += copy(IN, &np, labcpe, 1);
	*labcpe = EOF;
}
