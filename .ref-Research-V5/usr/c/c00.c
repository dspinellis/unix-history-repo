#
/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

#include "c0h.c"

int	isn 1;
int	peeksym -1;
int	line 1;
int	debug 0;
int	dimp	0;

struct kwtab {
	char	*kwname;
	int	kwval;
} kwtab[]
{
	"int",		INT,
	"char",		CHAR,
	"float",	FLOAT,
	"double",	DOUBLE,
	"struct",	STRUCT,
	"auto",		AUTO,
	"extern",	EXTERN,
	"static",	STATIC,
	"register",	REG,
	"goto",		GOTO,
	"return",	RETURN,
	"if",		IF,
	"while",	WHILE,
	"else",		ELSE,
	"switch",	SWITCH,
	"case",		CASE,
	"break",	BREAK,
	"continue",	CONTIN,
	"do",		DO,
	"default",	DEFAULT,
	"for",		FOR,
	"sizeof",	SIZEOF,
	0,		0,
};

main(argc, argv)
char *argv[];
{
	extern fin, fout;
	int treespace[ossiz];
	register char *sp, *np;
	register struct kwtab *ip;

	if(argc<4) {
		error("Arg count");
		exit(1);
	}
	if((fin=open(argv[1],0))<0) {
		error("Can't find %s", argv[1]);
		exit(1);
	}
	if (fcreat(argv[2], &fout)<0 || fcreat(argv[3], binbuf)<0) {
		error("Can't create temp");
		exit(1);
	}
	if (argc>4)
		proflg++;
	xdflg++;
	for (ip=kwtab; (np = ip->kwname); ip++) {
		for (sp = symbuf; sp<symbuf+ncps;)
			if ((*sp++ = *np++) == '\0')
				np--;
		np = lookup();
		np->hclass = KEYWC;
		np->htype = ip->kwval;
	}
	xdflg = 0;
	treebase = treespace+10;
	putw(treebase, binbuf);
	while(!eof) {
		extdef();
		blkend();
	}
	flush();
	fflush(binbuf);
	exit(nerror!=0);
}

struct hshtab *lookup()
{
	int ihash;
	register struct hshtab *rp;
	register char *sp, *np;

	ihash = 0;
	for (sp=symbuf; sp<symbuf+ncps;)
		ihash =+ *sp++;
	rp = &hshtab[ihash%hshsiz];
	while (*(np = rp->name)) {
		for (sp=symbuf; sp<symbuf+ncps;)
			if (*np++ != *sp++)
				goto no;
		return(rp);
	no:
		if (++rp >= &hshtab[hshsiz])
			rp = hshtab;
	}
	if(++hshused >= hshsiz) {
		error("Symbol table overflow");
		exit(1);
	}
	rp->hclass = 0;
	rp->htype = 0;
	rp->hoffset = 0;
	rp->dimp = 0;
	rp->hflag = xdflg;
	sp = symbuf;
	for (np=rp->name; sp<symbuf+ncps;)
		*np++ = *sp++;
	return(rp);
}

symbol() {
	register c;
	register char *sp;

	if (peeksym>=0) {
		c = peeksym;
		peeksym = -1;
		if (c==NAME)
			mosflg = 0;
		return(c);
	}
	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		if (eof)
			return(EOF);
		else
			c = getchar();
loop:
	switch(ctab[c]) {

	case INSERT:		/* ignore newlines */
		inhdr = 1;
		c = getchar();
		goto loop;

	case NEWLN:
		if (!inhdr)
			line++;
		inhdr = 0;

	case SPACE:
		c = getchar();
		goto loop;

	case EOF:
		eof++;
		return(0);

	case PLUS:
		return(subseq(c,PLUS,INCBEF));

	case MINUS:
		return(subseq(c,subseq('>',MINUS,ARROW),DECBEF));

	case ASSIGN:
		if (subseq(' ',0,1)) return(ASSIGN);
		c = symbol();
		if (c>=PLUS && c<=EXOR) {
			if (peekc==0)
				peekc = getchar();
			if (ctab[peekc] != SPACE 
			 && (c==MINUS || c==AND || c==TIMES)) {
				error("Warning: assignment operator assumed");
				nerror--;
			}
			return(c+30);
		}
		if (c==ASSIGN)
			return(EQUAL);
		peeksym = c;
		return(ASSIGN);

	case LESS:
		if (subseq(c,0,1)) return(LSHIFT);
		return(subseq('=',LESS,LESSEQ));

	case GREAT:
		if (subseq(c,0,1)) return(RSHIFT);
		return(subseq('=',GREAT,GREATEQ));

	case EXCLA:
		return(subseq('=',EXCLA,NEQUAL));

	case DIVIDE:
		if (subseq('*',1,0))
			return(DIVIDE);
com:
		c = getchar();
com1:
		switch(c) {
		case '\0':
			eof++;
			error("Nonterminated comment");
			return(0);
		case '\n':
			if (!inhdr)
				line++;
			inhdr = 0;
			goto com;
		case 001:		/* SOH, insert marker */
			inhdr++;
		default:
			goto com;
		case '*':
			c = getchar();
			if (c!='/')
				goto com1;
		}
		c = getchar();
		goto loop;

	case PERIOD:
	case DIGIT:
		peekc = c;
		if ((c=getnum(c=='0'?8:10)) == FCON)
			cval = isn++;
		return(c);

	case DQUOTE:
		return(getstr());

	case SQUOTE:
		return(getcc());

	case LETTER:
		sp = symbuf;
		if (mosflg) {
			*sp++ = '.';
			mosflg = 0;
		}
		while(ctab[c]==LETTER || ctab[c]==DIGIT) {
			if (sp<symbuf+ncps) *sp++ = c;
			c = getchar();
		}
		while(sp<symbuf+ncps)
			*sp++ = '\0';
		peekc = c;
		csym = lookup();
		if (csym->hclass==KEYWC) {
			if (csym->htype==SIZEOF)
				return(SIZEOF);
			cval = csym->htype;
			return(KEYW);
		}
		return(NAME);

	case AND:
		return(subseq('&', AND, LOGAND));

	case OR:
		return(subseq('|', OR, LOGOR));

	case UNKN:
		error("Unknown character");
		c = getchar();
		goto loop;

	}
	return(ctab[c]);
}

subseq(c,a,b) {
	if (!peekc)
		peekc = getchar();
	if (peekc != c)
		return(a);
	peekc = 0;
	return(b);
}

getstr() {
	register int c;
	register char *t, *d;

	nchstr = 1;
	t = ".text";
	d = ".data";
	printf("%s\nL%d:.byte ", (strflg?t:d), cval=isn++);
	while((c=mapch('"')) >= 0) {
		printf("%o,", c);
		nchstr++;
	}
	printf("0\n.even\n%s\n", (strflg?d:t));
	return(STRING);
}

getcc()
{
	register int c, cc;
	register char *ccp;

	cval = 0;
	ccp = &cval;
	cc = 0;
	while((c=mapch('\'')) >= 0)
		if(cc++ < ncpw)
			*ccp++ = c;
	if(cc>ncpw)
		error("Long character constant");
	return(CON);
}

mapch(ac)
{
	register int a, c, n;
	static mpeek;

	c = ac;
	if (mpeek) {
		a = mpeek;
		mpeek = 0;
	} else
		a = getchar();
loop:
	if (a==c)
		return(-1);
	switch(a) {

	case '\n':
	case '\0':
		error("Nonterminated string");
		peekc = a;
		return(-1);

	case '\\':
		switch (a=getchar()) {

		case 't':
			return('\t');

		case 'n':
			return('\n');

		case 'b':
			return('\b');

		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
			n = 0;
			c = 0;
			while (++c<=3 && '0'<=a && a<='7') {
				n =<< 3;
				n =+ a-'0';
				a = getchar();
			}
			mpeek = a;
			return(n);

		case 'r':
			return('\r');

		case '\n':
			if (!inhdr)
				line++;
			inhdr = 0;
			a = getchar();
			goto loop;
		}
	}
	return(a);
}

tree()
{
#define	SEOF	200
#define	SSIZE	20
	int *op, opst[SSIZE], *pp, prst[SSIZE];
	register int andflg, o;
	register struct hshtab *cs;
	int p, ps, os, *np;

	osleft = ossiz;
	space = treebase;
	op = opst;
	pp = prst;
	cp = cmst;
	*op = SEOF;
	*pp = 06;
	andflg = 0;

advanc:
	switch (o=symbol()) {

	case NAME:
		cs = csym;
		if (cs->hclass==0 && cs->htype==0)
			if(nextchar()=='(') {
				/* set function */
				cs->hclass = EXTERN;
				cs->htype = FUNC;
			} else if (initflg)
				cs->hclass = EXTERN;
			else {
				/* set label */
				cs->htype = ARRAY;
				if (cs->hoffset==0)
					cs->hoffset = isn++;
			}
		*cp++ = block(2,NAME,cs->htype,cs->hdimp,
		    cs->hclass,0);
		if (cs->hclass==EXTERN) {
			np = cs->name;
			for (o=0; o<4; o++) {
				pblock(*np);
				if (((*np++)&~0177) == 0)
					break;
			}
		} else
			pblock(cs->hoffset);
		goto tand;

	case FCON:
		if (!initflg)
			printf(".data\nL%d:%o;%o;%o;%o\n.text\n",cval,fcval);

	case CON:
	case SFCON:
		*cp++ = block(1,o,(o==CON?INT:DOUBLE),0,cval);
		goto tand;

	/* fake a static char array */
	case STRING:
		*cp++ = block(3, NAME, ARRAY+CHAR,0,STATIC,0,cval);

tand:
		if(cp>=cmst+cmsiz) {
			error("Expression overflow");
			exit(1);
		}
		if (andflg)
			goto syntax;
		andflg = 1;
		goto advanc;

	case INCBEF:
	case DECBEF:
		if (andflg)
			o =+ 2;
		goto oponst;

	case COMPL:
	case EXCLA:
	case SIZEOF:
		if (andflg)
			goto syntax;
		goto oponst;

	case MINUS:
		if (!andflg)  {
			if ((peeksym=symbol())==FCON) {
				fcval = - fcval;
				goto advanc;
			}
			if (peeksym==SFCON) {
				fcval = - fcval;
				cval =^ 0100000;
				goto advanc;
			}
			o = NEG;
		}
		andflg = 0;
		goto oponst;

	case AND:
	case TIMES:
		if (andflg)
			andflg = 0; else
			if(o==AND)
				o = AMPER;
			else
				o = STAR;
		goto oponst;

	case LPARN:
		if (andflg) {
			o = symbol();
			if (o==RPARN)
				o = MCALL;
			else {
				peeksym = o;
				o = CALL;
				andflg = 0;
			}
		}
		goto oponst;

	case RBRACK:
	case RPARN:
		if (!andflg)
			goto syntax;
		goto oponst;

	case DOT:
	case ARROW:
		mosflg++;
		break;

	}
	/* binaries */
	if (!andflg)
		goto syntax;
	andflg = 0;

oponst:
	p = (opdope[o]>>9) & 077;
	if ((o==COMMA || o==COLON) && initflg)
		p = 05;
opon1:
	ps = *pp;
	if (p>ps || p==ps && (opdope[o]&RASSOC)!=0) {
		switch (o) {

		case INCAFT:
		case DECAFT:
			p = 37;
			break;
		case LPARN:
		case LBRACK:
		case CALL:
			p = 04;
		}
		if (op >= &opst[SSIZE-1]) {
			error("expression overflow");
			exit(1);
		}
		*++op = o;
		*++pp = p;
		goto advanc;
	}
	--pp;
	switch (os = *op--) {

	case SEOF:
		peeksym = o;
		build(0);		/* flush conversions */
		return(*--cp);

	case CALL:
		if (o!=RPARN)
			goto syntax;
		build(os);
		goto advanc;

	case MCALL:
		*cp++ = block(0,0,0,0);	/* 0 arg call */
		os = CALL;
		goto fbuild;

	case LPARN:
		if (o!=RPARN)
			goto syntax;
		goto advanc;

	case LBRACK:
		if (o!=RBRACK)
			goto syntax;
		build(LBRACK);
		goto advanc;
	}
fbuild:
	build(os);
	goto opon1;

syntax:
	error("Expression syntax");
	errflush(o);
	return(0);
}

declare(askw, tkw, offset, elsize)
{
	register int o;
	register int skw;

	skw = askw;
	do {
		offset =+ decl1(skw, tkw, offset, elsize);
		if (xdflg && skw!=MOS)
			return;
	} while ((o=symbol()) == COMMA);
	if (o==SEMI || o==RPARN && skw==ARG1)
		return(offset);
	decsyn(o);
}

decl1(askw, tkw, offset, elsize)
{
	int t1, chkoff;
	register int type, skw;
	register struct hshtab *dsym;

	skw = askw;
	chkoff = 0;
	mosflg = skw==MOS;
	if ((peeksym=symbol())==SEMI || peeksym==RPARN)
		return(0);
	if ((t1=getype()) < 0)
		goto syntax;
	type = 0;
	do
		type = type<<2 | (t1 & 030);
	while (((t1=>>2) & 030)!=0);
	type =| tkw;
	dsym = defsym;
	if (!(dsym->hclass==0
	   || (skw==ARG && dsym->hclass==ARG1)
	   || (skw==EXTERN && dsym->hclass==EXTERN && dsym->htype==type)))
		if (skw==MOS && dsym->hclass==MOS && dsym->htype==type)
			chkoff = 1;
		else {
			redec();
			goto syntax;
		}
	dsym->htype = type;
	if (skw)
		dsym->hclass = skw;
	if (skw==ARG1) {
		if (paraml==0)
			paraml = dsym;
		else
			parame->hoffset = dsym;
		parame = dsym;
	}
	if (elsize && ((type&07)==RSTRUCT || (type&07)==STRUCT)) {
		dsym->lenp = dimp;
		chkdim();
		dimtab[dimp++] = elsize;
	}
	elsize = 0;
	if (skw==MOS) {
		elsize = length(dsym);
		if ((offset&1)!=0 && elsize!=1) {
			offset++;
			elsize++;
		}
		if (chkoff && dsym->hoffset != offset)
			redec();
		dsym->hoffset = offset;
	}
	if ((dsym->htype&030)==FUNC) {
		if (dsym->hclass!=EXTERN && dsym->hclass!=AUTO)
			error("Bad function");
		dsym->hclass = EXTERN;
	}
	if (dsym->hclass==AUTO) {
		autolen =+ rlength(dsym);
		dsym->hoffset = -autolen;
	} else if (dsym->hclass==STATIC) {
		dsym->hoffset = isn;
		printf(".bss\nL%d:.=.+%o\n.text\n", isn++, rlength(dsym));
	} else if (dsym->hclass==REG) {
		if ((type&07)>CHAR && (type&030)==0
		 || (type&030)>PTR || regvar<3)
			error("Bad register %o", type);
		dsym->hoffset = --regvar;
	}
syntax:
	return(elsize);
}

getype()
{
	register int o, type;
	register struct hshtab *ds;

	switch(o=symbol()) {

	case TIMES:
		return(getype()<<2 | PTR);

	case LPARN:
		type = getype();
		if ((o=symbol()) != RPARN)
			goto syntax;
		goto getf;

	case NAME:
		defsym = ds = csym;
		type = 0;
		ds->ssp = dimp;
	getf:
		switch(o=symbol()) {

		case LPARN:
			if (xdflg) {
				xdflg = 0;
				ds = defsym;
				declare(ARG1, 0, 0, 0);
				defsym = ds;
				xdflg++;
			} else
				if ((o=symbol()) != RPARN)
					goto syntax;
			type = type<<2 | FUNC;
			goto getf;

		case LBRACK:
			if ((o=symbol()) != RBRACK) {
				peeksym = o;
				cval = conexp();
				for (o=ds->ssp&0377; o<dimp; o++)
					dimtab[o] =* cval;
				dimtab[dimp++] = cval;
				if ((o=symbol())!=RBRACK)
					goto syntax;
			} else
				dimtab[dimp++] = 1;
			type = type<<2 | ARRAY;
			goto getf;
		}
		peeksym = o;
		return(type);
	}
syntax:
	decsyn(o);
	return(-1);
}

decsyn(o)
{
	error("Declaration syntax");
	errflush(o);
}

redec()
{
	error("%.8s redeclared", defsym->name);
}

