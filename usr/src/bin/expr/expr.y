/* Yacc productions for "expr" command: */

%token OR AND ADD SUBT MULT DIV REM EQ GT GEQ LT LEQ NEQ
%token A_STRING SUBSTR LENGTH INDEX NOARG MATCH

/* operators listed below in increasing precedence: */
%left OR
%left AND
%left EQ LT GT GEQ LEQ NEQ
%left ADD SUBT
%left MULT DIV REM
%left MCH
%left MATCH
%left SUBSTR
%left LENGTH INDEX
%%

/* a single `expression' is evaluated and printed: */

expression:	expr NOARG = {
			printf("%s\n", $1);
			exit((!strcmp($1,"0")||!strcmp($1,"\0"))? 1: 0);
			}
	;


expr:	'(' expr ')' = { $$ = $2; }
	| expr OR expr   = { $$ = conj(OR, $1, $3); }
	| expr AND expr   = { $$ = conj(AND, $1, $3); }
	| expr EQ expr   = { $$ = rel(EQ, $1, $3); }
	| expr GT expr   = { $$ = rel(GT, $1, $3); }
	| expr GEQ expr   = { $$ = rel(GEQ, $1, $3); }
	| expr LT expr   = { $$ = rel(LT, $1, $3); }
	| expr LEQ expr   = { $$ = rel(LEQ, $1, $3); }
	| expr NEQ expr   = { $$ = rel(NEQ, $1, $3); }
	| expr ADD expr   = { $$ = arith(ADD, $1, $3); }
	| expr SUBT expr   = { $$ = arith(SUBT, $1, $3); }
	| expr MULT expr   = { $$ = arith(MULT, $1, $3); }
	| expr DIV expr   = { $$ = arith(DIV, $1, $3); }
	| expr REM expr   = { $$ = arith(REM, $1, $3); }
	| expr MCH expr	 = { $$ = match($1, $3); }
	| MATCH expr expr = { $$ = match($2, $3); }
	| SUBSTR expr expr expr = { $$ = substr($2, $3, $4); }
	| LENGTH expr       = { $$ = length($2); }
	| INDEX expr expr = { $$ = index($2, $3); }
	| A_STRING
	;
%%
/*	expression command */
#include <stdio.h>
#define ESIZE	256
#define error(c)	errxx(c)
#define EQL(x,y) !strcmp(x,y)
long atol();
char	**Av;
int	Ac;
int	Argi;

char Mstring[1][128];
char *malloc();
extern int nbra;

main(argc, argv) char **argv; {
	Ac = argc;
	Argi = 1;
	Av = argv;
	yyparse();
}

char *operators[] = { "|", "&", "+", "-", "*", "/", "%", ":",
	"=", "==", "<", "<=", ">", ">=", "!=",
	"match", "substr", "length", "index", "\0" };
int op[] = { OR, AND, ADD,  SUBT, MULT, DIV, REM, MCH,
	EQ, EQ, LT, LEQ, GT, GEQ, NEQ,
	MATCH, SUBSTR, LENGTH, INDEX };
yylex() {
	register char *p;
	register i;

	if(Argi >= Ac) return NOARG;

	p = Av[Argi++];

	if(*p == '(' || *p == ')')
		return (int)*p;
	for(i = 0; *operators[i]; ++i)
		if(EQL(operators[i], p))
			return op[i];

	yylval = p;
	return A_STRING;
}

char *rel(op, r1, r2) register char *r1, *r2; {
	register long i;

	if(ematch(r1, "-*[0-9]*$") && ematch(r2, "[0-9]*$"))
		i = atol(r1) - atol(r2);
	else
		i = strcmp(r1, r2);
	switch(op) {
	case EQ: i = i==0; break;
	case GT: i = i>0; break;
	case GEQ: i = i>=0; break;
	case LT: i = i<0; break;
	case LEQ: i = i<=0; break;
	case NEQ: i = i!=0; break;
	}
	return i? "1": "0";
}

char *arith(op, r1, r2) char *r1, *r2; {
	long i1, i2;
	register char *rv;

	if(!((ematch(r1, "[0-9]*$") || ematch(r1, "-[0-9]*$")) &&
	     (ematch(r2, "[0-9]*$") || ematch(r2, "-[0-9]*$"))))
		yyerror("non-numeric argument");
	i1 = atol(r1);
	i2 = atol(r2);

	switch(op) {
	case ADD: i1 = i1 + i2; break;
	case SUBT: i1 = i1 - i2; break;
	case MULT: i1 = i1 * i2; break;
	case DIV: i1 = i1 / i2; break;
	case REM: i1 = i1 % i2; break;
	}
	rv = malloc(16);
	sprintf(rv, "%D", i1);
	return rv;
}
char *conj(op, r1, r2) char *r1, *r2; {
	register char *rv;

	switch(op) {

	case OR:
		if(EQL(r1, "0")
		|| EQL(r1, ""))
			if(EQL(r2, "0")
			|| EQL(r2, ""))
				rv = "0";
			else
				rv = r2;
		else
			rv = r1;
		break;
	case AND:
		if(EQL(r1, "0")
		|| EQL(r1, ""))
			rv = "0";
		else if(EQL(r2, "0")
		|| EQL(r2, ""))
			rv = "0";
		else
			rv = r1;
		break;
	}
	return rv;
}

char *substr(v, s, w) char *v, *s, *w; {
register si, wi;
register char *res;

	si = atol(s);
	wi = atol(w);
	while(--si) if(*v) ++v;

	res = v;

	while(wi--) if(*v) ++v;

	*v = '\0';
	return res;
}

char *length(s) register char *s; {
	register i = 0;
	register char *rv;

	while(*s++) ++i;

	rv = malloc(8);
	sprintf(rv, "%d", i);
	return rv;
}

char *index(s, t) char *s, *t; {
	register i, j;
	register char *rv;

	for(i = 0; s[i] ; ++i)
		for(j = 0; t[j] ; ++j)
			if(s[i]==t[j]) {
				sprintf(rv = malloc(8), "%d", ++i);
				return rv;
			}
	return "0";
}

char *match(s, p)
{
	register char *rv;

	sprintf(rv = malloc(8), "%d", ematch(s, p));
	if(nbra) {
		rv = malloc(strlen(Mstring[0])+1);
		strcpy(rv, Mstring[0]);
	}
	return rv;
}

#define INIT	register char *sp = instring;
#define GETC()		(*sp++)
#define PEEKC()		(*sp)
#define UNGETC(c)	(--sp)
#define RETURN(c)	return
#define ERROR(c)	errxx(c)


ematch(s, p)
char *s;
register char *p;
{
	static char expbuf[ESIZE];
	char *compile();
	register num;
	extern char *braslist[], *braelist[], *loc2;

	compile(p, expbuf, &expbuf[ESIZE], 0);
	if(nbra > 1)
		yyerror("Too many '\\('s");
	if(advance(s, expbuf)) {
		if(nbra == 1) {
			p = braslist[0];
			num = braelist[0] - p;
			strncpy(Mstring[0], p, num);
			Mstring[0][num] = '\0';
		}
		return(loc2-s);
	}
	return(0);
}

errxx(c)
{
	yyerror("RE error");
}

#define	CBRA	2
#define	CCHR	4
#define	CDOT	8
#define	CCL	12
#define	CDOL	20
#define	CEOF	22
#define	CKET	24
#define	CBACK	36

#define	STAR	01
#define RNGE	03

#define	NBRA	9

#define PLACE(c)	ep[c >> 3] |= bittab[c & 07]
#define ISTHERE(c)	(ep[c >> 3] & bittab[c & 07])

char	*braslist[NBRA];
char	*braelist[NBRA];
int	nbra;
char *loc1, *loc2, *locs;
int	sed;

int	circf;
int	low;
int	size;

char	bittab[] = {
	1,
	2,
	4,
	8,
	16,
	32,
	64,
	128
};

char *
compile(instring, ep, endbuf, seof)
register char *ep;
char *instring, *endbuf;
{
	INIT	/* Dependent declarations and initializations */
	register c;
	register eof = seof;
	char *lastep = instring;
	int cclcnt;
	char bracket[NBRA], *bracketp;
	int closed;
	char neg;
	int lc;
	int i, cflg;

	lastep = 0;
	if((c = GETC()) == eof) {
		if(*ep == 0 && !sed)
			ERROR(41);
		RETURN(ep);
	}
	bracketp = bracket;
	circf = closed = nbra = 0;
	if (c == '^')
		circf++;
	else
		UNGETC(c);
	for (;;) {
		if (ep >= endbuf)
			ERROR(50);
		if((c = GETC()) != '*' && ((c != '\\') || (PEEKC() != '{')))
			lastep = ep;
		if (c == eof) {
			*ep++ = CEOF;
			RETURN(ep);
		}
		switch (c) {

		case '.':
			*ep++ = CDOT;
			continue;

		case '\n':
			ERROR(36);
		case '*':
			if (lastep==0 || *lastep==CBRA || *lastep==CKET)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if(PEEKC() != eof)
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if(&ep[17] >= endbuf)
				ERROR(50);

			*ep++ = CCL;
			lc = 0;
			for(i = 0; i < 16; i++)
				ep[i] = 0;

			neg = 0;
			if((c = GETC()) == '^') {
				neg = 1;
				c = GETC();
			}

			do {
				if(c == '\0' || c == '\n')
					ERROR(49);
				if(c == '-' && lc != 0) {
					if ((c = GETC()) == ']') {
						PLACE('-');
						break;
					}
					while(lc < c) {
						PLACE(lc);
						lc++;
					}
				}
				lc = c;
				PLACE(c);
			} while((c = GETC()) != ']');
			if(neg) {
				for(cclcnt = 0; cclcnt < 16; cclcnt++)
					ep[cclcnt] ^= -1;
				ep[0] &= 0376;
			}

			ep += 16;

			continue;

		case '\\':
			switch(c = GETC()) {

			case '(':
				if(nbra >= NBRA)
					ERROR(43);
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;

			case ')':
				if(bracketp <= bracket)
					ERROR(42);
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;

			case '{':
				if(lastep == (char *) (0))
					goto defchar;
				*lastep |= RNGE;
				cflg = 0;
			nlim:
				c = GETC();
				i = 0;
				do {
					if ('0' <= c && c <= '9')
						i = 10 * i + c - '0';
					else
						ERROR(16);
				} while(((c = GETC()) != '\\') && (c != ','));
				if (i > 255)
					ERROR(11);
				*ep++ = i;
				if (c == ',') {
					if(cflg++)
						ERROR(44);
					if((c = GETC()) == '\\')
						*ep++ = 255;
					else {
						UNGETC(c);
						goto nlim; /* get 2'nd number */
					}
				}
				if(GETC() != '}')
					ERROR(45);
				if(!cflg)	/* one number */
					*ep++ = i;
				else if((ep[-1] & 0377) < (ep[-2] & 0377))
					ERROR(46);
				continue;

			case '\n':
				ERROR(36);

			case 'n':
				c = '\n';
				goto defchar;

			default:
				if(c >= '1' && c <= '9') {
					if((c -= '1') >= closed)
						ERROR(25);
					*ep++ = CBACK;
					*ep++ = c;
					continue;
				}
			}
			/* Drop through to default to use \ to turn off special chars */

		defchar:
		default:
			lastep = ep;
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}

step(p1, p2)
register char *p1, *p2;
{
	register c;

	if (circf) {
		loc1 = p1;
		return(advance(p1, p2));
	}
	/* fast check for first character */
	if (*p2==CCHR) {
		c = p2[1];
		do {
			if (*p1 != c)
				continue;
			if (advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while (*p1++);
		return(0);
	}
		/* regular algorithm */
	do {
		if (advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while (*p1++);
	return(0);
}

advance(lp, ep)
register char *lp, *ep;
{
	register char *curlp;
	char c;
	char *bbeg;
	int ct;

	for (;;) switch (*ep++) {

	case CCHR:
		if (*ep++ == *lp++)
			continue;
		return(0);

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp==0)
			continue;
		return(0);

	case CEOF:
		loc2 = lp;
		return(1);

	case CCL:
		c = *lp++ & 0177;
		if(ISTHERE(c)) {
			ep += 16;
			continue;
		}
		return(0);
	case CBRA:
		braslist[*ep++] = lp;
		continue;

	case CKET:
		braelist[*ep++] = lp;
		continue;

	case CCHR|RNGE:
		c = *ep++;
		getrnge(ep);
		while(low--)
			if(*lp++ != c)
				return(0);
		curlp = lp;
		while(size--) 
			if(*lp++ != c)
				break;
		if(size < 0)
			lp++;
		ep += 2;
		goto star;

	case CDOT|RNGE:
		getrnge(ep);
		while(low--)
			if(*lp++ == '\0')
				return(0);
		curlp = lp;
		while(size--)
			if(*lp++ == '\0')
				break;
		if(size < 0)
			lp++;
		ep += 2;
		goto star;

	case CCL|RNGE:
		getrnge(ep + 16);
		while(low--) {
			c = *lp++ & 0177;
			if(!ISTHERE(c))
				return(0);
		}
		curlp = lp;
		while(size--) {
			c = *lp++ & 0177;
			if(!ISTHERE(c))
				break;
		}
		if(size < 0)
			lp++;
		ep += 18;		/* 16 + 2 */
		goto star;

	case CBACK:
		bbeg = braslist[*ep];
		ct = braelist[*ep++] - bbeg;

		if(ecmp(bbeg, lp, ct)) {
			lp += ct;
			continue;
		}
		return(0);

	case CBACK|STAR:
		bbeg = braslist[*ep];
		ct = braelist[*ep++] - bbeg;
		curlp = lp;
		while(ecmp(bbeg, lp, ct))
			lp += ct;

		while(lp >= curlp) {
			if(advance(lp, ep))	return(1);
			lp -= ct;
		}
		return(0);


	case CDOT|STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (*lp++ == *ep);
		ep++;
		goto star;

	case CCL|STAR:
		curlp = lp;
		do {
			c = *lp++ & 0177;
		} while(ISTHERE(c));
		ep += 16;
		goto star;

	star:
		do {
			if(--lp == locs)
				break;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		return(0);

	}
}

getrnge(str)
register char *str;
{
	low = *str++ & 0377;
	size = *str == 255 ? 20000 : (*str &0377) - low;
}

ecmp(a, b, count)
register char	*a, *b;
register	count;
{
	if(a == b) /* should have been caught in compile() */
		error(51);
	while(count--)
		if(*a++ != *b++)	return(0);
	return(1);
}

static char *sccsid = "@(#)expr.y	4.5 (Berkeley) %G%";
yyerror(s)

{
	fprintf(stderr, "%s\n", s);
	exit(2);
}
