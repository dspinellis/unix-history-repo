%token COLON
%token PLUS MINUS
%token MUL DIV
%token IOR XOR AND
%token TILDE LSH RSH
%token LITOP REGOP
%token CM NL LB RB LP RP MP SEMI
%token INT NAME REG
%token NOCHAR SP ALPH DIG SQ SH
%token INST0 INSTn IJXXX
%token ISPACE IBYTE IWORD IINT ILONG
%token IDATA IGLOBAL ISET ITEXT ICOMM ILCOMM IALIGN
%token IFLOAT IDOUBLE IORG
%token ISTAB
%token ILSYM
%token FLTNUM
%token NOTYET

%nonassoc COLON
%left PLUS MINUS
%left IOR XOR AND
%left MUL DIV LSH RSH TILDE

%{
#include <stdio.h>
#include "as.h"
#include "as.yh"
static char AS0[]="@(#)as0.y 1.16 79/03/21 22:27:23"; /* SCCS id */
struct arg *ap	= { arglist};
struct	exp	*xp = { explist};
int	i, ii;
long	li;
struct	exp	usedot[];
struct	exp	*dotp = &usedot[0];
int	anyerrs;
int argcnt;
int	passno	= 1;
FILE	*tmpfil;
FILE	*relfil;
FILE	*txtfil;
int	hshused;
long	tsize;
long	dsize;
long	bitfield;
int	bitoff;
int	usrname;
int lclname;
struct symtab *lastnam;
char	yytext[NCPS+2];
char	sname[8];
struct symtab	*stpt;
%}

%%

%{
	struct exp *pval;
%}

wholefile: file = {
		curlen = NBPW/4;
		flushfield(NBPW/4);
	}

file:	/* empty */ = {
		goto reset;
	}
	| file linstruction NL = {
		lineno++;
		goto reset;
	}
	| file linstruction SEMI = {
		goto reset;
	}
	| file error NL = {
		lineno++;
		yyerrok;
	reset:
		ap = arglist; argcnt=0;
		xp = explist;
		usrname = 0;
	}
	;

labels:		/* empty */
	| labels NAME COLON = {
		flushfield(NBPW/4);
		if (($2->type&XTYPE)!=XUNDEF) {
			if(($2->type&XTYPE)!=dotp->xtype || $2->value!=dotp->xvalue
			 || passno==1 && $2->index != dotp->xloc)
				if ($2->name[0] != 'L' || $2->name[1] != 'L')
					yyerror("%.8s redefined", $2->name);
		}
		$2->type &= ~(XTYPE|XFORW);
		$2->type |= dotp->xtype;
		$2->value = dotp->xvalue;
		if (passno==1)
			$2->index = dotp-usedot;
	}
	;

linstruction:	labels instruction
		;

instruction:
	ISET NAME CM expr = {
		$2->type &= (XXTRN|XFORW);
		$2->type |= $4->xtype&(XTYPE|XFORW);
		$2->value = $4->xvalue;
		if (passno==1)
			$2->index = $4->xloc;
		if (($4->xtype&XTYPE)==XUNDEF)
			yyerror("Illegal set");
	}
	| setlcl NAME CM expr = {
		if ($4->xtype!=XABS) yyerror("Illegal lsym");
		$2->value=$4->xvalue; $2->type=XABS;
	}
	| IGLOBAL NAME = {
		$2->type |= XXTRN;
	}
	| IDATA = {
		i = -IDATA;
		goto chloc;
	}
	| IDATA expr = {
		i = IDATA;
		goto chloc;
	}
	| ITEXT = {
		i = -ITEXT;
		goto chloc;
	}
	| ITEXT expr = {
		i = ITEXT;
	chloc:
		if (i < 0) {
			ii = 0;
			i = -i;
		} else {
			if ($2->xtype != XABS || (ii=$2->xvalue) >= NLOC) {
				yyerror("illegal location counter");
				ii = 0;
			}
		}
		if (i == IDATA)
			ii += NLOC;
		flushfield(NBPW/4);
		dotp = &usedot[ii];
		if (passno==2) {
			if (usefile[ii] == NULL) {
				tmpn2[TMPC] = 'a'+ii;
				if ((usefile[ii]=fopen(tmpn2, "w"))==NULL) {
					yyerror("cannot create temp");
					delexit();
				}
				tmpn3[TMPC] = 'a'+ii;
				if ((rusefile[ii]=fopen(tmpn3, "w"))==NULL) {
					yyerror("cannot create temp");
					delexit();
				}
			}
			txtfil = usefile[ii];
			relfil = rusefile[ii];
		}
	}
	| IBYTE setchar explist = {
		flushfield(NBPW/4);
		if (bitoff)
			dotp->xvalue++;
	}
	| ILONG setlong explist = {
		flushfield(NBPW);
	}
	| IINT setlong explist = {
		flushfield(NBPW);
	}
	| IWORD setword explist = {
		flushfield(NBPW/2);
	}
	| ISPACE expr = {
		if ($2->xtype != XABS)
			yyerror("space size not absolute");
		li = $2->xvalue;
	ospace:
		flushfield(NBPW/4);
		while (--li>=0) outb(0);
	}
	| IORG expr = {
		if ($2->xtype==XABS)
			orgwarn++;
		else if ($2->xtype!=dotp->xtype)
			yyerror("Illegal 'org'");
		li = $2->xvalue - dotp->xvalue;
		if (li < 0)
			yyerror("Backwards 'org'");
		goto ospace;
	}
	| ISTAB expr CM expr CM expr CM expr CM expr CM expr CM expr CM expr CM
		expr CM expr CM expr CM expr = {
		if (passno == 1) 
			if ($18->xvalue & STABTYPS) {
				struct symtab *p;
				stpt = symalloc();
				stpt->name[0] = $2->xvalue;
				stpt->name[1] = $4->xvalue;
				stpt->name[2] = $6->xvalue;
				stpt->name[3] = $8->xvalue;
				stpt->name[4] = $10->xvalue;
				stpt->name[5] = $12->xvalue;
				stpt->name[6] = $14->xvalue;
				stpt->name[7] = $16->xvalue;
				stpt->ptype = $18->xvalue;
				stpt->other = $20->xvalue;
				stpt->desc = $22->xvalue;
				p = $24->xname;
				if(p == NULL) {
					stpt->value = $24->xvalue;
				}
				else {
					stpt->value = $24->xname;
					stpt->index = p->index;
					stpt->type = p->type | 0200;
/*
printf("FORWARD REF FOR .STAB...\n");
printf("value (xname) = %x value(value(xname) = %x\n", stpt->value, stpt->value->value);
printf("name: %.8s\n\n", (stpt->value)->name);
*/
				}
			}
			else yyerror("Invalid type in .stab");
	}
	| comm NAME CM expr = {
		if ($4->xtype != XABS)
			yyerror("comm size not absolute");
		if (passno==1 && ($2->type&XTYPE)!=XUNDEF)
			yyerror("Redefinition of %.8s", $2->name);
		if (passno==1) {
			$2->value = $4->xvalue;
			if ($1==ICOMM)
				$2->type |= XXTRN;
			else {
				$2->type &= ~XTYPE;
				$2->type |= XBSS;
			}
		}
	}
	| IALIGN expr = {
		jalign($2);
	}
	| INST0 = {
		insout($1, 0, 0);
	}
	| INSTn args = {
		insout($1, arglist, argcnt);
	}
	| IJXXX args = {
		insout($1, arglist, -argcnt);
	}
	| floating flist
	|  /* empty */
	;

comm:	ICOMM = {
		$$ = ICOMM;
	}
	| ILCOMM = {
		$$ = ILCOMM;
	}
	;
explist: /* empty */
	| explist CM outexpr
	| outexpr
	;

outexpr: expr = {
		i = curlen;
		pval = $1;
		flushfield(curlen);
		goto outx;
	}
	| expr COLON expr = {
		if ($1->xtype != XABS)
			yyerror("Width expression not absolute");
		i = $1->xvalue;
		pval = $3;
		if (bitoff+i > curlen)
			flushfield(curlen);
		if (i > curlen)
			yyerror("Expression crosses field boundary");
	outx:
		 if ((pval->xtype&XTYPE)!=XABS) {
			if (bitoff) yyerror("Illegal relocation in field");
			i=LEN1+!PCREL;
			if (curlen==NBPW) i=LEN4+!PCREL; if (curlen==NBPW/2) i=LEN2+!PCREL;
			outrel(&pval->xvalue,i,pval->xtype,pval->xname);
		} else {
			li = pval->xvalue & ((1L<<i)-1);
			bitfield |= li << bitoff; bitoff += i;
		}
		ap = arglist;
		xp = explist;
	}

floating: IFLOAT = {
		curlen = 4;
	}
	| IDOUBLE = {
		curlen = 8;
	}

flist:	/* empty */
	| flist CM fltout
	| fltout
	;

fltout:	FLTNUM = {
		dotp->xvalue += curlen;
		if (passno==2)
			putflt($1, curlen);
	}
	;

setlcl: ILSYM = {
		lclname=1;
	}

setchar: = {
		curlen = NBPW/4;
	}

setlong: = {
		curlen = NBPW;
	}

setword: = {
		curlen = NBPW/2;
	}

args:	arg = {if(++argcnt>6) yyerror("More than 6 arguments"); ++ap;}
	| args CM arg = {if(++argcnt>6) yyerror("More than 6 arguments"); ++ap;}
	;
arg:	reg = {
		ap->atype = AREG;
		ap->areg1 = $1;
	}
	| argb1 LB reg RB = {
		ap->atype |= AINDX;
		ap->areg2 = $3;
	}
	| argb1
	;
argb1:	MP reg RP = {
		ap->atype = ADECR;
		ap->areg1 = $2;
	}
	| MUL argb2 = {
		if (ap->atype == ABASE) {/* concession for *(%r) meaning *0(%r) */
			ap->atype = ADISP; xp->xtype = XABS; xp->xvalue = 0; xp->xloc = 0;
			ap->xp = xp++;
		}
		ap->atype |= ASTAR;
	}
	| argb2
	;
argb2:	expr = {
		ap->atype = AEXP;
		ap->xp = $1;
		ap->areg1 = 0;
	}
	|	LP reg RP = {
		ap->atype = ABASE;
		ap->areg1 = $2;
	}
	| expr LP reg RP = {
		ap->atype = ADISP;
		ap->xp = $1;
		ap->areg1 = $3;
	}
	| LP reg RP PLUS = {
		ap->atype = AINCR;
		ap->areg1 = $2;
	}
	| LITOP expr = {
		ap->atype = AIMM;
		ap->areg1 = 0;
		ap->xp = $2;
	}
	;

reg:
	REG
	| REGOP expr = {
		if (passno==2 && (($2->xtype&XTYPE)!=XABS || $2->xvalue<0 || $2->xvalue>=16))
			yyerror("Illegal register");
		$$ = $2->xvalue;
	};

expr:	NAME = {
		$$ = xp++;
		$$->xtype = $1->type;
		if (($1->type&XTYPE)==XUNDEF) {
			$$->xname = $1;
			$$->xvalue = 0;
			if (passno==1)
				$1->type |= XFORW;
		} else {
			$$->xvalue = $1->value;
			$$->xname = NULL;
		}
	}
	| INT = {
		$$ = xp++;
		$$->yvalue = 0;
		$$->xtype = XABS;
		$$->xvalue = * (long *)$1;
		$$->xloc = 0;
		$$->xname = NULL;
	}
	| FLTNUM = {
		$$ = xp++;
		$$->yvalue = * (((long *)$1)+1);
		$$->xtype = XABS;
		$$->xvalue = * (long *)$1;
		$$->xloc = 0;
		$$->xname = NULL;
	}
	| LP expr RP = {
		$$ = $2;
	}
	| expr PLUS expr = {
		goto comb;
	}
	| expr MINUS expr = {
		goto comb;
	}
	| expr TILDE expr = {
		goto comb;
	}
	| expr IOR expr = {
		goto comb;
	}
	| expr XOR expr = {
		goto comb;
	}
	| expr AND expr = {
		goto comb;
	}
	| expr LSH expr = {
		goto comb;
	}
	| expr RSH expr = {
		goto comb;
	}
	| expr MUL expr = {
		goto comb;
	}
	| expr DIV expr = {
	comb:
		$$ = combine($2, $1, $3);
	}
	| nothing MINUS expr = {
		goto comb;
	}
	| nothing TILDE expr = {
		goto comb;
	}
	| REG = {
	sugar:
		$$ = xp++;
		$$->xtype = XABS;
		$$->xvalue = $1 & 0xFF;
		$$->xloc = 0;
	}
	| INST0 = {goto sugar;}
	| INSTn = {goto sugar;}
	;

nothing: = {
		$$ = xp++;
		$$->xtype = XABS;
		$$->xvalue = 0;
	}

%%

yyerror(s, a)
char *s;
{
	if (anyerrs==0)
		fprintf(stderr, "Assembler:\n");
	anyerrs++;
	fprintf(stderr, "line %d: ", lineno);
	fprintf(stderr, s, a);
	fprintf(stderr, "\n");
	yyerrok;
}

/*
 * Tables for combination of operands.
 */

/* table for + */
readonly char pltab[6][6] = {
/*	UND	ABS	TXT	DAT	BSS	EXT */
/*UND*/	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/	XUNDEF,	XABS,	XTEXT,	XDATA,	XBSS,	XXTRN,
/*TXT*/	XUNDEF,	XTEXT,	ERR,	ERR,	ERR,	ERR,
/*DAT*/	XUNDEF,	XDATA,	ERR,	ERR,	ERR,	ERR,
/*BSS*/	XUNDEF,	XBSS,	ERR,	ERR,	ERR,	ERR,
/*EXT*/	XUNDEF,	XXTRN,	ERR,	ERR,	ERR,	ERR,
};

/* table for - */
readonly char mintab[6][6] = {
/*	UND	ABS	TXT	DAT	BSS	EXT */
/*UND*/	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/	XUNDEF,	XABS,	ERR,	ERR,	ERR,	ERR,
/*TXT*/	XUNDEF,	XTEXT,	XABS,	ERR,	ERR,	ERR,
/*DAT*/	XUNDEF,	XDATA,	ERR,	XABS,	ERR,	ERR,
/*BSS*/	XUNDEF,	XBSS,	ERR,	ERR,	XABS,	ERR,
/*EXT*/	XUNDEF,	XXTRN,	ERR,	ERR,	ERR,	ERR,
};

/* table for other operators */
readonly char othtab[6][6] = {
/*	UND	ABS	TXT	DAT	BSS	EXT */
/*UND*/	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/	XUNDEF,	XABS,	ERR,	ERR,	ERR,	ERR,
/*TXT*/	XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*DAT*/	XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*BSS*/	XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*EXT*/	XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
};

struct exp *
combine(op, r1, r2)
register struct exp *r1, *r2;
{
	register t1, t2, type, loc;

	lastnam=0; /* kluge for jxxx instructions */
	t1 = r1->xtype&XTYPE;
	t2 = r2->xtype&XTYPE;
	if (r1->xtype==XXTRN+XUNDEF)
		t1 = XTXRN;
	if (r2->xtype==XXTRN+XUNDEF)
		t2 = XTXRN;
	if (passno==1)
		if (r1->xloc!=r2->xloc && t1==t2)
			t1 = t2 = XTXRN;	/* error on != loc ctrs */
	t1 >>= 1;
	t2 >>= 1;
	switch (op) {

	case PLUS:
		r1->xvalue += r2->xvalue;
		type = pltab[t1][t2];
		break;

	case MINUS:
		r1->xvalue -= r2->xvalue;
		type = mintab[t1][t2];
		break;

	case IOR:
		r1->xvalue |= r2->xvalue;
		goto comm;

	case XOR:
		r1->xvalue ^= r2->xvalue;
		goto comm;

	case AND:
		r1->xvalue &= r2->xvalue;
		goto comm;

	case LSH:
		r1->xvalue <<= r2->xvalue;
		goto comm;

	case RSH:
		r1->xvalue >>= r2->xvalue;
		goto comm;

	case TILDE:
		r1->xvalue |= ~ r2->xvalue;
		goto comm;

	case MUL:
		r1->xvalue *= r2->xvalue;
		goto comm;

	case DIV:
		if (r2->xvalue == 0)
			yyerror("Divide check");
		else
			r1->xvalue /= r2->xvalue;
		goto comm;

	comm:
		type = othtab[t1][t2];
		break;

	default:
		yyerror("Internal error: unknown operator");
	}
	if (t2==(XTXRN>>1))
		r1->xname = r2->xname;
	r1->xtype = type | (r1->xtype|r2->xtype)&(XFORW|XXTRN);
	if (type==ERR)
		yyerror("Relocation error");
	return(r1);
}

readonly short type[] = {
	EOF,
	SP,	0,	0,	0,	0,	0,	0,	0,
	0,	SP,	NL,	0,	0,	SP,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	SP,	0,	0,	SH,	LITOP,	REG,	AND,	SQ,
	LP,	RP,	MUL,	PLUS,	CM,	MINUS,	ALPH,	DIV,
	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,
	DIG,	DIG,	COLON,	SEMI,	LSH,	0,	RSH,	0,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	LB,	0,	RB,	XOR,	ALPH,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	0,	IOR,	0,	TILDE,	0,
};

yylex()
{
	register short val;
	register base;
	register char *cp;
	struct symtab *op;
	static long intval;
	static double fltval;
	char fltchr[64];

	if (passno!=1) {
		val = get2(tmpfil);
		if (val==NAME)
			fread(&yylval, sizeof(yylval), 1, tmpfil);
		else if (val==INT) {
			fread(&intval, sizeof(intval), 1, tmpfil);
			yylval = &intval;
		} else if (val==FLTNUM) {
			fread(&fltval, sizeof(fltval), 1, tmpfil);
			yylval = &fltval;
		} else
			yylval = get2(tmpfil);
		return(val);
	}
loop:
	switch(yylval = (type+1)[val = getchar()]) {

	case SH:
		while ((val = getchar()) != '\n' && val>0)
			;
		val = NL;
		goto ret;

	case SP:
		goto loop;

	case REG:
		val = getchar();
		if (val>='0' && val<='9') {
			yylval = val-'0';
			if (val=='1') {
				if ((val = getchar())>='0' && val<='5')
					yylval = 10+val-'0';
				else
					ungetc(val, stdin);
			}
			while ((val = getchar()) == '+' || val=='-' ) {
				if (val=='+')
					yylval++;
				else
					yylval--;
			}
			ungetc(val, stdin);
			val = REG;
		} else {
			ungetc(val, stdin);
			val = REGOP;
		}
		goto ret;

	case EOF:
		val = 0;
		goto ret;

	case ALPH:
		cp = yytext;
		do {
			if (cp <= &yytext[NCPS])
				*cp++ = val;
		} while ((type+1)[val=getchar()]==ALPH || (type+1)[val]==DIG);
		*cp = '\0';
		while (val=='\t' || val==' ')
			val = getchar();
		ungetc(val, stdin);
		base = 1;
		if (val==':') {
			base = usrname;
			usrname = 1;
		}
		if ((op = *lookup(1))->tag) {
			yylval = op->type;
			val = op->tag+256;
			goto ret;
		} else {
			lastnam = yylval = op;
			usrname = base;
			val = NAME;
			goto ret;
		}

	case DIG:
		intval = val-'0';
		if (val=='0') {
			val = getchar();
			if (val=='x' || val=='X') {
				base = 16;
			} else if (val=='d' || val=='D' || val=='f' || val=='F') {
				char *p = fltchr;
				double atof();
				while (p < &fltchr[63] && ((val=getchar())=='.'
				 || val=='e' || val=='d' || val=='E' || val=='D'
				 || val=='-' || val=='+' || (type+1)[val]==DIG))
					*p++ = val;
				ungetc(val, stdin);
				*p++ = '\0';
				fltval = atof(fltchr);
				val = FLTNUM;
				goto ret;
			} else {
				ungetc(val, stdin);
				base = 8;
			}
		} else
			base = 10;
		while ((type+1)[val=getchar()]==DIG
		    || (base==16 && (val>='a' && val<='f'||val>='A' && val<='F'))) {
			if (base==8)
				intval <<= 3;
			else if (base==10)
				intval *= 10;
			else
				intval <<= 4;
			if (val>='a' && val<='f')
				val -= 'a' - 10 - '0';
			else if (val>='A' && val<='F')
				val -= 'A' - 10 - '0';
			intval += val-'0';
		}
		ungetc(val, stdin);
		val = INT;
		goto ret;

	case MINUS:
		if ((val = getchar())=='(') yylval=val=MP;
		else {ungetc(val,stdin); val=MINUS;}
		goto ret;

	case SQ:
		if ((yylval = getchar()) == '\n')
			lineno++;
		intval = yylval;
		val = INT;
		goto ret;

	case 0:
		yyerror("Illegal character");
		val = NOCHAR;
		goto ret;

	default:
		val = yylval;
		goto ret;
	}
ret:
	put2(val, tmpfil);
	if (val==NAME)
		fwrite(&yylval, sizeof(yylval), 1, tmpfil);
	else if (val==INT) {
		fwrite(&intval, sizeof(intval), 1, tmpfil);
		yylval = &intval;
	} else if (val==FLTNUM) {
		fwrite(&fltval, sizeof(fltval), 1, tmpfil);
		yylval = &fltval;
	} else
		put2(yylval, tmpfil);
	return(val);
}

