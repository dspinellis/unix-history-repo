/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)asexpr.c 4.2 8/15/80";
#include <stdio.h>
#include "as.h"
#include "asexpr.h"

/*
 * Tables for combination of operands.
 */
#define	XTXRN	5<<1		/* indexes last row/column when right shifted */

/*
 *	table for +
 */
readonly char pltab[6][6] = {
/*		UND	ABS	TXT	DAT	BSS	EXT */

/*UND*/		XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/		XUNDEF,	XABS,	XTEXT,	XDATA,	XBSS,	XXTRN,
/*TXT*/		XUNDEF,	XTEXT,	ERR,	ERR,	ERR,	ERR,
/*DAT*/		XUNDEF,	XDATA,	ERR,	ERR,	ERR,	ERR,
/*BSS*/		XUNDEF,	XBSS,	ERR,	ERR,	ERR,	ERR,
/*EXT*/		XUNDEF,	XXTRN,	ERR,	ERR,	ERR,	ERR,
};

/*
 *	table for -
 */
readonly char mintab[6][6] = {
/*		UND	ABS	TXT	DAT	BSS	EXT */

/*UND*/		XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/		XUNDEF,	XABS,	ERR,	ERR,	ERR,	ERR,
/*TXT*/		XUNDEF,	XTEXT,	XABS,	ERR,	ERR,	ERR,
/*DAT*/		XUNDEF,	XDATA,	ERR,	XABS,	ERR,	ERR,
/*BSS*/		XUNDEF,	XBSS,	ERR,	ERR,	XABS,	ERR,
/*EXT*/		XUNDEF,	XXTRN,	ERR,	ERR,	ERR,	ERR,
};

/* 
 *	table for other operators
 */
readonly char othtab[6][6] = {
/*		UND	ABS	TXT	DAT	BSS	EXT */

/*UND*/		XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,	XUNDEF,
/*ABS*/		XUNDEF,	XABS,	ERR,	ERR,	ERR,	ERR,
/*TXT*/		XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*DAT*/		XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*BSS*/		XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
/*EXT*/		XUNDEF,	ERR,	ERR,	ERR,	ERR,	ERR,
};

struct exp *
combine(op, exp1, exp2)
	register struct exp *exp1, *exp2;
{
	register 	e1_type, e2_type;
	register	back_type;

	lastnam=0; 			/* kludge for jxxx instructions */

	e1_type = exp1->e_xtype&XTYPE;
	e2_type = exp2->e_xtype&XTYPE;

	if (exp1->e_xtype==XXTRN+XUNDEF)
		e1_type = XTXRN;
	if (exp2->e_xtype==XXTRN+XUNDEF)
		e2_type = XTXRN;
	if (passno==1)
		if (exp1->e_xloc!=exp2->e_xloc && e1_type==e2_type)
			e1_type = e2_type = XTXRN;	/* error on != loc ctrs */
	e1_type >>= 1;		/*dispose of the external (XXTRN) bit*/
	e2_type >>= 1;

	switch (op) {
	case PLUS:
		exp1->e_xvalue += exp2->e_xvalue;
		back_type = pltab[e1_type][e2_type];
		break;
	case MINUS:
		exp1->e_xvalue -= exp2->e_xvalue;
		back_type = mintab[e1_type][e2_type];
		break;
	case IOR:
		exp1->e_xvalue |= exp2->e_xvalue;
		goto comm;
	case XOR:
		exp1->e_xvalue ^= exp2->e_xvalue;
		goto comm;
	case AND:
		exp1->e_xvalue &= exp2->e_xvalue;
		goto comm;
	case ORNOT:
		exp1->e_xvalue |= ~exp2->e_xvalue;
		goto comm;
	case LSH:
		exp1->e_xvalue <<= exp2->e_xvalue;
		goto comm;
	case RSH:
		exp1->e_xvalue >>= exp2->e_xvalue;
		goto comm;
	case TILDE:
		exp1->e_xvalue |= ~ exp2->e_xvalue;
		goto comm;
	case MUL:
		exp1->e_xvalue *= exp2->e_xvalue;
		goto comm;
	case DIV:
		if (exp2->e_xvalue == 0)
			yyerror("Divide check");
		else
			exp1->e_xvalue /= exp2->e_xvalue;
		goto comm;
	case REGOP:
		if (exp2->e_xvalue == 0)
			yyerror("Divide check (modulo)");
		else
			exp1->e_xvalue %= exp2->e_xvalue;
		goto comm;
	
	comm:
		back_type = othtab[e1_type][e2_type];
		break;
	default:
		yyerror("Internal error: unknown operator");
	}

	if (e2_type==(XTXRN>>1))
		exp1->e_xname = exp2->e_xname;
	exp1->e_xtype = back_type | (
			(exp1->e_xtype|exp2->e_xtype) & (XFORW|XXTRN) );
	if (back_type==ERR)
		yyerror("Relocation error");
	return(exp1);
}

buildtokensets()
{
#define clobber(val, set) tokensets[(val)] |= (set)

	clobber(SEMI,	LINSTBEGIN);
	clobber(NL,	LINSTBEGIN);
	clobber(INT,	LINSTBEGIN);

	clobber(NAME,	YUKKYEXPRBEG + LINSTBEGIN);
	clobber(INSTn,	YUKKYEXPRBEG);
	clobber(INST0,	YUKKYEXPRBEG);
	clobber(REG,	YUKKYEXPRBEG);
	clobber(BFINT,	YUKKYEXPRBEG);

	clobber(INT,	SAFEEXPRBEG);
	clobber(FLTNUM,	SAFEEXPRBEG);

	clobber(PLUS,	ADDOPS);
	clobber(MINUS,	ADDOPS + EBEGOPS);

	clobber(LP,	EBEGOPS);

	clobber(IOR,	BOOLOPS);
	clobber(XOR,	BOOLOPS);
	clobber(AND,	BOOLOPS);
	clobber(ORNOT,	BOOLOPS);

	clobber(TILDE,	MULOPS + EBEGOPS);
	clobber(LSH,	MULOPS);
	clobber(RSH,	MULOPS);
	clobber(MUL,	MULOPS);
	clobber(DIV,	MULOPS);
	clobber(REGOP,	MULOPS);	/* % */

}

/*
 *	We keep the current token class in this global variable, so 
 *	the recursive descent expression analyzers can talk amongst
 *	themselves, and so that we may use the macros shift and shift over
 */

extern	int	yylval;		/*the value of the lexical value*/
extern	struct	exp	*xp;	/*the next free expression slot*/

static int	val;
int exprparse(inval, backexpr)	/*return the value the read head is sitting on*/
	int	inval;
	struct	exp **backexpr;
{
	register struct exp *lexpr;
	int	op;

	val = inval;
	lexpr = boolterm();
	while (INTOKSET(val, ADDOPS)){
		op = val;
		shift;
		lexpr = combine(op, lexpr, boolterm());
	}
	*backexpr = lexpr;
	return(val);
}

struct exp *boolterm()
{
	register	struct exp *lexpr;
	int	op;

	lexpr = term();
	while(INTOKSET(val, BOOLOPS)){
		op = val;
		shift;
		lexpr = combine(op, lexpr, term());
	}
	return(lexpr);
}

struct exp *term()
{
	register	struct	exp	*lexpr;
	int		op;

	lexpr = factor();
	while(INTOKSET(val, MULOPS)){
		op = val;
		shift;
		lexpr = combine(op, lexpr, factor());
	}
	return(lexpr);
}

struct exp *factor()
{
	struct	exp *lexpr;
	int		op;
	extern		int	droppedLP;	/*called exprparse after consuming an LP*/

	if (val == LP || droppedLP){
		if (droppedLP)
			droppedLP = 0;
		else
			shift;		/*the LP*/
		val = exprparse(val, &lexpr);
		if (val != RP)
			yyerror("right parenthesis expected");
		else
			shift;
	} else
	if (INTOKSET(val, YUKKYEXPRBEG)){
		lexpr = yukkyexpr(val, yylval);
		shift;
	}
	else if (INTOKSET(val, SAFEEXPRBEG)){
		lexpr = (struct exp *)yylval;
		shift;
	}
	else if ( (val == TILDE) || (val == MINUS) ){
		op = val;
		shift;
		lexpr = xp++;
		lexpr->e_xtype = XABS;
		lexpr->e_xvalue = 0;
		lexpr = combine(op, lexpr, factor());
	}
	else {
		yyerror("Bad expression syntax");
		lexpr = xp++;
		lexpr->e_xtype = XABS;
		lexpr->e_xvalue = 0;
	}
	return(lexpr);
}

struct exp *yukkyexpr(val, np)
	int	val;
	register	np;
{
	register	struct exp *locxp;
	extern		int	exprisname;	/*last factor is a name*/

	exprisname = 0;
	locxp = xp++;
	if (val == NAME || val == BFINT){
		if (val == BFINT) {
			int off = 0;
			yylval = ((struct exp *)np)->e_xvalue;
			if (yylval < 0) {
				yylval = -yylval;
				yylval--;
				off = -1;
				if (lgensym[yylval] == 1)
					yyerror("Reference to undefined local label %db", yylval);
			} else {
				yylval--;
				genref[yylval] = 1;
			}
			sprintf(yytext, "L%d\001%d", yylval, lgensym[yylval] + off);
			yylval = np = (int)*lookup(passno == 1);
			lastnam = (struct symtab *)np;
		}
		exprisname++;
		locxp->e_xtype = ((struct symtab *)np)->s_type;
		if (( ((struct symtab *)np)->s_type&XTYPE)==XUNDEF) { /*forward*/
			locxp->e_xname = (struct symtab *)np;
			locxp->e_xvalue = 0;
			if (passno==1)
				((struct symtab *)np)->s_type |= XFORW;
		} else {	/*otherwise, just get the value*/
			locxp->e_xvalue = ((struct symtab *)np)->s_value;
			locxp->e_xname = NULL;
		}
	} else {	/*INSTn or INST0 or REG*/
		locxp->e_xtype = XABS;
		locxp->e_xvalue = ( (int)np) & 0xFF;
		locxp->e_xloc = 0;
		locxp->e_xname = NULL;
	}

	return(locxp);
}


#ifdef DEBUG
char	*tok_name[LASTTOKEN - FIRSTTOKEN + 1];
struct Tok_Desc{
	int		tok_which;
	char		*tok_name;
} tok_desc[] = {
	FIRSTTOKEN,	"firsttoken",	/* 0 */
	ISPACE,		"ispace", 	/* 1 */
	IBYTE,		"ibyte", 	/* 2 */
	IWORD,		"iword", 	/* 3 */
	IINT,		"iint", 	/* 4 */
	ILONG,		"ilong", 	/* 5 */
	IDATA,		"idata", 	/* 6 */
	IGLOBAL,	"iglobal", 	/* 7 */
	ISET,		"iset", 	/* 8 */
	ITEXT,		"itext", 	/* 9 */
	ICOMM,		"icomm", 	/* 10 */
	ILCOMM,		"ilcomm", 	/* 11 */
	IFLOAT,		"ifloat", 	/* 12 */
	IDOUBLE,	"idouble", 	/* 13 */
	IORG,		"iorg", 	/* 14 */
	IASCII,		"iascii", 	/* 15 */
	IASCIZ,		"iasciz", 	/* 16 */
	ILSYM,		"ilsym", 	/* 17 */
	IFILE,		"ifile", 	/* 18 */
	ILINENO,	"ilineno", 	/* 19 */
	IABORT,		"iabort", 	/* 20 */
	ISTAB,		"istab", 	/* 23 */
	ISTABSTR,	"istabstr", 	/* 24 */
	ISTABNONE,	"istabnone", 	/* 25 */
	ISTABDOT,	"istabdot", 	/* 26 */
	IJXXX,		"ijxxx", 	/* 27 */
	IALIGN,		"ialign", 	/* 28 */
	INST0,		"inst0", 	/* 29 */
	INSTn,		"instn", 	/* 30 */
	BFINT,		"bfint",	/* 31 */
	PARSEEOF,	"parseeof",	/* 32 */
	ILINESKIP,	"ilineskip",	/* 33 */
	VOID,		"void",		/* 34 */
	SKIP,		"skip",		/* 35 */
	INT,		"int",		/* 36 */
	FLTNUM,		"fltnum",	/* 37 */
	NAME,		"name",		/* 38 */
	STRING,		"string",	/* 39 */
	QUAD,		"quad",		/* 40 */
	SIZESPEC,	"sizespec", 	/* 41 */
	REG,		"reg",		/* 42 */
	MUL,		"mul",		/* 43 */
	LITOP,		"litop",	/* 44 */
	LP,		"lp",		/* 45 */
	MP,		"mp",		/* 46 */
	NEEDSBUF,	"needsbuf",	/* 48 */	
	REGOP,		"regop",	/* 49 */
	NL,		"nl",		/* 50 */
	SCANEOF,	"scaneof",	/* 51 */
	BADCHAR,	"badchar",	/* 52 */
	SP,		"sp",		/* 53 */
	ALPH,		"alph",		/* 54 */
	DIG,		"dig",		/* 55 */
	SQ,		"sq",		/* 56 */
	DQ,		"dq",		/* 57 */
	SH,		"sh",		/* 58 */
	LSH,		"lsh",		/* 59 */
	RSH,		"rsh",		/* 60 */
	MINUS,		"minus",	/* 61 */
	SIZEQUOTE,	"sizequote",	/* 62 */
	XOR,		"xor",		/* 64 */
	DIV,		"div",		/* 65 */
	SEMI,		"semi",		/* 66 */
	COLON,		"colon",	/* 67 */
	PLUS,		"plus",		/* 68 */
	IOR,		"ior",		/* 69 */ 
	AND,		"and",		/* 70 */
	TILDE,		"tilde",	/* 71 */
	ORNOT,		"ornot",	/* 72 */
	CM,		"cm",		/* 73 */
	LB,		"lb",		/* 74 */
	RB,		"rb",		/* 75 */
	RP,		"rp",		/* 76 */
	LASTTOKEN,	"lasttoken"	/* 80 */
};
/*
 *	turn a token type into a string
 */
static	int	fixed = 0;
char *tok_to_name(token)
{
	if (!fixed){
		int	i;
		for (i = FIRSTTOKEN; i <= LASTTOKEN; i++)
			tok_name[i] = "NOT ASSIGNED";
		for (i = FIRSTTOKEN; i <= sizeof(tok_desc)/sizeof(struct Tok_Desc); i++){
			tok_name[tok_desc[i].tok_which] = tok_desc[i].tok_name;
		}
		fixed = 1;
	}
	if (FIRSTTOKEN <= token && token <= LASTTOKEN)
		return(tok_name[token]);
	else
		panic("Unknown token number, %d\n", token);
	/*NOTREACHED*/
}
#endif DEBUG
