/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)asexpr.c 4.5 6/30/83";

#endif not lint
#include <stdio.h>
#include "as.h"
#include "asscan.h"
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

struct exp *combine(op, exp1, exp2)
	reg struct exp *exp1, *exp2;
{
	reg 	int e1_type, e2_type;
	reg	int back_type;
	char	*btype = "The assembler can only do arithmetic on 1,2, or 4 byte integers";

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

	switch (op){
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
	clobber(BIGNUM,	SAFEEXPRBEG);

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

static	inttoktype	val;

/*
 *	return the value the read head is sitting on
 */
inttoktype exprparse(inval, backexpr)
	inttoktype	inval;
	struct	exp **backexpr;
{
	reg	struct exp *lexpr;
	inttoktype	op;

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
	reg	struct exp *lexpr;
	inttoktype	op;

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
	reg	struct	exp	*lexpr;
	inttoktype	op;

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
	inttoktype	op;
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
		lexpr->e_number = Znumber;
		lexpr->e_number.num_tag = TYPL;
		lexpr = combine(op, lexpr, factor());
	} else {
		yyerror("Bad expression syntax");
		lexpr = xp++;
		lexpr->e_xtype = XABS;
		lexpr->e_number = Znumber;
		lexpr->e_number.num_tag = TYPL;
	}
	return(lexpr);
}

struct exp *yukkyexpr(val, np)
	int	val;
	reg	int np;
{
	reg	struct exp *locxp;
	extern	int	exprisname;	/*last factor is a name*/
		int	off = 0;

	exprisname = 0;
	locxp = xp++;
	locxp->e_number = Znumber;
	locxp->e_number.num_tag = TYPL;

	switch(val){
	case BFINT:
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
		(void)sprintf(yytext, "L%d\001%d", yylval, lgensym[yylval] + off);
		yylval = np = (int)*lookup(passno == 1);
		lastnam = (struct symtab *)np;
		/* FALLTHROUGH */
	case NAME:
		exprisname = (int)np;
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
		break;
	default:
		yyerror("Internal Error in yukkyexpr");
		/* FALLTHROUGH */

	case INSTn:
	case INST0:
	case REG:
		locxp->e_xtype = XABS;
		locxp->e_xvalue = ( (int)np) & 0xFF;
		locxp->e_xloc = 0;
		locxp->e_xname = NULL;
		break;
	}

	return(locxp);
}

/*
 *	Print definitions for token kinds
 */
static char pdirect[]	= "directive";
static char pinstr[]	= "instruction";
static char phunk[]	= "lexeme";
static char psmall[]	= "small symbol";
static char pcntrl[]	= "control token";

#define	DIRECT	pdirect
#define	INSTR	pinstr
#define	HUNK	phunk
#define	SMALL	psmall
#define	CNTRL	pcntrl

struct Tok_Desc{
	int		tok_which;
	char		*tok_kind;
	char		*tok_name;
};
struct Tok_Desc *tok_name[LASTTOKEN - FIRSTTOKEN + 1];

struct Tok_Desc tok_desc[] = {
	FIRSTTOKEN,	DIRECT,	"first token",

	IBYTE,		DIRECT,	".byte",
	IWORD,		DIRECT,	".word",
	IINT,		DIRECT,	".int",
	ILONG,		DIRECT,	".long",
	IQUAD,		DIRECT,	".quad",
	IFFLOAT,	DIRECT,	".ffloat",
	IDFLOAT,	DIRECT,	".dfloat",
	IASCII,		DIRECT,	".ascii",
	IASCIZ,		DIRECT,	".asciz",
	IFILL,		DIRECT,	".fill",
	ISPACE,		DIRECT,	".space",

	IDATA,		DIRECT,	".data",
	ITEXT,		DIRECT,	".text",
	IGLOBAL,	DIRECT,	".global",
	IALIGN,		DIRECT,	".align",

	ISET,		DIRECT,	".set",
	ICOMM,		DIRECT,	".comm",
	ILCOMM,		DIRECT,	".lcomm",
	IORG,		DIRECT,	".org",
	ILSYM,		DIRECT,	".lsym",

	ISTAB,		DIRECT,	".stab",
	ISTABSTR,	DIRECT,	".stabstr",
	ISTABNONE,	DIRECT,	".stabnone",
	ISTABDOT,	DIRECT,	".stabdot",

	IFILE,		DIRECT,	".file",
	ILINENO,	DIRECT,	".lineno",
	IABORT,		DIRECT,	".abort",

	IJXXX,		INSTR,	"jump pseudo",
	INST0,		INSTR,	"0 argument inst",
	INSTn,		INSTR,	"n argument inst",

	PARSEEOF,	CNTRL,	"parse end of file",
	ILINESKIP,	CNTRL,	"skip lines",
	VOID,		CNTRL,	"void",
	SKIP,		CNTRL,	"skip",
	NL,		CNTRL,	"new line",
	SCANEOF,	CNTRL,	"scanner end of file",
	BADCHAR,	CNTRL,	"bad character",
	SH,		CNTRL,	"comment, #",

	INT,		HUNK,	"int",
	BFINT,		HUNK,	"local label",
	BIGNUM,		HUNK,	"big number",
	NAME,		HUNK,	"name",
	STRING,		HUNK,	"string",
	REG,		HUNK,	"register specifier",

	SIZESPEC,	SMALL,	"size specifier, [BWLbwl]",
	SIZEQUOTE,	SMALL,	"sizequote, [^']",
	LITOP,		SMALL,	"litop",

	MP,		SMALL,	"minus parenthesis, -(",
	REGOP,		SMALL,	"register operator, %",

	SP,		SMALL,	"space",
	ALPH,		SMALL,	"alphabetic character, [A-Za-z_]",
	DIG,		SMALL,	"digit character, [A-Fa-f0-9]",

	SQ,		SMALL,	"single quote, '",
	DQ,		SMALL,	"double quote, \"",

	LSH,		SMALL,	"arithmetic left shift, <",
	RSH,		SMALL,	"arithmetic right shift, >",
	XOR,		SMALL,	"exclusive or, ^",

	PLUS,		SMALL,	"plus, +",
	MINUS,		SMALL,	"minus, -",
	MUL,		SMALL,	"multiply, *",
	DIV,		SMALL,	"divide, /",
	SEMI,		SMALL,	"semi colon, ;",
	COLON,		SMALL,	"colon, :",
	IOR,		SMALL,	"inclusive or, |",
	AND,		SMALL,	"and, &",

	TILDE,		SMALL,	"one's complement, ~",
	ORNOT,		SMALL,	"ornot, !",
	CM,		SMALL,	"comma",

	LB,		SMALL,	"left bracket, [",
	RB,		SMALL,	"right bracket, ]",
	LP,		SMALL,	"left parenthesis, (",
	RP,		SMALL,	"right parentheis, )",

	LASTTOKEN,	SMALL,	"last token",
};
/*
 *	turn a token type into a string
 */
char *tok_to_name(token)
{
	static	int	fixed = 0;
	static	char	buf[64];
	static	struct	Tok_Desc 	NA = {0, (char *)0, "NOT ASSIGNED"};
		int	i;

	if (!fixed){
		for (i = FIRSTTOKEN; i <= LASTTOKEN; i++)
			tok_name[i] = &NA;
		for (i = 0; i <= sizeof(tok_desc)/sizeof(struct Tok_Desc); i++){
			tok_name[tok_desc[i].tok_which] = &tok_desc[i];
		}
		fixed = 1;
	}
	if (FIRSTTOKEN <= token && token <= LASTTOKEN){
		(void)sprintf(buf, "%s %s", tok_name[token]->tok_kind,
			tok_name[token]->tok_name);
		return(buf);
	} else {
		panic("Unknown token number, %d\n", token);
		/*NOTREACHED*/
	}
}
