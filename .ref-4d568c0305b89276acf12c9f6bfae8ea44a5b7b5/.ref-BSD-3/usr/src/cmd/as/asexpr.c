/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include "as.h"
#include "asexpr.h"

/*
 * Tables for combination of operands.
 */

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
	register	type;

	lastnam=0; 			/* kludge for jxxx instructions */

	e1_type = exp1->xtype&XTYPE;
	e2_type = exp2->xtype&XTYPE;

	if (exp1->xtype==XXTRN+XUNDEF)
		e1_type = XTXRN;
	if (exp2->xtype==XXTRN+XUNDEF)
		e2_type = XTXRN;
	if (passno==1)
		if (exp1->xloc!=exp2->xloc && e1_type==e2_type)
			e1_type = e2_type = XTXRN;	/* error on != loc ctrs */
	e1_type >>= 1;		/*dispost of the external (XXTRN) bit*/
	e2_type >>= 1;

	switch (op) {
	case PLUS:
		exp1->xvalue += exp2->xvalue;
		type = pltab[e1_type][e2_type];
		break;
	case MINUS:
		exp1->xvalue -= exp2->xvalue;
		type = mintab[e1_type][e2_type];
		break;
	case IOR:
		exp1->xvalue |= exp2->xvalue;
		goto comm;
	case XOR:
		exp1->xvalue ^= exp2->xvalue;
		goto comm;
	case AND:
		exp1->xvalue &= exp2->xvalue;
		goto comm;
	case ORNOT:
		exp1->xvalue |= ~exp2->xvalue;
		goto comm;
	case LSH:
		exp1->xvalue <<= exp2->xvalue;
		goto comm;
	case RSH:
		exp1->xvalue >>= exp2->xvalue;
		goto comm;
	case TILDE:
		exp1->xvalue |= ~ exp2->xvalue;
		goto comm;
	case MUL:
		exp1->xvalue *= exp2->xvalue;
		goto comm;
	case DIV:
		if (exp2->xvalue == 0)
			yyerror("Divide check");
		else
			exp1->xvalue /= exp2->xvalue;
		goto comm;
	case REGOP:
		if (exp2->xvalue == 0)
			yyerror("Divide check (modulo)");
		else
			exp1->xvalue %= exp2->xvalue;
		goto comm;
	
	comm:
		type = othtab[e1_type][e2_type];
		break;
	default:
		yyerror("Internal error: unknown operator");
	}

	if (e2_type==(XTXRN>>1))
		exp1->xname = exp2->xname;
	exp1->xtype = type | (
			(exp1->xtype|exp2->xtype) & (XFORW|XXTRN) );
	if (type==ERR)
		yyerror("Relocation error");
	return(exp1);
}

buildtokensets()
{
#define clobber(val, set) tokensets[(val)] |= (set)

	clobber(SEMI,	LINSTBEGIN);
	clobber(NL,	LINSTBEGIN);

	clobber(NAME,	YUKKYEXPRBEG + LINSTBEGIN);
	clobber(INSTn,	YUKKYEXPRBEG);
	clobber(INST0,	YUKKYEXPRBEG);
	clobber(REG,	YUKKYEXPRBEG);

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
		lexpr->xtype = XABS;
		lexpr->xvalue = 0;
		lexpr = combine(op, lexpr, factor());
	}
	else {
		yyerror("Bad expression syntax");
		lexpr = xp++;
		lexpr->xtype = XABS;
		lexpr->xvalue = 0;
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
	if (val == NAME){
		exprisname++;
		locxp->xtype = ((struct symtab *)np)->type;
		if (( ((struct symtab *)np)->type&XTYPE)==XUNDEF) { /*forward*/
			locxp->xname = (struct symtab *)np;
			locxp->xvalue = 0;
			if (passno==1)
				((struct symtab *)np)->type |= XFORW;
		} else {	/*otherwise, just get the value*/
			locxp->xvalue = ((struct symtab *)np)->value;
			locxp->xname = NULL;
		}
	} else {	/*INSTn or INST0 or REG*/
		locxp->xtype = XABS;
		locxp->xvalue = ( (int)np) & 0xFF;
		locxp->xloc = 0;
		locxp->xname = NULL;
	}

	return(locxp);
}
