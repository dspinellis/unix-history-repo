/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)asexpr.h 4.3 %G%
 */
/*
 *	Definitions to parse tokens
 */

#define ERROR(string)		yyerror(string); goto errorfix

#define peekahead (*tokptr)

#define shift 			val = yylex()
#define advance 	shift

#define shiftover(token)	if (val != token) { \
					yyerror("token expected"); \
					goto errorfix; \
				} \
				shift

#define advanceover 	shiftover

/*
 *	To speed up the expression processing, we class the input tokens
 *	into various sets.
 *
 *	We don't call the recursive descent expression analyzer if we can
 *	determine by looking at the next token after the first token in
 *	an expression that the expression is simple (name, integer or floating
 *	point value).  Expressions with operators are parsed using the recursive
 *	descent method.
 */

/*
 *	Functional forwards for expression utility routines
 */
struct	exp	*combine();
struct	exp	*boolterm();
struct	exp	*term();
struct	exp	*factor();
struct	exp	*yukkyexpr();

/*
 *	The set definitions
 */

extern	char	tokensets[(LASTTOKEN) - (FIRSTTOKEN) + 1];

#define	LINSTBEGIN	01	/*SEMI, NL, NAME*/
#define	EBEGOPS		02	/*LP, MINUS, TILDE*/
#define	YUKKYEXPRBEG	04	/*NAME, INSTn, INST0, REG, BFINT*/
#define	SAFEEXPRBEG	010	/*INT, FLTNUM*/
#define	ADDOPS		020	/*PLUS, MINUS*/
#define	BOOLOPS		040	/*IOR, XOR, AND*/
#define	MULOPS		0100	/*LSH, RSH, MUL, DIV, TILDE*/

#define	INTOKSET(val, set)	(tokensets[(val)] & (set) )

inttoktype	exprparse();
inttoktype	funnyreg();
inttoktype	yylex();

#define expr(xp, val) { \
	if ( (!INTOKSET(val, EBEGOPS)) && (!INTOKSET(peekahead, ADDOPS+BOOLOPS+MULOPS))) { \
		if (INTOKSET(val, YUKKYEXPRBEG)) xp = yukkyexpr(val, yylval); \
		else xp = (struct exp *) yylval; \
		shift; \
	} else { \
		val = exprparse(val, ptrloc1xp); \
		xp = loc1xp; \
	} \
    }

/*
 *	Registers can be either of the form r0...pc, or
 *	of the form % <expression>
 *	NOTE:	Reizers documentation on the assembler says that it
 *	can be of the form r0 + <expression>.. That's not true.
 *
 *	NOTE:	Reizer's yacc grammar would seem to allow an expression
 *	to be: (This is undocumented)
 *		a)	a register
 *		b)	an Instruction (INSTn or INST0)
 */

#define findreg(regno) \
	if (val == REG) { \
		regno = yylval; \
		shift; \
	} else \
	if (val == REGOP) { \
		shift;	/*over the REGOP*/ \
		val = funnyreg(val, ptrregno); \
	} \
	else { ERROR ("register expected"); }
