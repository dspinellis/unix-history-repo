/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */

#ifndef	TOKEN_H
#define	TOKEN_H

#include "zmath.h"


/*
 * Token types
 */
#define T_NULL			0	/* null token */
#define T_LEFTPAREN		1	/* left parenthesis "(" */
#define T_RIGHTPAREN		2	/* right parenthesis ")" */
#define T_LEFTBRACE		3	/* left brace "{" */
#define T_RIGHTBRACE		4	/* right brace "}" */
#define T_SEMICOLON		5	/* end of statement ";" */
#define T_EOF			6	/* end of file */
#define T_COLON			7	/* label character ":" */
#define T_ASSIGN		8	/* assignment "=" */
#define T_PLUS			9	/* plus sign "+" */
#define T_MINUS			10	/* minus sign "-" */
#define T_MULT			11	/* multiply sign "*" */
#define T_DIV			12	/* divide sign "/" */
#define T_MOD			13	/* modulo sign "%" */
#define T_POWER			14	/* power sign "^" or "**" */
#define T_EQ			15	/* equality "==" */
#define T_NE			16	/* notequal "!=" */
#define T_LT			17	/* less than "<" */
#define T_GT			18	/* greater than ">" */
#define T_LE			19	/* less than or equals "<=" */
#define T_GE			20	/* greater than or equals ">=" */
#define T_LEFTBRACKET		21	/* left bracket "[" */
#define T_RIGHTBRACKET		22	/* right bracket "]" */
#define T_SYMBOL		23	/* symbol name */
#define T_STRING		24	/* string value (double quotes) */
#define T_NUMBER		25	/* numeric real constant */
#define T_PLUSEQUALS		26	/* plus equals "+=" */
#define T_MINUSEQUALS		27	/* minus equals "-=" */
#define T_MULTEQUALS		28	/* multiply equals "*=" */
#define T_DIVEQUALS		29	/* divide equals "/=" */
#define T_MODEQUALS		30	/* modulo equals "%=" */
#define T_PLUSPLUS		31	/* plusplus "++" */
#define T_MINUSMINUS		32	/* minusminus "--" */
#define T_COMMA			33	/* comma "," */
#define T_ANDAND		34	/* logical and "&&" */
#define T_OROR			35	/* logical or "||" */
#define T_OLDVALUE		36	/* old value from previous calculation */
#define T_SLASHSLASH		37	/* integer divide "//" */
#define T_NEWLINE		38	/* newline character */
#define T_SLASHSLASHEQUALS	39	/* integer divide equals "//=" */
#define T_AND			40	/* arithmetic and "&" */
#define T_OR			41	/* arithmetic or "|" */
#define T_NOT			42	/* logical not "!" */
#define T_LEFTSHIFT		43	/* left shift "<<" */
#define T_RIGHTSHIFT		44	/* right shift ">>" */
#define T_ANDEQUALS		45	/* and equals "&=" */
#define T_OREQUALS		46	/* or equals "|= */
#define T_LSHIFTEQUALS		47	/* left shift equals "<<=" */
#define T_RSHIFTEQUALS		48	/* right shift equals ">>= */
#define T_POWEREQUALS		49	/* power equals "^=" or "**=" */
#define T_PERIOD		50	/* period "." */
#define T_IMAGINARY		51	/* numeric imaginary constant */
#define	T_AMPERSAND		52	/* ampersand "&" */
#define	T_QUESTIONMARK		53	/* question mark "?" */


/*
 * Keyword tokens
 */
#define T_IF			101	/* if keyword */
#define T_ELSE			102	/* else keyword */
#define T_WHILE			103	/* while keyword */
#define T_CONTINUE		104	/* continue keyword */
#define T_BREAK			105	/* break keyword */
#define T_GOTO			106	/* goto keyword */
#define T_RETURN		107	/* return keyword */
#define T_LOCAL			108	/* local keyword */
#define T_GLOBAL		109	/* global keyword */
#define	T_STATIC		110	/* static keyword */
#define T_DO			111	/* do keyword */
#define T_FOR			112	/* for keyword */
#define T_SWITCH		113	/* switch keyword */
#define T_CASE			114	/* case keyword */
#define T_DEFAULT		115	/* default keyword */
#define T_QUIT			116	/* quit keyword */
#define T_DEFINE		117	/* define keyword */
#define T_READ			118	/* read keyword */
#define T_SHOW			119	/* show keyword */
#define T_HELP			120	/* help keyword */
#define T_WRITE			121	/* write keyword */
#define T_MAT			122	/* mat keyword */
#define T_OBJ			123	/* obj keyword */
#define T_PRINT			124	/* print keyword */
#define T_USE			125	/* use keyword */


#define iskeyword(n) ((n) > 100)	/* TRUE if token is a keyword */


/*
 * Flags returned describing results of expression parsing.
 */
#define EXPR_RVALUE	0x0001		/* result is an rvalue */
#define EXPR_CONST	0x0002		/* result is constant */
#define EXPR_ASSIGN	0x0004		/* result is an assignment */

#define isrvalue(n)	((n) & EXPR_RVALUE)	/* TRUE if expression is rvalue */
#define islvalue(n)	(((n) & EXPR_RVALUE) == 0)	/* TRUE if expr is lvalue */
#define isconst(n)	((n) & EXPR_CONST)	/* TRUE if expr is constant */
#define isassign(n)	((n) & EXPR_ASSIGN)	/* TRUE if expr is an assignment */


/*
 * Flags for modes for tokenizing.
 */
#define TM_DEFAULT	0x0		/* normal mode */
#define TM_NEWLINES	0x1		/* treat any newline as a token */
#define TM_ALLSYMS	0x2		/* treat almost everything as a symbol */


extern long errorcount;		/* number of errors found */

extern char *tokenstring MATH_PROTO((void));
extern long tokennumber MATH_PROTO((void));
extern void inittokens MATH_PROTO((void));
extern int tokenmode MATH_PROTO((int flag));
extern int gettoken MATH_PROTO((void));
extern void rescantoken MATH_PROTO((void));

#ifdef VARARGS
extern void scanerror();
#else
extern void scanerror MATH_PROTO((int, char *, ...));
#endif

#endif

/* END CODE */
