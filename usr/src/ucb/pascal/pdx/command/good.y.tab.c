
# line 2 "grammar.yacc"
/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)grammar.yacc 1.2 1/20/82";

/*
 * yacc grammar for debugger commands
 */

#include "defs.h"
#include "command.h"
#include "sym.h"
#include "symtab.h"
#include "tree.h"
#include "process.h"
#include "source.h"


# line 43 "grammar.yacc"
typedef union  {
	SYM *y_sym;
	NODE *y_node;
	int y_int;
	OP y_op;
	long y_long;
	double y_real;
	char *y_string;
	BOOLEAN y_bool;
} YYSTYPE;
# define ALIAS 257
# define ASSIGN 258
# define CALL 259
# define CHFILE 260
# define CONT 261
# define DUMP 262
# define EDIT 263
# define GRIPE 264
# define HELP 265
# define LIST 266
# define NEXT 267
# define QUIT 268
# define REMAKE 269
# define PRINT 270
# define RUN 271
# define SH 272
# define SOURCE 273
# define STATUS 274
# define STEP 275
# define STOP 276
# define STOPI 277
# define TRACE 278
# define TRACEI 279
# define DELETE 280
# define WHATIS 281
# define WHICH 282
# define WHERE 283
# define XI 284
# define XD 285
# define AT 286
# define IN 287
# define IF 288
# define FILENAME 289
# define INT 290
# define REAL 291
# define NAME 292
# define STRING 293
# define DIV 294
# define MOD 295
# define AND 296
# define OR 297
# define NOT 298
# define UNARYSIGN 299
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 530 "grammar.yacc"


/*
 * parser error handling
 */

yyerror(s)
char *s;
{
	if (strcmp(s, "syntax error") == 0) {
		error("bad command syntax");
	} else {
		error(s);
	}
}

/*
 * In recovering from an error we gobble input up to a newline.
 */

gobble()
{
	register int t;

	if (!nlflag) {
		while ((t = yylex()) != '\n' && t != 0);
	}
}
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 140,
	60, 0,
	61, 0,
	62, 0,
	-2, 79,
	};
# define YYNPROD 106
# define YYLAST 370
short yyact[]={

   4,  48, 135,  79,  44,  50,  45,  47, 127,  69,
  76,  77,  90,  89,  87,  86,  83,  76,  93, 150,
 109,  56,  40, 145, 104, 100, 109, 101, 143, 105,
 104, 100, 130, 101, 152, 105, 109,  55, 141, 142,
 104,  55, 110, 112, 111, 105,  55, 109, 110, 112,
 111, 104, 100,  96, 101, 109, 105,  95,  39, 104,
 100, 122, 101,  92, 105,  49,  94, 119, 151, 110,
 112, 111,  75,  80,  53,  42, 109, 110, 112, 111,
 104, 100,  66, 101,  41, 105,   8,   3,  67,   2,
   1,  62,  51,  85,  60,  59,  61,  93,  82,  62,
 132,  74,  60,   6,  61,  57, 103,  73,  62, 133,
  58,  60, 103,  61, 134,  93,  62,   5, 137,  60,
  46,  61, 113,  71,  71,  99,  98,  88,  93,  81,
  81,  78,  97, 103, 124, 144,  17,  84,  18, 117,
 118, 103,  92,   0, 129,  94, 126, 128,   0, 131,
   0,   0,  91,   0,   0, 149, 125, 136,   0,   0,
  92,   0, 103,  94,   0,   0, 114, 115, 116,   0,
   0,   0,   0,  92, 120,   0,  94,   0,   0,   0,
 121,   0, 120,   0,   0, 146,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 147,   0,
   0,   0,   0, 138, 139, 140,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,  43,   0,   0, 148,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,  25,   9,  26,
  10,  11,  28,  27,  29,  30,  12,  13,  15,  31,
  14,  34,   7,  32,  33,  16,  35,  36,  37,  38,
  19,  20,  21,  22,  23,  24, 106, 107, 108, 102,
   0,   0, 106, 107, 108, 102,   0,   0,   0,   0,
 123,  54, 106, 107, 108,  54,   0,  52,   0,   0,
  54,   0,   0, 106, 107, 108, 102,   0,   0,   0,
   0, 106, 107, 108, 102,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, 106, 107, 108, 102,   0,  48,   0,   0,
   0,  63,  64,  47,  65,  70,  69,  76,  72,  63,
  64,  47,  65,   0,  70,  69,  68,  72,  63,  64,
  47,  65,  48,   0,   0,   0,  63,  64,  47,  65 };
short yypact[]={

-1000, -10,-1000,  48,-1000,-1000, -40,-1000, -56,-285,
-284,-1000,   5,-1000,  76,-1000,-1000,  68,  59,-279,
-285,-289,-1000,  76,  76,-273,-285,-284,-1000,-1000,
-1000,-1000,-274,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-275,-1000, -56,-1000,-276,-277,  51,-1000,-1000,-1000,
-1000,-1000,-1000,  13,-1000,-1000,-1000,   9,  82,-1000,
  76,  76,  76,-1000,-1000,-1000,-271,-271,  76,-285,
   1,  17,  10,-278,-271,-1000,  76,-1000, -28,-1000,
-1000, -12,-1000,-284,  69,-1000,-1000,-1000,-1000,-1000,
-1000,  17,  76,-290,-1000,  10,  76,  76,  76,  76,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
 -23, -33,-1000,  76,  -2,  -2, -18,-1000,-1000,-1000,
  17, -28,-1000,  10,-1000,-271,-1000,   1,-1000,-1000,
  76,-1000,-1000,  76, -74,-1000,-1000,-1000,  -2,-1000,
  38,-1000,-1000,-1000,  27,-1000,-1000,-1000,  17,  -7,
-1000,-1000,-1000 };
short yypgo[]={

   0, 138, 136, 132, 126, 125,  65, 117, 103,  88,
  82, 100,  72,  21, 105,  67, 110,  95,  92,  61,
  73,  90,  89,  87,  86,  84,  75 };
short yyr1[]={

   0,  21,  21,  22,  22,  23,  23,  23,  23,  23,
  24,  25,  25,  26,  26,  26,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   8,   8,
   8,   8,   8,   8,   8,   8,   8,   8,   1,   1,
   2,   2,   9,   9,  10,  10,  10,   6,   6,  11,
  11,  18,  18,  18,  19,  19,  20,  20,  12,  12,
  13,  13,  14,  14,  14,  14,  14,  14,  14,  14,
  14,  15,  16,  16,  16,  16,  16,  17,  17,  17,
   3,   3,   3,   3,   4,   4,   4,   4,   4,   4,
   5,   5,   5,   5,   5,   5 };
short yyr2[]={

   0,   2,   0,   2,   1,   1,   1,   3,   1,   2,
   1,   2,   0,   1,   2,   2,   3,   2,   1,   2,
   2,   1,   2,   1,   1,   3,   3,   3,   4,   3,
   3,   2,   2,   2,   2,   1,   2,   2,   3,   1,
   3,   2,   1,   1,   1,   1,   2,   1,   1,   1,
   1,   1,   1,   2,   2,   2,   3,   0,   1,   0,
   3,   0,   1,   3,   1,   1,   1,   3,   0,   2,
   1,   3,   1,   4,   1,   2,   2,   3,   3,   3,
   3,   1,   1,   1,   4,   3,   2,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   2,   1,   2,   1,   2 };
short yychk[]={

-1000, -21, -22, -23,  10,  -7,  -8, 272, -24, 258,
 260, 261, 266, 267, 270, 268, 275,  -2,  -1, 280,
 281, 282, 283, 284, 285, 257, 259, 263, 262, 264,
 265, 269, 273, 274, 271, 276, 277, 278, 279,  10,
  62, -25, -26, 289,  60,  62, -16, 292, 286,  -6,
 289, -18, 292, -19, 290,  36, -13, -14, -16, -17,
  43,  45,  40, 290, 291, 293, -10,  -9, 288, 287,
 286, -14, 289,  -9, -10, -12, 288, 290, -16, 292,
 -20, -14, -20, 289, -16,  -6, 289, 289, -25, 289,
 289, -14,  91,  46,  94,  44,  44,  -3,  -4,  -5,
  43,  45, 297, 124,  42,  47, 294, 295, 296,  38,
  60,  62,  61,  40, -14, -14, -14, -12, -12, -15,
 -14, -16, -19, 289, -19, -10, -12, 286, -12, -15,
  44,  -6, -11,  40, -13, 292, -19, -13, -14, -14,
 -14,  61,  62,  61, -13,  41, -19, -12, -14, -13,
  93,  41,  41 };
short yydef[]={

   2,  -2,   1,   0,   4,   5,   6,   8,  12,   0,
  57,  18,  61,  21,   0,  23,  24,   0,  68,   0,
   0,   0,  35,   0,   0,  39,   0,  57,  42,  43,
  44,  45,   0,  47,  10,  50,  51,  48,  49,   3,
   0,   9,  12,  13,   0,   0,   0,  82,  83,  17,
  58,  19,  20,  62,  64,  65,  22,  70,  72,  74,
   0,   0,   0,  87,  88,  89,  68,  68,   0,   0,
  83,  52,   0,  68,  68,  31,   0,  32,  33,  34,
  36,  66,  37,  57,  59,  41,  46,   7,  11,  14,
  15,  16,   0,   0,  86,   0,   0,   0,   0,   0,
  90,  91,  92,  93,  94,  95,  96,  97,  98,  99,
 100, 102, 104,   0,  75,  76,   0,  25,  26,  27,
  81,  54,  55,   0,  53,  68,  30,   0,  29,  69,
   0,  38,  40,   0,   0,  85,  63,  71,  77,  78,
  -2, 101, 105, 103,   0,  80,  56,  28,  67,   0,
  84,  73,  60 };
#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 1:
# line 66 "grammar.yacc"
{
		prompt();
} break;
case 5:
# line 83 "grammar.yacc"
{
		eval(yypvt[-0].y_node);
} break;
case 6:
# line 87 "grammar.yacc"
{
		eval(yypvt[-0].y_node);
} break;
case 7:
# line 91 "grammar.yacc"
{
		setout(yypvt[-0].y_string);
		eval(yypvt[-2].y_node);
		unsetout();
} break;
case 8:
# line 97 "grammar.yacc"
{
		shell(yypvt[-0].y_string);
} break;
case 9:
# line 101 "grammar.yacc"
{
		run();
} break;
case 10:
# line 107 "grammar.yacc"
{
		arginit();
} break;
case 13:
# line 117 "grammar.yacc"
{
		newarg(yypvt[-0].y_string);
} break;
case 14:
# line 121 "grammar.yacc"
{
		inarg(yypvt[-0].y_string);
} break;
case 15:
# line 125 "grammar.yacc"
{
		outarg(yypvt[-0].y_string);
} break;
case 16:
# line 131 "grammar.yacc"
{
		yyval.y_node = build(O_ASSIGN, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 17:
# line 135 "grammar.yacc"
{
		yyval.y_node = build(O_CHFILE, yypvt[-0].y_string);
} break;
case 18:
# line 139 "grammar.yacc"
{
		yyval.y_node = build(O_CONT);
} break;
case 19:
# line 143 "grammar.yacc"
{
		yyval.y_node = build(O_LIST, yypvt[-0].y_node);
} break;
case 20:
# line 147 "grammar.yacc"
{
		yyval.y_node = build(O_LIST, build(O_NAME, yypvt[-0].y_sym));
} break;
case 21:
# line 151 "grammar.yacc"
{
		yyval.y_node = build(O_NEXT);
} break;
case 22:
# line 155 "grammar.yacc"
{
		yyval.y_node = build(O_PRINT, yypvt[-0].y_node);
} break;
case 23:
# line 159 "grammar.yacc"
{
		quit(0);
} break;
case 24:
# line 163 "grammar.yacc"
{
		yyval.y_node = build(O_STEP);
} break;
case 25:
# line 167 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-2].y_int, NIL, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 26:
# line 171 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-2].y_int, yypvt[-1].y_node, NIL, yypvt[-0].y_node);
} break;
case 27:
# line 175 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-2].y_int, NIL, NIL, yypvt[-0].y_node);
} break;
case 28:
# line 179 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-3].y_int, yypvt[-2].y_node, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 29:
# line 183 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-2].y_int, NIL, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 30:
# line 187 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-2].y_int, yypvt[-1].y_node, NIL, yypvt[-0].y_node);
} break;
case 31:
# line 191 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-1].y_int, NIL, NIL, yypvt[-0].y_node);
} break;
case 32:
# line 195 "grammar.yacc"
{
		yyval.y_node = build(O_DELETE, yypvt[-0].y_long);
} break;
case 33:
# line 199 "grammar.yacc"
{
		yyval.y_node = build(O_WHATIS, yypvt[-0].y_node);
} break;
case 34:
# line 203 "grammar.yacc"
{
		yyval.y_node = build(O_WHICH, yypvt[-0].y_sym);
} break;
case 35:
# line 207 "grammar.yacc"
{
		yyval.y_node = build(O_WHERE);
} break;
case 36:
# line 211 "grammar.yacc"
{
		yyval.y_node = build(O_XI, yypvt[-0].y_node);
} break;
case 37:
# line 215 "grammar.yacc"
{
		yyval.y_node = build(O_XD, yypvt[-0].y_node);
} break;
case 38:
# line 221 "grammar.yacc"
{
		yyval.y_node = build(O_ALIAS, yypvt[-1].y_string, yypvt[-0].y_string);
} break;
case 39:
# line 225 "grammar.yacc"
{
		yyval.y_node = build(O_ALIAS, NIL, NIL);
} break;
case 40:
# line 229 "grammar.yacc"
{
		yyval.y_node = build(O_CALL, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 41:
# line 233 "grammar.yacc"
{
		yyval.y_node = build(O_EDIT, yypvt[-0].y_string);
} break;
case 42:
# line 237 "grammar.yacc"
{
		yyval.y_node = build(O_DUMP);
} break;
case 43:
# line 241 "grammar.yacc"
{
		yyval.y_node = build(O_GRIPE);
} break;
case 44:
# line 245 "grammar.yacc"
{
		yyval.y_node = build(O_HELP);
} break;
case 45:
# line 249 "grammar.yacc"
{
		yyval.y_node = build(O_REMAKE);
} break;
case 46:
# line 253 "grammar.yacc"
{
		yyval.y_node = build(O_SOURCE, yypvt[-0].y_string);
} break;
case 47:
# line 257 "grammar.yacc"
{
		yyval.y_node = build(O_STATUS);
} break;
case 48:
# line 263 "grammar.yacc"
{
		yyval.y_int = O_TRACE;
} break;
case 49:
# line 267 "grammar.yacc"
{
		yyval.y_int = O_TRACEI;
} break;
case 50:
# line 273 "grammar.yacc"
{
		yyval.y_int = O_STOP;
} break;
case 51:
# line 277 "grammar.yacc"
{
		yyval.y_int = O_STOPI;
} break;
case 53:
# line 284 "grammar.yacc"
{
		yyval.y_node = build(O_QLINE, yypvt[-1].y_string, yypvt[-0].y_node);
} break;
case 54:
# line 290 "grammar.yacc"
{
		yyval.y_node = yypvt[-0].y_node;
} break;
case 55:
# line 294 "grammar.yacc"
{
		yyval.y_node = build(O_QLINE, cursource, yypvt[-0].y_node);
} break;
case 56:
# line 298 "grammar.yacc"
{
		yyval.y_node = build(O_QLINE, yypvt[-1].y_string, yypvt[-0].y_node);
} break;
case 57:
# line 304 "grammar.yacc"
{
		yyval.y_string = NIL;
} break;
case 59:
# line 311 "grammar.yacc"
{
		yyval.y_node = NIL;
} break;
case 60:
# line 315 "grammar.yacc"
{
		yyval.y_node = yypvt[-1].y_node;
} break;
case 61:
# line 321 "grammar.yacc"
{
		NODE *first, *last;

		first = build(O_LCON, (long) 1);
		last = build(O_LCON, (long) lastlinenum);
		yyval.y_node = build(O_COMMA, first, last);
} break;
case 62:
# line 329 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-0].y_node, yypvt[-0].y_node);
} break;
case 63:
# line 333 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 64:
# line 339 "grammar.yacc"
{
		yyval.y_node = build(O_LCON, yypvt[-0].y_long);
} break;
case 65:
# line 343 "grammar.yacc"
{
		yyval.y_node = build(O_LCON, (long) lastlinenum);
} break;
case 66:
# line 349 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-0].y_node, yypvt[-0].y_node);
} break;
case 67:
# line 353 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 68:
# line 359 "grammar.yacc"
{
		yyval.y_node = NIL;
} break;
case 69:
# line 363 "grammar.yacc"
{
		yyval.y_node = yypvt[-0].y_node;
} break;
case 70:
# line 369 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-0].y_node, NIL);
} break;
case 71:
# line 373 "grammar.yacc"
{
		yyval.y_node = build(O_COMMA, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 72:
# line 379 "grammar.yacc"
{
		yyval.y_node = build(O_RVAL, yypvt[-0].y_node);
} break;
case 73:
# line 383 "grammar.yacc"
{
		yyval.y_node = build(O_CALL, yypvt[-3].y_node, yypvt[-1].y_node);
} break;
case 75:
# line 388 "grammar.yacc"
{
		yyval.y_node = yypvt[-0].y_node;
} break;
case 76:
# line 392 "grammar.yacc"
{
		yyval.y_node = build(O_NEG, yypvt[-0].y_node);
} break;
case 77:
# line 396 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-1].y_op, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 78:
# line 400 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-1].y_op, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 79:
# line 404 "grammar.yacc"
{
		yyval.y_node = build(yypvt[-1].y_op, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 80:
# line 408 "grammar.yacc"
{
		yyval.y_node = yypvt[-1].y_node;
} break;
case 81:
# line 414 "grammar.yacc"
{
		chkboolean(yyval.y_node = yypvt[-0].y_node);
} break;
case 82:
# line 420 "grammar.yacc"
{
		yyval.y_node = build(O_NAME, yypvt[-0].y_sym);
} break;
case 83:
# line 424 "grammar.yacc"
{
		SYM *s;

		s = st_lookup(symtab, "at");
		if (s == NIL) {
			error("\"at\" is not defined");
		}
		yyval.y_node = build(O_NAME, s);
} break;
case 84:
# line 434 "grammar.yacc"
{
		yyval.y_node = subscript(yypvt[-3].y_node, yypvt[-1].y_node);
} break;
case 85:
# line 438 "grammar.yacc"
{
		yyval.y_node = dot(yypvt[-2].y_node, yypvt[-0].y_sym);
} break;
case 86:
# line 442 "grammar.yacc"
{
		yyval.y_node = build(O_INDIR, yypvt[-1].y_node);
} break;
case 87:
# line 448 "grammar.yacc"
{
		yyval.y_node = build(O_LCON, yypvt[-0].y_long);
} break;
case 88:
# line 452 "grammar.yacc"
{
		yyval.y_node = build(O_FCON, yypvt[-0].y_real);
} break;
case 89:
# line 456 "grammar.yacc"
{
		yyval.y_node = build(O_SCON, yypvt[-0].y_string);
} break;
case 90:
# line 462 "grammar.yacc"
{
		yyval.y_op = O_ADD;
} break;
case 91:
# line 466 "grammar.yacc"
{
		yyval.y_op = O_SUB;
} break;
case 92:
# line 470 "grammar.yacc"
{
		yyval.y_op = O_OR;
} break;
case 93:
# line 474 "grammar.yacc"
{
		yyval.y_op = O_OR;
} break;
case 94:
# line 480 "grammar.yacc"
{
		yyval.y_op = O_MUL;
} break;
case 95:
# line 484 "grammar.yacc"
{
		yyval.y_op = O_DIVF;
} break;
case 96:
# line 488 "grammar.yacc"
{
		yyval.y_op = O_DIV;
} break;
case 97:
# line 492 "grammar.yacc"
{
		yyval.y_op = O_MOD;
} break;
case 98:
# line 496 "grammar.yacc"
{
		yyval.y_op = O_AND;
} break;
case 99:
# line 500 "grammar.yacc"
{
		yyval.y_op = O_AND;
} break;
case 100:
# line 506 "grammar.yacc"
{
		yyval.y_op = O_LT;
} break;
case 101:
# line 510 "grammar.yacc"
{
		yyval.y_op = O_LE;
} break;
case 102:
# line 514 "grammar.yacc"
{
		yyval.y_op = O_GT;
} break;
case 103:
# line 518 "grammar.yacc"
{
		yyval.y_op = O_GE;
} break;
case 104:
# line 522 "grammar.yacc"
{
		yyval.y_op = O_EQ;
} break;
case 105:
# line 526 "grammar.yacc"
{
		yyval.y_op = O_NE;
} break;
		}
		goto yystack;  /* stack new state and value */

	}
