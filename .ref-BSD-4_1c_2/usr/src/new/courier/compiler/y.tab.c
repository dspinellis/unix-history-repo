
# line 2 "courier.y"
#include "Courier.h"

# line 16 "courier.y"
typedef union  {
	int integer;
	struct object *object;
	list list;
} YYSTYPE;
# define identifier 257
# define number 258
# define ARRAY 259
# define _BEGIN 260
# define BOOLEAN 261
# define CARDINAL 262
# define CHOICE 263
# define DEPENDS 264
# define END 265
# define ERROR 266
# define INTEGER 267
# define LONG 268
# define OF 269
# define PROCEDURE 270
# define PROGRAM 271
# define RECORD 272
# define REPORTS 273
# define RETURNS 274
# define SEQUENCE 275
# define STRING 276
# define TYPE 277
# define UNSPECIFIED 278
# define UPON 279
# define VERSION 280
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 368 "courier.y"


#include "lex.yy.c"
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 63
# define YYLAST 217
short yyact[]={

  38,  87,  51,  20,  11,  85, 101,  52,   9,  16,
  83,  78,  76,   8,   5,  60,  58,  14,  53,  12,
 103,  47,  38,  26, 112,  82, 107,  93,  56,  86,
  45,  19,   3,  28, 117,  74,  99,  98,  79, 111,
  80,  96,  96, 114, 102, 109,  64,  57, 123,  81,
 113, 119,  24,  50,  22,  49,  48,  98,  89,  88,
   6,  77,  69,  21,  63, 104,  70,  23, 118,  75,
  31,  97,  25,  15,  18,  17,  10,   7,   4,   1,
  84, 100, 110,  71,  54, 108, 125,  59,  62,  55,
 121,  95,   2,  29,  61,  65,  30,   0,  72,  46,
   0,   0,   0,   0,   0,   0,   0,   0,  67,  68,
  92,   0,  94,  66,   0, 116,  73,   0,   0,   0,
   0,   0,   0,  91,   0,  90,   0,   0,   0,   0,
   0, 106,   0,   0,  45,   0,  39, 105,  32,  33,
  42, 115,   0,  44,  35,  34,   0,  43,   0,  41,
   0,   0,  40,  36,  27,  37,  45, 126,  39, 124,
  32,  33,  42, 122, 120,  44,  35,  34,   0,  43,
   0,  41,   0,   0,  40,  36,   0,  37,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,  13 };
short yypact[]={

-225,-1000,-246,   2,-1000,-251,-263,-1000,-275, -42,
-248,-226,-1000,-277,  17,-1000,  -4,   8,-1000,  32,
-235,-1000,-123,-1000,-226,-237,  -5,  -6,  -8,-1000,
-1000,-1000,-1000,-1000,-260,-1000,-1000,-1000,-229,-242,
-242, -45,-227, -45, -45,  16,-1000,  25,-1000,-101,
-242,-1000,-1000,-1000,  -9,-1000,  29,-257,-1000,-1000,
  15,-258,-1000,-1000,-232,-259,-1000,-269,-1000,-228,
-279,   0,  -1,-1000,-229,-242,-101,-230,-101,  -2,
-1000,  13,-1000, -87,-267, -47,-1000,-238,-1000,-1000,
-1000,  24,-1000,-1000,-1000,-1000,-232,-101,-231,-233,
-1000, -48,-232,-1000,-1000,-1000,-1000,-1000, -10,-1000,
   7,-1000,  29,-1000,-232,  -3,-1000,-233, -14,-233,
  -7,-1000,-1000,-101,-1000,-1000,-1000 };
short yypgo[]={

   0,  96,  95,  94,  47,  93,  92,  87,  70,  33,
  64,  45,  85,  50,  84,  39,  82,  81,  40,  38,
  49,  80,  79,  78,  77,  76,  75,  74,  73 };
short yyr1[]={

   0,  22,   6,   6,  23,  24,  24,  26,  26,  27,
  25,  25,  28,  28,   9,   9,   9,   5,   5,   5,
   5,   5,   5,   5,   5,   1,   1,   1,   1,   1,
   1,   1,   8,   8,  14,  14,  13,   3,   3,   4,
   4,   2,   2,  12,  12,  11,  16,  16,  15,  15,
  10,  10,  21,  21,  17,  17,  19,  19,  18,   7,
   7,  20,  20 };
short yyr2[]={

   0,   2,   4,   7,   5,   0,   4,   1,   3,   6,
   0,   2,   6,   6,   1,   1,   1,   1,   1,   2,
   1,   2,   1,   1,   2,   3,   4,   4,   2,   6,
   4,   2,   1,   3,   1,   3,   4,   1,   0,   1,
   1,   0,   1,   1,   3,   4,   1,   3,   1,   1,
   0,   3,   0,   4,   0,   4,   1,   3,   3,   1,
   3,   1,   3 };
short yychk[]={

-1000, -22,  -6, 257, -23, 260,  58, -24, 264, 271,
 -25, 279,  61, 258, 265, -28, 257, -26, -27, 257,
 280,  46,  58,  59,  44,  40, 258, 277,  -9,  -5,
  -1,  -8, 261, 262, 268, 267, 276, 278, 123, 259,
 275, 272, 263, 270, 266, 257, -27, 258,  61,  61,
  61, 262, 267, 278, -14, -13, 257,  -4, 258,  -7,
 257,  -3,  -4, -10,  91,  -2,  -8, -10, -10,  46,
  41,  -9,  -4, 125,  44,  40, 269,  46, 269, -19,
 -18, -20, 257, 269, -21, 274, 257, 280,  59,  59,
 -13,  -4,  -9, 257,  -9,  93,  44,  58,  44, 123,
 -17, 273,  91, 258,  41, -18,  -9, 257, -12, -11,
 -16, -15, 257, -13,  91, -19, 125,  44,  61,  44,
 -20,  93, -11,  62, -15,  93,  -9 };
short yydef[]={

   0,  -2,   0,   0,   1,   5,   0,  10,   0,   0,
   0,   0,   2,   0,   0,  11,   0,   0,   7,   0,
   0,   4,   0,   6,   0,   0,   0,   0,   0,  14,
  15,  16,  17,  18,   0,  20,  22,  23,   0,   0,
  38,  50,  41,  50,  50,  32,   8,   0,   3,   0,
   0,  19,  21,  24,   0,  34,   0,   0,  39,  40,
  59,   0,  37,  28,   0,   0,  42,  52,  31,   0,
   0,   0,   0,  25,   0,   0,   0,   0,   0,   0,
  56,   0,  61,   0,  54,   0,  33,   0,  12,  13,
  35,   0,  26,  60,  27,  51,   0,   0,   0,   0,
  30,   0,   0,   9,  36,  57,  58,  62,   0,  43,
   0,  46,  48,  49,   0,   0,  29,   0,   0,   0,
   0,  53,  44,   0,  47,  55,  45 };
#ifndef lint
static char yaccpar_sccsid[] = "@(#)yaccpar	4.1	(Berkeley)	2/11/83";
#endif not lint

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
# line 40 "courier.y"
 {
			program(yypvt[-1].object);
		} break;
case 2:
# line 47 "courier.y"
 {
			program_header(yypvt[-3].object);
			yyval.object = yypvt[-3].object;
		} break;
case 3:
# line 53 "courier.y"
 {
			program_header(yypvt[-6].object);
			yyval.object = yypvt[-6].object;
		} break;
case 6:
# line 66 "courier.y"
 {
			yyerror("Dependencies on other Courier programs are not supported");
		} break;
case 12:
# line 87 "courier.y"
 {
			compile_type(yypvt[-5].object, yypvt[-1].object);
		} break;
case 13:
# line 91 "courier.y"
 {
			if (type_check(yypvt[-3].object, yypvt[-1].object)) {
				compile_def(yypvt[-5].object, yypvt[-3].object, yypvt[-1].object);
			} else
				yyerror("Type clash in declaration of %s",
					name_of(yypvt[-5].object));
		} break;
case 14:
# line 102 "courier.y"
 {
			type_functions(yypvt[-0].object);
			yyval.object = yypvt[-0].object;
		} break;
case 15:
# line 107 "courier.y"
 {
			type_functions(yypvt[-0].object);
			yyval.object = yypvt[-0].object;
		} break;
case 16:
# line 112 "courier.y"
 {
			type_functions(yypvt[-0].object);
			yyval.object = yypvt[-0].object;
		} break;
case 17:
# line 120 "courier.y"
 {
			yyval.object = Boolean_type;
		} break;
case 18:
# line 124 "courier.y"
 {
			yyval.object = Cardinal_type;
		} break;
case 19:
# line 128 "courier.y"
 {
			yyval.object = LongCardinal_type;
		} break;
case 20:
# line 132 "courier.y"
 {
			yyval.object = Integer_type;
		} break;
case 21:
# line 136 "courier.y"
 {
			yyval.object = LongInteger_type;
		} break;
case 22:
# line 140 "courier.y"
 {
			yyval.object = String_type;
		} break;
case 23:
# line 144 "courier.y"
 {
			yyval.object = Unspecified_type;
		} break;
case 24:
# line 148 "courier.y"
 {
			yyval.object = LongUnspecified_type;
		} break;
case 25:
# line 155 "courier.y"
 {
			yyval.object = construct_type1(C_ENUMERATION, yypvt[-1].list);
		} break;
case 26:
# line 159 "courier.y"
 {
			yyval.object = construct_type2(C_ARRAY, yypvt[-2].object, yypvt[-0].object);
		} break;
case 27:
# line 163 "courier.y"
 {
			yyval.object = construct_type2(C_SEQUENCE, yypvt[-2].object, yypvt[-0].object);
		} break;
case 28:
# line 167 "courier.y"
 {
			yyval.object = construct_type1(C_RECORD, yypvt[-0].list);
		} break;
case 29:
# line 171 "courier.y"
 {
			yyval.object = construct_choice(yypvt[-4].object, yypvt[-1].list);
		} break;
case 30:
# line 175 "courier.y"
 {
			yyval.object = construct_procedure(yypvt[-2].list, yypvt[-1].list, yypvt[-0].list);
		} break;
case 31:
# line 179 "courier.y"
 {
			yyval.object = construct_type1(C_ERROR, yypvt[-0].list);
		} break;
case 32:
# line 186 "courier.y"
 {
			if (check_def(yypvt[-0].object))
				yyval.object = yypvt[-0].object;
			else
				yyval.object = Unspecified_type;
		} break;
case 33:
# line 193 "courier.y"
 {
			yyerror("References to types in other Courier programs are not supported");
			yyval.object = Unspecified_type;
		} break;
case 34:
# line 201 "courier.y"
 {
			yyval.list = cons(yypvt[-0].list, NIL);
		} break;
case 35:
# line 205 "courier.y"
 {
			yyval.list = nconc(yypvt[-2].list, cons(yypvt[-0].list, NIL));
		} break;
case 36:
# line 212 "courier.y"
 {
			yyval.list = cons(yypvt[-3].object, yypvt[-1].object);
		} break;
case 37:
# line 219 "courier.y"
 {
			yyval.object = yypvt[-0].object;
		} break;
case 38:
# line 223 "courier.y"
 {
			yyval.object = NIL;
		} break;
case 39:
# line 230 "courier.y"
 {
			yyval.object = yypvt[-0].object;
		} break;
case 40:
# line 234 "courier.y"
 {
			yyval.object = yypvt[-0].object;
		} break;
case 41:
# line 241 "courier.y"
 {
			yyval.object = NIL;
		} break;
case 42:
# line 245 "courier.y"
 {
			yyval.object = yypvt[-0].object;
		} break;
case 43:
# line 252 "courier.y"
 {
			yyval.list = cons(yypvt[-0].list, NIL);
		} break;
case 44:
# line 256 "courier.y"
 {
			yyval.list = nconc(yypvt[-2].list, cons(yypvt[-0].list, NIL));
		} break;
case 45:
# line 263 "courier.y"
 {
			yyval.list = cons(yypvt[-3].list, yypvt[-0].object);
		} break;
case 46:
# line 270 "courier.y"
 {
			yyval.list = cons(yypvt[-0].list, NIL);
		} break;
case 47:
# line 274 "courier.y"
 {
			yyval.list = nconc(yypvt[-2].list, cons(yypvt[-0].list, NIL));
		} break;
case 48:
# line 281 "courier.y"
 {
			yyval.list = cons(yypvt[-0].object, NIL);
		} break;
case 49:
# line 285 "courier.y"
 {
			yyval.list = yypvt[-0].list;
		} break;
case 50:
# line 292 "courier.y"
 {
			yyval.list = NIL;
		} break;
case 51:
# line 296 "courier.y"
 {
			yyval.list = yypvt[-1].list;
		} break;
case 52:
# line 303 "courier.y"
 {
			yyval.list = NIL;
		} break;
case 53:
# line 307 "courier.y"
 {
			yyval.list = yypvt[-1].list;
		} break;
case 54:
# line 314 "courier.y"
 {
			yyval.list = NIL;
		} break;
case 55:
# line 318 "courier.y"
 {
			yyval.list = yypvt[-1].list;
		} break;
case 56:
# line 325 "courier.y"
 {
			yyval.list = cons(yypvt[-0].list, NIL);
		} break;
case 57:
# line 329 "courier.y"
 {
			yyval.list = nconc(yypvt[-2].list, cons(yypvt[-0].list, NIL));
		} break;
case 58:
# line 336 "courier.y"
 {
			yyval.list = cons(yypvt[-2].list, yypvt[-0].object);
		} break;
case 59:
# line 344 "courier.y"
 {
			if (check_def(yypvt[-0].object))
				yyval.object = yypvt[-0].object;
			else
				yyval.object = Undefined_constant;
		} break;
case 60:
# line 351 "courier.y"
 {
			yyerror("References to constants in other Courier programs are not supported");
			yyval.object = Undefined_constant;
		} break;
case 61:
# line 359 "courier.y"
 {
			yyval.list = cons(yypvt[-0].object, NIL);
		} break;
case 62:
# line 363 "courier.y"
 {
			yyval.list = nconc(yypvt[-2].list, cons(yypvt[-0].object, NIL));
		} break;
		}
		goto yystack;  /* stack new state and value */

	}
