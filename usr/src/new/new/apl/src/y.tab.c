
# line 2 "apl.y"
static char apl_y_Sccsid[] = "apl.y @(#)apl.y	1.3	10/5/82 Berkeley ";

# line 4 "apl.y"
typedef union  {
	char	*charptr;
	char	charval;
} YYSTYPE;
# define lex0 257
# define lex1 258
# define lex2 259
# define lex3 260
# define lex4 261
# define lex5 262
# define lex6 263
# define lpar 264
# define rpar 265
# define lbkt 266
# define rbkt 267
# define eol 268
# define unk 269
# define com 270
# define com0 271
# define Quad 272
# define asg 273
# define null 274
# define dot 275
# define cln 276
# define semi 277
# define comnt 278
# define tran 279
# define strng 280
# define nam 281
# define numb 282
# define nfun 283
# define mfun 284
# define dfun 285
# define comexpr 286
# define comnam 287
# define comnull 288
# define comlist 289
# define dscal 290
# define mdscal 291
# define m 292
# define d 293
# define md 294
# define msub 295
# define mdsub 296

# line 26 "apl.y"
#include "apl.h"
	int	vcount;
	int	scount;
	int	litflag;
	int	nlexsym;
	int	context;
	char	*iline;
	char	*ccharp, *ccharp2;
	data	lnumb;		/* current label number */
	char	*labcpp;	/* label prologue */
	char	*labcpe;	/* label epilogue */
	int	immedcmd;	/* immediate command number */
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 515 "apl.y"

#include "tab.c"
#include "lex.c"
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 31,
	266, 69,
	-2, 67,
-1, 33,
	266, 70,
	-2, 90,
-1, 34,
	270, 91,
	271, 91,
	275, 91,
	-2, 88,
-1, 57,
	276, 93,
	-2, 48,
-1, 58,
	276, 94,
	-2, 47,
-1, 59,
	276, 95,
	-2, 63,
-1, 77,
	275, 92,
	-2, 81,
-1, 81,
	266, 87,
	-2, 85,
-1, 83,
	266, 86,
	-2, 90,
	};
# define YYNPROD 97
# define YYLAST 278
short yyact[]={

  28,  35, 130,  43,  11,  44,  45,  46,  29,  61,
  62,  64,  63,  38,  14,  55,  26,  57,  38,  58,
  59,  46, 126,  69, 103, 132,  35,  34,  30,  98,
  37,  31,  33,  28,  13, 133, 127,  11,  97, 116,
 115,  29,  90,  91, 109, 104,  67,  14,  10,  26,
  25,  38,  24,  22,  28, 134,  42,  88,  11,  35,
  34,  30,  29,  37,  31,  33,  86, 122,  14, 118,
  26,  25,  38,  24,  22,  28,  68,  16,  96, 101,
  35,  34,  30,  29,  37,  31,  33, 107,  50,   9,
   1,  26,  25,  38,  24,  22,  28,  82,  71,  89,
  65,  35,  34,  30,  29,  37,  31,  33,  87,  72,
  21,  23,  26,  25,  38,  24,  22,  28,  20,  27,
  85,  15,  35,  34,  30,  29,  37,  31,  33,  75,
  52,  60,  66,  26,  25,  38,  24,  22,  81,  79,
  84,  80,  74,  35,  34,  30, 112,  37,  31,  33,
  92, 123,  41,  73,  53,  94,  93,  32,  77,  34,
  56,  78,  37,  43,  83,  44,  45,  46,   2,   3,
   4,   5,   6,   7,  70,  76,  54, 102,  40,   8,
  39, 114,  99, 105,  48,  49,  18, 117,  36,  19,
 111,  17, 113, 110,  95,  12,  51,  47,   0, 121,
   0,   0,   0, 136,  56, 135, 100, 119,   0,   0,
 120,   0,   0,   0,   0, 106, 108,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 129, 131,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 124, 125,   0,
   0,   0,   0,   0,   0, 119,   0,   0,   0,   0,
 128,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,  95 };
short yypact[]={

 -89,-1000,-231,-210,-278,-278,-278,-264,-1000,-277,
-168,-1000,-222,-1000,-1000,-254,-1000,-132,-147,-200,
-1000,-209,-1000,-228,-1000,-1000,-1000,-1000,-147,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-269,-1000,-1000,-1000,
-1000,-118,-1000,-1000,-1000,-1000,-1000,-239,-1000,-1000,
-1000,-264,-1000,-1000,-1000,-189,-252,-1000,-1000,-1000,
-223,-147,-278,-278,-1000,-1000,-224,-1000,-147,-1000,
-147,-1000,-209,-1000,-235,-236,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-147,-1000,-1000,-147,-1000,
-209,-1000,-198,-1000,-278,-278,-1000,-259,-1000,-1000,
-1000,-1000,-232,-1000,-1000,-1000,-1000,-278,-1000,-1000,
-1000,-254,-1000,-1000,-1000,-289,-289,-242,-1000,-1000,
-212,-1000,-1000,-1000,-278,-1000,-239,-1000,-1000,-1000,
-1000,-1000,-1000,-147,-1000,-1000,-1000 };
short yypgo[]={

   0, 178,  56, 197,  78, 196, 154, 130, 176, 195,
 193, 121, 191, 189, 188, 187,  69, 186, 174, 108,
 152,  77, 131, 120, 118, 110, 109,  99,  98,  97,
 157, 111,  90,  89,  88,  87,  76, 119 };
short yyr1[]={

   0,  32,  32,  32,  32,  32,  32,  32,  32,  32,
   1,   1,   2,   3,   3,   3,   4,   4,  33,  22,
  22,  22,  22,  35,  35,  34,  34,   5,   5,   6,
   7,   7,   7,   8,   8,   9,   9,   9,  21,  10,
  10,  36,  11,  11,  11,  12,  12,  13,  13,  13,
  13,  13,  13,  37,  37,  14,  23,  15,  15,  16,
  16,  17,  17,  17,  17,  17,  24,  24,  24,  25,
  25,  18,  18,  18,  18,  18,  26,  19,  27,  27,
  28,  28,  28,  28,  28,  28,  29,  29,  30,  30,
  30,  31,  31,  20,  20,  20,  20 };
short yyr2[]={

   0,   2,   4,   3,   4,   2,   2,   2,   2,   2,
   3,   1,   2,   3,   2,   1,   3,   1,   1,   2,
   2,   2,   1,   2,   1,   2,   1,   1,   2,   2,
   1,   2,   3,   1,   2,   1,   1,   1,   3,   1,
   1,   1,   1,   2,   3,   1,   4,   1,   1,   1,
   1,   3,   1,   2,   1,   1,   1,   1,   3,   1,
   0,   1,   2,   1,   2,   3,   1,   1,   1,   1,
   1,   1,   2,   1,   3,   3,   1,   3,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1 };
short yychk[]={

-1000, -32, 257, 258, 259, 260, 261, 262,  -8, -33,
 279, 268,  -9, 265, 278, -11, -21, -12, -17, -13,
 -24, -25, 284, -31, 283, 281, 280, -37, 264, 272,
 292, 295, -30, 296, 291, 290, -14, 294, 282,  -8,
  -1, -20,  -2, 281, 283, 284, 285,  -3,  -1,  -1,
 -34,  -5,  -7,  -6,  -8, 279, -20, 281, 283, 284,
 -22, 286, 287, 289, 288, 268, -11, 268, -36, 277,
 -18, -28, -26, 285, 274, -31, -30, 290, 293, 271,
 273, 270, -29, 296, -11, -23, 266, -19, 266, -27,
 270, 271, -11, -37, 273, -20,  -4, 277, 268,  -7,
  -6, 268, -11, 276, 268, -11, -20, -35, -20, 268,
 -10, -11, -21, -11, -19, 275, 275, -15, -16, -11,
 -11, -19, 265,  -2, -20, -20, 281, 268, -20, -31,
 291, -31, 267, 277, 267,  -4, -16 };
short yydef[]={

   0,  -2,   0,   0,   0,   0,   0,   0,   1,   0,
   0,  33,   0,  18,  35,  36,  37,  42,   0,  45,
  61,   0,  63,   0,  47,  48,  49,  50,   0,  52,
  66,  -2,  68,  -2,  -2,  92,  54,  89,  55,   5,
   6,  15,  11,  93,  94,  95,  96,   0,   7,   8,
   9,   0,  26,  27,  30,   0,   0,  -2,  -2,  -2,
   0,   0,   0,   0,  22,   3,   0,  34,   0,  41,
   0,  71,   0,  73,   0,   0,  80,  -2,  82,  83,
  84,  -2,  76,  -2,  43,  60,  56,  62,   0,  64,
  78,  79,   0,  53,   0,  14,  12,   0,  17,  25,
  28,  31,   0,  29,   2,  19,  20,  21,  24,   4,
  38,  39,  40,  44,  72,   0,   0,   0,  57,  59,
   0,  65,  51,  10,  15,  13,   0,  32,  23,  74,
  91,  75,  46,  60,  77,  16,  58 };
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
# line 51 "apl.y"

	{
		integ = ccharp[-1];
		if(integ != ASGN && integ != PRINT && integ != COMNT)
			*ccharp++ = PRINT;
		*ccharp++ = EOL;
	} break;
case 2:
# line 58 "apl.y"

	{
		*ccharp++ = IMMED;
		*ccharp++ = yypvt[-1].charval;
	} break;
case 3:
# line 66 "apl.y"

	{
		*ccharp++ = SICLR0;
	} break;
case 4:
# line 70 "apl.y"

	{
		*ccharp++ = SICLR;
	} break;
case 10:
# line 107 "apl.y"

	{
		switch(context) {

		case lex3:
			name(yyval.charptr, AUTO);
			/*
			 * see comments in ai.c/funcomp() concerning
			 * label processing.
			 */
			*ccharp++ = ELID;
			break;

		case lex4:
			ccharp2 = ccharp;
			*ccharp++ = EOL;
			name(yyval.charptr, RVAL);
			name(yyval.charptr, REST);
			invert(yypvt[-0].charptr, ccharp2);
		}
	} break;
case 11:
# line 128 "apl.y"

	{
		if(context == lex3)
			*ccharp++ = ELID;
		if(context == lex4){
			*ccharp++ = EOL;	/* pop previous result */
			*ccharp++ = NILRET;	/* return empty result */
		}
	} break;
case 12:
# line 138 "apl.y"

	{
		if(context == lex4)
			invert(yyval.charptr, yypvt[-0].charptr);
	} break;
case 13:
# line 145 "apl.y"

	{
		yyval.charptr = ccharp;
		switch(context) {

		case lex2:
			name(yypvt[-1].charptr, DF);
			break;

		case lex3:
			name(yypvt[-0].charptr, ARG2);
			name(yypvt[-2].charptr, ARG1);
			break;

		case lex4:
			name(yypvt[-2].charptr, REST);
			name(yypvt[-0].charptr, REST);
		}
	} break;
case 14:
# line 164 "apl.y"

	{
		yyval.charptr = ccharp;
		switch(context) {

		case lex2:
			name(yypvt[-1].charptr, MF);
			break;

		case lex3:
			name(yypvt[-0].charptr, ARG1);
			break;

		case lex4:
			name(yypvt[-0].charptr, REST);
		}
	} break;
case 15:
# line 181 "apl.y"

	{
		if(context == lex2)
			name(yyval.charptr, NF);
		yyval.charptr = ccharp;
	} break;
case 16:
# line 188 "apl.y"

	{
		yyval.charptr = yypvt[-0].charptr;
		switch(context) {

		case lex3:
			name(yypvt[-1].charptr, AUTO);
			break;

		case lex4:
			ccharp2 = name(yypvt[-1].charptr, REST);
			invert(yyval.charptr, ccharp2);
		}
	} break;
case 17:
# line 202 "apl.y"

	{
		yyval.charptr = ccharp;
	} break;
case 18:
# line 211 "apl.y"

	{
		litflag = -1;
	} break;
case 20:
# line 217 "apl.y"

	{
		name(yypvt[-0].charptr, NAME);
	} break;
case 23:
# line 225 "apl.y"

	{
	    *ccharp++ = IMMED;
	    *ccharp++ = immedcmd;
	    name(yypvt[-0].charptr, NAME);
	} break;
case 24:
# line 231 "apl.y"

	{
	    name(yypvt[-0].charptr, NAME);
	} break;
case 29:
# line 251 "apl.y"
 {
		if(labgen)
			genlab(yypvt[-1].charptr);
	} break;
case 30:
# line 257 "apl.y"

	{
		integ = ccharp[-1];
		if(integ != ASGN && integ != PRINT && integ != COMNT)
			*ccharp++ = PRINT;
	} break;
case 31:
# line 263 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = BRAN0;
	} break;
case 32:
# line 268 "apl.y"

	{
		yyval.charptr = yypvt[-1].charptr;
		*ccharp++ = BRAN;
	} break;
case 33:
# line 274 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = COMNT;
	} break;
case 35:
# line 281 "apl.y"

	{
		litflag = 1;
		yyval.charptr = ccharp;
		*ccharp++ = COMNT;
	} break;
case 39:
# line 292 "apl.y"

	{
		*ccharp++ = PRINT;
	} break;
case 41:
# line 298 "apl.y"

	{
		*ccharp++ = HPRINT;
	} break;
case 43:
# line 304 "apl.y"

	{
		invert(yyval.charptr, yypvt[-0].charptr);
	} break;
case 44:
# line 308 "apl.y"

	{
		invert(yyval.charptr, yypvt[-0].charptr);
	} break;
case 46:
# line 314 "apl.y"

	{
		invert(yyval.charptr, yypvt[-1].charptr);
		*ccharp++ = INDEX;
		*ccharp++ = scount;
		scount = yypvt[-2].charval;
	} break;
case 47:
# line 322 "apl.y"

	{
		yyval.charptr = name(yyval.charptr, FUN);
	} break;
case 48:
# line 326 "apl.y"

	{
		yyval.charptr = name(yyval.charptr, NAME);
	} break;
case 49:
# line 330 "apl.y"

	{
		yyval.charptr = ccharp;
		ccharp += 2;
		integ = iline[-1];
		vcount = 0;
		for(;;) {
			if(*iline == '\n') {
				nlexsym = unk;
				break;
			}
			if(*iline == integ) {
				iline++;
				if(*iline != integ)
					break;
			}
			*ccharp++ = *iline++;
			vcount++;
		}
		((struct chrstrct *)yyval.charptr)->c[0] = QUOT;
		((struct chrstrct *)yyval.charptr)->c[1] = vcount;
	} break;
case 50:
# line 352 "apl.y"

	{
		*ccharp++ = CONST;
		*ccharp++ = vcount;
		invert(yyval.charptr, ccharp-2);
	} break;
case 51:
# line 358 "apl.y"

	{
		yyval.charptr = yypvt[-1].charptr;
	} break;
case 52:
# line 362 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = yypvt[-0].charval;
	} break;
case 53:
# line 368 "apl.y"

	{
		vcount++;
	} break;
case 54:
# line 372 "apl.y"

	{
		vcount = 1;
	} break;
case 55:
# line 377 "apl.y"

	{
		yyval.charptr = ccharp;
		ccharp += copy(DA,&datum,ccharp,1);
	} break;
case 56:
# line 388 "apl.y"

	{
		yyval.charval = scount;
		scount = 1;
	} break;
case 58:
# line 395 "apl.y"

	{
		invert(yyval.charptr, yypvt[-0].charptr);
		scount++;
	} break;
case 60:
# line 402 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = ELID;
	} break;
case 61:
# line 412 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = yypvt[-0].charval;
	} break;
case 62:
# line 417 "apl.y"

	{
		yyval.charptr = yypvt[-0].charptr;
		*ccharp++ = yypvt[-1].charval+1;
	} break;
case 63:
# line 422 "apl.y"

	{
		yyval.charptr = name(yyval.charptr, FUN);
	} break;
case 64:
# line 426 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = yypvt[-0].charval+1;
		*ccharp++ = yypvt[-1].charval;
	} break;
case 65:
# line 432 "apl.y"

	{
		yyval.charptr = yypvt[-0].charptr;
		*ccharp++ = yypvt[-1].charval+3;
		*ccharp++ = yypvt[-2].charval;
	} break;
case 68:
# line 441 "apl.y"

	{
		yyval.charval++;
	} break;
case 70:
# line 447 "apl.y"

	{
		yyval.charval += 2;
	} break;
case 71:
# line 456 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = yypvt[-0].charval;
	} break;
case 72:
# line 461 "apl.y"

	{
		yyval.charptr = yypvt[-0].charptr;
		*ccharp++ = yypvt[-1].charval;
	} break;
case 73:
# line 466 "apl.y"

	{
		yyval.charptr = name(yyval.charptr, FUN);
	} break;
case 74:
# line 470 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = OPROD;
		*ccharp++ = yypvt[-0].charval;
	} break;
case 75:
# line 476 "apl.y"

	{
		yyval.charptr = ccharp;
		*ccharp++ = IPROD;
		*ccharp++ = yypvt[-2].charval;
		*ccharp++ = yypvt[-0].charval;
	} break;
case 76:
# line 484 "apl.y"

	{
		yyval.charval += 2;
	} break;
case 77:
# line 495 "apl.y"

	{
		yyval.charptr = yypvt[-1].charptr;
	} break;
		}
		goto yystack;  /* stack new state and value */

	}
