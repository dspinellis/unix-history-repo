
# line 3 "parser.y"
/*******************************************************************
*                                                                  *
*    File: CIFPLOT/parser.y                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

/* This is a YACC description for the parser */

/* In the offical CIF language description SEMI = BLANKLIST ';' BLANKLIST
 * but this causes ambiguities for the LALR(1) parser so the scanner
 * returns ';' when it spots the pattern BLANKLIST ';'
 */

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "alloc.h"

#define yyparse parser
#define yylex scanner

#define null  Concat("",0);
int Definning=0;
int SendAll=0;		/* If set causes the lexical analyzer to return
			 * BLANKLIST and ';' seperately */
int A = 1;
int B = 1;

#define SCALE(x)	( (((real) A) * ((real) x)) / ((real) B ))

# define BLANK 257
# define OTHERCHAR 258
# define COMMENT_COMMAND 259
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 527 "parser.y"


#include "scanner.c"

short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	0, 4,
	-2, 167,
-1, 23,
	59, 35,
	-2, 16,
-1, 49,
	59, 35,
	-2, 26,
-1, 155,
	59, 81,
	-2, 160,
	};
# define YYNPROD 168
# define YYLAST 1046
short yyact[]={

 242, 158, 132, 183, 226, 227, 192, 193, 184, 157,
 182, 194, 162, 164,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  36, 159, 160, 167, 166, 165,
  70,  99, 100, 101, 102, 103, 104, 105, 106, 107,
 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
 118, 119, 120, 121, 122, 123, 124, 188,  68,  66,
  64,  61,  11, 192, 193,  55,  52, 133, 194, 132,
 150,  89,  90,  91,  92,  93,  94,  95,  96,  97,
  98,  11, 131,  57,  44, 149, 220, 218,  99, 100,
 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
 121, 122, 123, 124, 230,  54, 200, 214, 231,  76,
 192, 193,  10,  26,  75, 194, 198,  34,  89,  90,
  91,  92,  93,  94,  95,  96,  97,  98,  43,  22,
  21,  20,  19,  18,  17,  99, 100, 101, 102, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
 124,  16, 129,  36, 126,  25,  27,  50,  15,  14,
  13,  41,   6,  36,  83,   5,   2,   1,   0,  36,
   0,   0,   0, 159, 217,   0,   0,   0,   0,   0,
 192, 193, 159,   0,   0, 194,   0,   0,  89,  90,
  91,  92,  93,  94,  95,  96,  97,  98, 159,   0,
   0, 181, 174, 232, 191,  99, 100, 101, 102, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
 124, 188, 207, 170, 204,   0,  36, 192, 193,  38,
  12,   0, 194,   0, 180,  89,  90,  91,  92,  93,
  94,  95,  96,  97,  98,  48,   0,   0, 224,   0,
 235, 191,  99, 100, 101, 102, 103, 104, 105, 106,
 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
 117, 118, 119, 120, 121, 122, 123, 124, 234,  89,
  90,  91,  92,  93,  94,  95,  96,  97,  98, 146,
   0,   0,  89,  90,  91,  92,  93,  94,  95,  96,
  97,  98,   0, 251, 250,   0, 253, 232, 191,  99,
 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
 120, 121, 122, 123, 124, 141,   0,   0,  89,  90,
  91,  92,  93,  94,  95,  96,  97,  98,   0,   0,
   0,   0,   0,   0,   0,  99, 100, 101, 102, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
 124,  87,  88, 186,   0,   0,  86, 215, 191,  89,
  90,  91,  92,  93,  94,  95,  96,  97,  98,   0,
  48,   0,   0, 187, 185,   0,  99, 100, 101, 102,
 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
 123, 124,  89,  90,  91,  92,  93,  94,  95,  96,
  97,  98,   0,   0,   0, 191,   0,   0,   0,  99,
 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
 120, 121, 122, 123, 124,  99, 100, 101, 102, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
 124, 136, 137,  99, 100, 101, 102, 103, 104, 105,
 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
 116, 117, 118, 119, 120, 121, 122, 123, 124,  59,
   4,   0,  35,   0,   0,   4,   0,   0,   0,   0,
   0,   4,  11,   0,  35,   0,   0, 136, 137,  29,
  33,  24,   9,   0,   4,   0, 156, 134,   0,  32,
   0, 140,   4,  28,  11,  30,   0,   0, 202, 211,
  31,  29,  33,  47,  29,  33,  24,   4,   0,   0,
   4,  32,   0,   0,  32,  28, 211,  30,  28,   0,
  30,   0,  31,  29,  33,  31, 211,   0,  36, 155,
 212,   0, 216,  32,   0, 211,   0,  28, 221,  30,
   0, 135, 153,   0,  31, 138, 142,   0, 144,   0,
 142, 147, 233, 148, 145,   0,   0,   0,   7,   0,
 161, 163,   0,   0,  39,  40, 154,   3, 233,   0,
   0,  36,  37,  45,  58,   0,   0,   0,  42,  85,
   0,   0,   0,   0,  53,   0,   0,   0,   0,   4,
  35,  51,   0,   0,  56,   0, 139, 136, 137,  67,
 125,   0,   0,   0,  62,  62,  62, 128,  62, 152,
   0,   0,   0,   0, 127, 189,   0, 130,   0,   4,
 199,   0,   0,   0,   0, 136, 137,   0,   0,  35,
   0,   0,   0, 176,   0,   0,   0,   0, 178,   0,
   0,   0,   0,   4, 195,   0,   0,   0,   0, 201,
   0, 203,   0,  84,   0,   0,   0,   0,   0, 205,
 219,   0,   0,   0,   0, 206,   0,   0, 155,   8,
 155,   0,  23,   0, 143,   0,   0,   0, 143,   0,
 219,   0,   0, 151, 213, 168, 155, 155,   0,   0,
 155,  46, 152,   0,  49,  36, 169,  23, 190,   0,
   0,   0, 155,   0,  60, 190,  62,   0,  62,   0,
 236, 237,   0,   4,  36,   0,  49, 240,   0,  62,
   0,   0,   0, 152,   0,   0, 197, 190,   0, 173,
 248,   0,   0,  62, 249,  63,  65, 190,  69, 252,
 190,   0, 179,   0, 190, 190,   0,   0,   0, 152,
 209, 225,   0, 228, 190,   0, 175, 143,   0,   0,
 190, 175, 143, 190,   0,   0, 151,   0,   0, 238,
 239,   0,   0, 241,   0,   0, 190, 208,   0, 210,
  62,   0,   0,   0,   0, 247,   0,   0,  62,   0,
 152,   0, 175,   0, 175,   0,   0, 151,   0,   0,
   0, 223,   0,   0,   0,   0,   0,   0,   0,   0,
   0,  62,   0,   0,   0,  62,   0,   0,   0,   0,
 254,   0,   0, 151,   0,   0,   0,   0,   0,   0,
 243,   0, 245,   0,   0,   0, 171,   0, 172,  71,
  72,  73,  77,  78,  79,  80,  81,  82,  74, 177,
  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,
   0,   0,   0, 196, 151,   0,   0,  99, 100, 101,
 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
 122, 123, 124,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 222,   0,   0,   0,   0,   0,   0,   0, 229,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0, 244,   0,   0,   0, 246 };
short yypact[]={

-1000,-1000, 513,-233,-1000,   3,  22,-1000,-1000,-1000,
-1000,-1000,-1000,-1000, 535,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-190,  22,-1000,-1000,-191,-195,
-196,-197,-198,-226, 891,-1000,-1000, 371,-1000,-1000,
-1000,  22,-233,-1000,-1000,-1000,-1000,-1000,  22,-1000,
-1000,  -1,-1000,-1000, 468,-1000,-1000, 440, 320,-1000,
 468,-1000, 274, 468,-1000, 468,-1000, 414,-1000,-1000,
-1000, -64,-231, -55, -39,-227,-228,-1000,-1000,-1000,
-1000,-1000,-1000,-229,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,  22, -68,-1000,-1000,
 538,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
 261, 261,-1000,-1000,-1000, 261, 261,-1000,-1000,-1000,
 902,-1000,-1000,-1000, -74,-1000, 217,-232,-1000,-1000,
-1000,  82,-232, 217,-232,-1000,-1000,-1000,-1000, 557,
-1000, 468,-1000,-1000,-1000,-1000, 261, 468, 261,-1000,
 468, 902,-1000,-1000,-1000, 217,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, 217, 468,-233, 160,-1000,
-1000,  52, 217, 217,-1000,-1000,-1000, 902,-1000, -84,
-1000,-1000, 217,-1000,-1000,-1000,-1000,-1000,  80,-1000,
-1000,  23, 468, 468,-1000,-1000,-1000,-1000,-1000, 468,
-1000,-1000,-1000,-1000, -34,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-232,-1000,-1000, 468,-1000, 902,-1000,
-1000, 468,-1000,-1000,-233 };
short yypgo[]={

   0, 187, 186, 185, 666, 658, 184, 182, 181, 122,
 260, 180, 179, 123, 178, 177, 176,  84, 175, 174,
 172, 171, 144, 143, 142, 141, 140, 139, 115, 804,
 587, 694,  85, 642, 127, 586, 434,  83, 126,  87,
 124, 119, 559, 674, 591, 715,  70, 641, 413, 433,
 118, 117 };
short yyr1[]={

   0,   1,   1,   1,   1,   1,   2,   2,   2,   8,
   2,   7,   9,   9,   9,   9,  15,  14,  14,  14,
  12,  12,  12,  19,  12,  17,  20,  17,  10,  10,
  10,  10,  10,  10,  10,  10,  21,  21,  22,  22,
  22,  23,  23,  24,  24,  25,  25,  18,  18,  13,
  11,  16,  26,  26,  27,  27,  27,  27,  27,  27,
  27,  27,  27,  27,  27,  40,  40,  40,  40,  41,
  41,  41,  41,  41,  41,  34,   3,  33,  33,  33,
  33,  33,  28,  28,  31,  31,  37,  37,  29,  29,
  44,  44,  32,  32,  32,  32,  46,  46,  43,  43,
  30,  30,  30,  47,  47,  47,  47,  47,  47,  47,
  47,  47,  47,  47,  47,  47,  47,  47,  47,  47,
  47,  47,  47,  47,  47,  47,  47,  47,  47,  45,
  45,  45,  45,  45,  45,  45,  45,  45,  45,  36,
  36,  48,  48,  39,  39,  50,  50,  38,  38,  51,
  51,  51,  49,  49,  49,  49,  49,  49,  35,   4,
   4,   6,   6,   6,   6,   6,   5,  42 };
short yyr2[]={

   0,   3,   3,   3,   1,   5,   3,   2,   1,   0,
   4,   1,   1,   1,   2,   1,   0,   4,   1,   1,
   2,   2,   2,   0,   4,   2,   0,   4,   1,   1,
   1,   1,   1,   1,   1,   1,   2,   2,   6,   8,
   2,   4,   2,   4,   2,   3,   2,   4,   8,   3,
   4,   2,   3,   2,   4,   5,  13,   4,   7,   8,
   4,   7,   9,   3,   3,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   4,   5,   5,
   4,   1,   3,   1,   3,   2,   2,   3,   2,   3,
   2,   1,   1,   2,   3,   4,   1,   1,   2,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   2,
   1,   1,   1,   2,   1,   1,   1,   2,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   2,   2,
   1,   1,   1,   1,   1,   1,   2,   0 };
short yychk[]={

-1000,  -1,  -2,  -4, -42,  -3,  -7,  -5, 256,  69,
  -9,  59, -10, -11, -12, -14, -21, -22, -23, -24,
 -25, -26, -27, 259,  68, -18, -13, -16,  80,  66,
  82,  87,  76,  67, -34, -42, 257,  -4, 256,  -5,
  -5,  -8,  -4, -13, -17,  -5, 256,  68, -10, 259,
 -15,  -4, 256,  -5, -28, 256, -31, -37, -43, -42,
 -29, 256, -43, -29, 256, -29, 256,  -4, 256, -29,
 256,  48,  49,  50,  57, -40, -41,  51,  52,  53,
  54,  55,  56,  -6, -45, -47,  45,  40,  41,  48,
  49,  50,  51,  52,  53,  54,  55,  56,  57,  65,
  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,
  86,  87,  88,  89,  90,  -5, -19,  -4,  -5, -20,
  -4,  83,  70,  68, -30, -47, 257, 258, -30, 256,
 -44,  45, -30, -45, -30, -44,  45, -30, -30, -32,
 -46, -45, -47, -33,  -4, -42, -35,  73,  65, 257,
 257, -35,  67, -35,  52, 256, 256, 256,  -5,  -4,
  -9, -29, -29, -31, -37, -45, -44, -29, -44, -31,
 -28, -46,  84,  77,  82, -36, -48, -49,  34, -45,
 -47, 258,  40,  41,  45, -35, -29,  -4, -38, -42,
  34, -35, -36, -35, -17, -30, -30, -46, -31,  -4,
 -31, -48, -36, -30, -51, 257, -49,  34, -39, -42,
  34, -36, -29, -31, -46, -33,  88,  89, -33, -29,
  34, -50, 257, -49, -39, 257, -30, -30, -33, -33,
 -30, -33,  34, -31, -29, -31, -29, -33, -35, -30,
 -32, -37, -30, -37,  -4 };
short yydef[]={

 167,  -2,  -2,   8, 160, 167,   0,   7,   9,  76,
  11, 167,  12,  13, 167,  15,  28,  29,  30,  31,
  32,  33,  34,  -2, 167,   0,  18,  19, 167, 167,
 167, 167, 167, 167,   0,  75, 159,   1,   2,   3,
   6,   0, 166,  14,  20,  21,  23, 167,   0,  -2,
 167,   0,  51,  22,  36,  37,  83,   0,   0,  99,
   0,  40,   0,   0,  42,   0,  44,   0,  46, 167,
  53,  65,  66,  67,  68,   0,   0,  69,  70,  71,
  72,  73,  74,   0, 161, 162, 163, 164, 165, 129,
 130, 131, 132, 133, 134, 135, 136, 137, 138, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
 124, 125, 126, 127, 128,  10,   0,   0,  25, 167,
 167, 167,  49, 167, 167, 100, 101, 102, 167,  85,
  86,   0,  98,  91, 167,  88,   0, 167, 167,  45,
  92,  96,  97,  52,   0,  -2,   0,   0, 167, 167,
 167,   0,   0,   0,   0,  63,  64,   5,  24, 167,
  17,  47,  50,  82,  84,  90,  87,   0,  89,  41,
  43,  93, 167, 167, 167,  54, 140, 141, 142, 152,
 153, 154, 155, 156, 157,   0,   0, 158,  57, 148,
 167,   0,  60,   0,  27, 167, 167,  94, 167,   0,
 167, 139,  55, 167, 147, 149, 150, 151,   0, 144,
 167,   0,   0,  38,  95,  77, 167, 167,  80,   0,
 167, 143, 145, 146,   0, 167, 167, 167,  78,  79,
 167,  58, 167,  61,  48,  39,   0,  59,   0, 167,
  62,   0, 167, 167,  56 };
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
			
case 2:
# line 47 "parser.y"
{ Error("Improper Ending",FATAL); } break;
case 3:
# line 49 "parser.y"
{ Error("Semi-Colon found after End",
							RECOVERABLE); } break;
case 4:
# line 52 "parser.y"
{ Error("No End Statement",RECOVERABLE); } break;
case 5:
# line 54 "parser.y"
{ Error("Garbage after End Statement",
							RECOVERABLE); } break;
case 9:
# line 63 "parser.y"
{ Error("Unrecognizable Command",FATAL); } break;
case 12:
# line 73 "parser.y"
{ Execute(yypvt[-0]);		} break;
case 14:
# line 76 "parser.y"
{ Execute(yypvt[-1]);
					  Definning = 0;
					  A=1; B=1;	 	} break;
case 16:
# line 84 "parser.y"
{ Error("Comments must end with a semi-colon",
							RECOVERABLE); 		} break;
case 17:
# line 87 "parser.y"
{ yyval = yypvt[-3]; } break;
case 18:
# line 89 "parser.y"
{ Error("Define Finnished found outside of Definition",
							FATAL); 
					  yyval = yypvt[-0]; 	} break;
case 19:
# line 93 "parser.y"
{ Error("Unrecognized Definition Command",
							FATAL);	} break;
case 20:
# line 99 "parser.y"
{ yyval = AddCmd(yypvt[-1],yypvt[-0]);	} break;
case 21:
# line 101 "parser.y"
{ yyval = yypvt[-1]; } break;
case 22:
# line 103 "parser.y"
{ Definning = 1;
					  A=(int) ((Command *) yypvt[-1])->Ctype.Symbl.a;
					  B=(int) ((Command *) yypvt[-1])->Ctype.Symbl.b;
					  yyval = yypvt[-1];		} break;
case 23:
# line 108 "parser.y"
{ yyval = yypvt[-1];
					  Error("Unrecognized or Illegal Command in Definition",
							FATAL); } break;
case 25:
# line 116 "parser.y"
{ yyval = yypvt[-1]; } break;
case 26:
# line 118 "parser.y"
{ Error("Comments must end with a semi-colon",RECOVERABLE); } break;
case 27:
# line 120 "parser.y"
{ yyval = yypvt[-0];	} break;
case 36:
# line 136 "parser.y"
{ /* Polygons must have more than two
					   * vertices to be well defined */
					  if( ((struct PathHeader *) yypvt[-0])->PNo < 3)
						Error("Degenerate Polygon",WARNING);
					  /* Polygons with less than two vertices are
					    * useless	*/
					  if( ((struct PathHeader *) yypvt[-0])->PNo < 2) {
						Error("Command Ignored",WARNING);
						yyval = MakeComment();
						}
					      else
					  	yyval = MakePoly(yypvt[-0]);	} break;
case 37:
# line 149 "parser.y"
{ Error("Bad Path Descriptor in Polygon",FATAL);		
					  yyval = MakeComment();	} break;
case 38:
# line 155 "parser.y"
{ yyval = MakeBox(SCALE(yypvt[-4]),SCALE(yypvt[-2]),yypvt[-0],
							MakePoint(1.0,0.0)); 
					  Free(yypvt[-0]);		} break;
case 39:
# line 159 "parser.y"
{ if(!CheckPoint(yypvt[-0]))
						Error("Bad Direction Vector in Box Command",FATAL);
					  yyval = MakeBox(SCALE(yypvt[-6]),SCALE(yypvt[-4]),yypvt[-2],yypvt[-0]);
			  		  Free(yypvt[-2]); Free(yypvt[-0]);	} break;
case 40:
# line 164 "parser.y"
{ Error("Illegal Box Description",FATAL);
					  yyval = MakeComment();	} break;
case 41:
# line 170 "parser.y"
{ yyval = MakeFlash(SCALE(yypvt[-2]),yypvt[-0]);} break;
case 42:
# line 172 "parser.y"
{ Error("Illegal Round Flash Description",FATAL);
					  yyval = MakeComment();	} break;
case 43:
# line 178 "parser.y"
{ if( ((struct PathHeader *) yypvt[-0])->PNo < 2) 
						Error("Degenerate Wire",WARNING);
					  yyval = MakeWire(SCALE(yypvt[-2]),yypvt[-0]); } break;
case 44:
# line 182 "parser.y"
{ Error("Illegal Wire Description",FATAL);
					  yyval = MakeComment();	} break;
case 45:
# line 188 "parser.y"
{ yyval = MakeLayer(yypvt[-0]);	} break;
case 46:
# line 190 "parser.y"
{ Error("Illegal Layer Command",FATAL);
					  yyval = MakeComment();	} break;
case 47:
# line 196 "parser.y"
{ yyval = MakeSymbol(yypvt[-0],1,1);} break;
case 48:
# line 198 "parser.y"
{ yyval = MakeSymbol(yypvt[-4],yypvt[-2],yypvt[-0]);} break;
case 49:
# line 203 "parser.y"
{ yyval = 0; } break;
case 50:
# line 208 "parser.y"
{ DeleteDefintion(yypvt[-0]);
					  yyval = MakeComment();	} break;
case 52:
# line 219 "parser.y"
{ yyval = MakeCall(yypvt[-1],yypvt[-0]);	} break;
case 53:
# line 221 "parser.y"
{ Error("Illegal Call Command",FATAL);		
					  yyval = MakeComment();	} break;
case 54:
# line 227 "parser.y"
{ yyval = MakeComment();
					  if(!standard)
					     Include(yypvt[-0]);
					   else
					     Error("User Extention Ignored\n",WARNING);
					 SendAll = 0;
					 Free(yypvt[-0]);		} break;
case 55:
# line 235 "parser.y"
{ yyval = MakeComment();
					  if(!standard)
					     Include(yypvt[-1]);
					   else
					     Error("User Extention Ignored\n",WARNING);
					  SendAll = 0;
					  Free(yypvt[-1]);		} break;
case 56:
# line 243 "parser.y"
{ if(!standard)
					     yyval = MakeArray(yypvt[-9],yypvt[-7],yypvt[-5],SCALE(yypvt[-3]),SCALE(yypvt[-1]));
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 57:
# line 251 "parser.y"
{ yyval = MakeComment();
					  if(!standard)
					     fprintf(stderr,"%s\n",yypvt[-0]);
					   else
					     Error("User Extention Ignored\n",WARNING);
					  SendAll = 0;
					  Free(yypvt[-0]);		} break;
case 58:
# line 259 "parser.y"
{ if(!standard)
					     yyval = MakeText(yypvt[-2],yypvt[-0],'l');
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 59:
# line 267 "parser.y"
{ if(!standard)
					     yyval = MakeText(yypvt[-2],yypvt[-0],'c');
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 60:
# line 275 "parser.y"
{ if(!standard)
					     yyval = MakeName(yypvt[-0]);
					   else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 61:
# line 283 "parser.y"
{ if(!standard) {
					    yyval = MakePointName(yypvt[-2],yypvt[-0],"all");
					    Free(yypvt[-0]);
					    }
					  else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 62:
# line 293 "parser.y"
{ if(!standard) {
					    yyval = MakePointName(yypvt[-4],yypvt[-2],yypvt[-0]);
					    Free(yypvt[-2]);
					    }
					  else {
					     Error("User Extention Ignored\n",WARNING);
					     yyval = MakeComment();
					     }
					  SendAll = 0;		} break;
case 63:
# line 303 "parser.y"
{ yyval = MakeComment();
					  Error("Bad Syntax in Extension Command --- Command Ignored",
								WARNING);
					   SendAll = 0;   } break;
case 64:
# line 308 "parser.y"
{ yyval = MakeComment();
					  Error("Unimplemented User Extension",
								WARNING);
					  SendAll = 0;     } break;
case 75:
# line 323 "parser.y"
{SendAll = 1; } break;
case 77:
# line 332 "parser.y"
{ yyval = Translate(yypvt[-1],yypvt[-0]);	
					  if(output == NOPLOT)
					      Free(((transform *) yypvt[-0])->TransString);
					  Free(yypvt[-1]);  FreeTransform(yypvt[-0]);	} break;
case 78:
# line 337 "parser.y"
{ yyval = Mirror('x',yypvt[-0]);	
					  if(output == NOPLOT)
					      Free(((transform *) yypvt[-0])->TransString);
					  FreeTransform(yypvt[-0]);		} break;
case 79:
# line 342 "parser.y"
{ yyval = Mirror('y',yypvt[-0]);	
					  if(output == NOPLOT)
					      Free(((transform *) yypvt[-0])->TransString);
					  FreeTransform(yypvt[-0]);		} break;
case 80:
# line 347 "parser.y"
{ if(!CheckPoint(yypvt[-1]))
					      Error("Bad Rotation Vector",FATAL);
					  yyval = Rotate(yypvt[-1],yypvt[-0]);
					  if(output == NOPLOT)
					      Free(((transform *) yypvt[-0])->TransString);
					  Free(yypvt[-1]);  FreeTransform(yypvt[-0]);	} break;
case 81:
# line 354 "parser.y"
{ yyval = MakeTransform();		} break;
case 82:
# line 359 "parser.y"
{ yyval = AddPath(yypvt[-2],yypvt[-0]);
					  Free(yypvt[-0]);		} break;
case 83:
# line 362 "parser.y"
{ yyval = MakePath(yypvt[-0]);		} break;
case 84:
# line 367 "parser.y"
{ yyval = MakePoint(SCALE(yypvt[-2]),SCALE(yypvt[-0]));	} break;
case 85:
# line 369 "parser.y"
{ Error("Odd number of values encountered",
							FATAL); 
					  yyval = MakePoint(SCALE(yypvt[-1]),0.0);} break;
case 86:
# line 376 "parser.y"
{ yyval = yypvt[-0]; } break;
case 87:
# line 378 "parser.y"
{ yyval = - yypvt[-0];		} break;
case 88:
# line 383 "parser.y"
{ yyval = yypvt[-0]; } break;
case 89:
# line 385 "parser.y"
{ Error("Expected Positive Integer",FATAL);
					  yyval = yypvt[-0];			} break;
case 90:
# line 391 "parser.y"
{ if (yypvt[-1] > (0x800000 / 10)) {
						Error("Integers may not exceed 2**24",
								FATAL);
						yyval = 0;
						}
					    else
						yyval = (yypvt[-1]*10)+yypvt[-0]-'0';	} break;
case 91:
# line 399 "parser.y"
{ yyval = yypvt[-0] - '0';	} break;
case 93:
# line 405 "parser.y"
{ yyval = Concat(yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-1]); Free(yypvt[-0]);		 } break;
case 94:
# line 408 "parser.y"
{ yyval = Concat(yypvt[-2],yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-2]); Free(yypvt[-1]); Free(yypvt[-0]);	} break;
case 95:
# line 411 "parser.y"
{ yyval = Concat(yypvt[-3],yypvt[-2],yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-3]); Free(yypvt[-2]);
					  Free(yypvt[-1]); Free(yypvt[-0]);		} break;
case 96:
# line 418 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 97:
# line 420 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 139:
# line 448 "parser.y"
{ yyval = Concat(yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-1]); Free(yypvt[-0]);		} break;
case 142:
# line 456 "parser.y"
{ yyval = MakeString('"'); } break;
case 143:
# line 461 "parser.y"
{ yyval = Concat(yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-1]); Free(yypvt[-0]);		} break;
case 144:
# line 464 "parser.y"
{ yyval = null;		} break;
case 145:
# line 469 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 147:
# line 475 "parser.y"
{ yyval = Concat(yypvt[-1],yypvt[-0],0);
					  Free(yypvt[-1]); Free(yypvt[-0]);		} break;
case 148:
# line 478 "parser.y"
{ yyval = null;		} break;
case 149:
# line 483 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 151:
# line 486 "parser.y"
{ yyval = MakeString('"'); } break;
case 152:
# line 491 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 153:
# line 493 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 154:
# line 495 "parser.y"
{ yyval = MakeString(yypvt[-0]); } break;
case 155:
# line 497 "parser.y"
{ yyval = MakeString('('); } break;
case 156:
# line 499 "parser.y"
{ yyval = MakeString(')'); } break;
case 157:
# line 501 "parser.y"
{ yyval = MakeString('-'); } break;
		}
		goto yystack;  /* stack new state and value */

	}
