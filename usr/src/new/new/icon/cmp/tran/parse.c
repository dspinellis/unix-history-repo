# define CSETLIT 257
# define EOFX 258
# define IDENT 259
# define INTLIT 260
# define REALLIT 261
# define STRINGLIT 262
# define BREAK 263
# define BY 264
# define CASE 265
# define CREATE 266
# define DEFAULT 267
# define DO 268
# define DYNAMIC 269
# define ELSE 270
# define END 271
# define EVERY 272
# define EXTERNAL 273
# define FAIL 274
# define GLOBAL 275
# define IF 276
# define INITIAL 277
# define LOCAL 278
# define NEXT 279
# define NOT 280
# define OF 281
# define PROCEDURE 282
# define RECORD 283
# define REPEAT 284
# define RETURN 285
# define STATIC 286
# define SUSPEND 287
# define THEN 288
# define TO 289
# define UNTIL 290
# define WHILE 291
# define ASSIGN 292
# define AT 293
# define AUGACT 294
# define AUGAND 295
# define AUGEQ 296
# define AUGEQV 297
# define AUGGE 298
# define AUGGT 299
# define AUGLE 300
# define AUGLT 301
# define AUGNE 302
# define AUGNEQV 303
# define AUGSEQ 304
# define AUGSGE 305
# define AUGSGT 306
# define AUGSLE 307
# define AUGSLT 308
# define AUGSNE 309
# define BACKSLASH 310
# define BANG 311
# define BAR 312
# define CARET 313
# define CARETASGN 314
# define COLON 315
# define COMMA 316
# define CONCAT 317
# define CONCATASGN 318
# define CONJUNC 319
# define DIFF 320
# define DIFFASGN 321
# define DOT 322
# define EQUIV 323
# define INTER 324
# define INTERASGN 325
# define LBRACE 326
# define LBRACK 327
# define LCONCAT 328
# define LCONCATASGN 329
# define LEXEQ 330
# define LEXGE 331
# define LEXGT 332
# define LEXLE 333
# define LEXLT 334
# define LEXNE 335
# define LPAREN 336
# define MCOLON 337
# define MINUS 338
# define MINUSASGN 339
# define MOD 340
# define MODASGN 341
# define NOTEQUIV 342
# define NUMEQ 343
# define NUMGE 344
# define NUMGT 345
# define NUMLE 346
# define NUMLT 347
# define NUMNE 348
# define PCOLON 349
# define PLUS 350
# define PLUSASGN 351
# define QMARK 352
# define RBRACE 353
# define RBRACK 354
# define REVASSIGN 355
# define REVSWAP 356
# define RPAREN 357
# define SCANASGN 358
# define SEMICOL 359
# define SLASH 360
# define SLASHASGN 361
# define STAR 362
# define STARASGN 363
# define SWAP 364
# define TILDE 365
# define UNION 366
# define UNIONASGN 367

# line 26 "ucon.g"
#include "../h/config.h"
#include "utran.h"
#include "sym.h"
#include "tree.h"
#include "keyword.h"
#define YYSTYPE nodeptr
#define YYMAXDEPTH 500
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

# line 38 "ucon.g"
int argcnt;
int idflag;
int i;
#ifdef EXT
int cstack[50];   /* context stack expression lists */
int stacktop = 0; /* stack top */
#endif
# define YYERRCODE 256

# line 370 "ucon.g"


#ifdef EXT
pushcs(val)
int val;
{
  stacktop = stacktop + 1;
  cstack[stacktop] = val;
}

popcs()
{
  stacktop = stacktop - 1;
}

int cswitch(x,y)
   int x, y;
   {
  if (cstack[stacktop]) return(CREATENODE(y,x));
   else return(x);
}
#endif
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 17,
	359, 29,
	-2, 27,
-1, 100,
	359, 29,
	-2, 27,
-1, 107,
	359, 29,
	-2, 27,
	};
# define YYNPROD 200
# define YYLAST 607
short yyact[]={

  28,  85, 281,  56,  82,  83,  84,  58, 282,  90,
  55, 275, 166, 234, 107, 279,  93, 203,  86, 108,
  89,  17, 269,  57,  41, 264, 243, 108,  94,  87,
 168,  88, 271, 278,  92,  91, 233,  40, 109, 204,
  99, 162, 165, 260, 236, 284, 272, 237, 240, 108,
 239, 238, 163, 171,  81,  64,  42,  69, 167, 246,
 245,  43, 277,  62,  65, 108,  63,  77,  70, 173,
  60,  61,  44, 256,  75, 108, 203, 283, 244,  76,
  59, 106,  72, 176, 274, 108,  80,  73, 247, 172,
 194, 170,  74, 180,  66, 108,  79, 178, 177, 108,
 175, 108, 108, 174,  68, 193,  67, 179,  96,  71,
  78,  28,  85, 241,  56,  82,  83,  84,  58, 250,
  90,  55, 230, 108,  16,  98,  22,  93,   3,  86,
 231,  89,  35, 190,  57,  41,  36,  34,  27,  94,
  87, 252,  88, 105, 206,  92,  91, 205,  40,  33,
   2, 102, 103, 251,  14, 249, 273, 268, 248, 229,
 104, 267,  24,  95,  97,  81,  64,  42,  69, 228,
  31,  21,  43, 258,  62,  65, 192,  63,  77,  70,
 255,  60,  61,  44,   4,  75,  12, 189,  11,  54,
  76,  59,  53,  72,  52,  13,   9,  80,  73,  12,
  51,  11,  50,  74,  49,  66,  48,  79,  13,   9,
  47,  46,  45,  39,  37,  68, 169,  67, 164, 161,
  71,  78,  85,  38,  56,  82,  83,  84,  58, 146,
  90,  55,  32, 111, 110,  30, 101,  93,  20,  86,
 100,  89,  23,  10,  57,  41, 208, 211,  15,  94,
  87,  19,  88,  18,   8,  92,  91,   7,  40,   6,
   5,   1,   0, 207, 181, 182, 183, 184, 185, 186,
 210,   0,   0,   0,   0,  81,  64,  42,  69,   0,
   0,   0,  43,   0,  62,  65, 218,  63,  77,  70,
   0,  60,  61,  44,   0,  75, 220, 222,   0, 221,
  76,  59,   0,  72,   0,   0, 223,  80,  73,   0,
   0, 224,   0,  74,   0,  66, 219,  79,  29,   0,
   0,  26,   0, 235,   0,  68,   0,  67,   0, 115,
  71,  78, 117, 232, 114, 113, 131, 132, 133, 134,
 135, 136, 137, 138, 139, 140, 141, 142, 143, 144,
   0,  25, 242, 145, 130,   0,   0,   0, 120, 253,
 254, 122,   0,   0, 159, 127,   0, 285, 257, 121,
   0, 147, 148, 149, 150, 151, 152, 187,   0, 125,
   0, 129,   0, 160, 153, 154, 155, 156, 157, 158,
   0, 124, 188,   0, 191, 119, 118,   0, 112, 225,
 226, 128,   0, 126, 116,   0,   0, 123,   0,   0,
   0, 197, 198, 199, 200, 201, 202,   0, 265,   0,
   0, 195, 196,   0, 209,   0,   0,   0, 212, 213,
 214, 215, 216, 217,  25,   0,   0,   0,   0,   0,
   0,  25,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 227,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 259,   0,
 261, 262, 263,   0,   0,   0,   0,   0, 191,   0,
   0, 266,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, 276,   0,   0,   0,   0,   0,   0,   0,
 270,   0,   0,   0, 280,   0,   0,   0,   0,   0,
   0,   0,   0,   0, 276, 286, 287 };
short yypact[]={

-128,-1000, -74,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-235,-1000,-1000,-1000, -87,-133,-1000,-145,-151,-151,
-134,-1000,-296,-126,-190,-345,-254,-314,-1000,-1000,
  40,-1000,  41,-276,-308,-271,-1000,-210,-1000,-229,
 -35, -35, -35, -35, -35, -35,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-145,-1000,-1000,-145,-1000,
-145,-1000,-169,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-145,-145,-145,
-145,-145,-145,-145,-145,-240,-1000,-240,-297,-151,
-145,-151,-145,-1000,-1000,-1000,-1000,-145, -35, -35,
 -35, -35, -35, -35, -35, -35,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, -35, -35,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000, -35,-1000,-1000, -35,-1000,-1000,-1000,-1000, -35,
-1000,-1000,-1000,-1000, -35, -35, -35,-145,-1000,-1000,
-137,-1000,-1000,-1000,-1000,-1000,-1000,-254,-1000,-145,
-317,-346,-145,-1000,-1000,-1000,-1000,-244,-234,-217,
-218,-220,-254,-146,-151,-331,-240,-193,-299,-300,
-1000,-314,-1000,-1000,-1000,-1000,-1000,-1000,-176,-1000,
-276,-308,-271,-1000,-1000,-1000,-1000,-196,-145,-145,
-1000,-243,-1000,-1000,-145,-243,-145,-283,-145,-145,
-145,-1000,-332,-1000,-1000,-1000,-1000, -35,-1000,-145,
-1000,-1000,-1000,-243,-243,-335,-145,-1000,-322,-224,
-256,-254,-254,-254,-1000,-1000,-292,-320,-342,-1000,
-1000,-1000,-145,-351,-1000,-238,-270,-1000,-1000,-1000,
-254,-1000,-256,-145,-145,-1000,-254,-254 };
short yypgo[]={

   0, 261, 150, 260, 259, 257, 254, 253, 144, 251,
 248, 147, 243, 242, 240, 162, 238, 236, 321, 333,
 138, 318, 235, 234, 233, 170, 232, 149, 229, 137,
 219, 132, 218, 136, 216, 214, 223, 213, 212, 211,
 210, 206, 204, 202, 200, 194, 192, 189, 187, 130,
 180, 133, 176, 173, 169, 161, 159, 157, 156,  84,
 155 };
short yyr1[]={

   0,   1,   2,   2,   3,   3,   3,   7,   6,   9,
   6,  10,   4,   5,  16,  12,  11,  11,   8,   8,
  13,  13,  17,  17,  17,  14,  14,  15,  15,  19,
  19,  18,  18,  20,  20,  21,  21,  21,  21,  21,
  21,  23,  23,  23,  23,  24,  24,  24,  24,  24,
  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,
  24,  24,  24,  24,  24,  24,  24,  24,  24,  24,
  22,  22,  22,  25,  25,  26,  26,  28,  28,  28,
  28,  28,  28,  28,  28,  28,  28,  28,  28,  28,
  28,  27,  27,  30,  30,  29,  29,  32,  32,  32,
  32,  31,  31,  34,  34,  34,  34,  33,  33,  35,
  35,  35,  36,  36,  36,  36,  36,  36,  36,  38,
  38,  38,  38,  38,  38,  38,  38,  38,  38,  38,
  38,  38,  38,  38,  38,  38,  38,  38,  37,  37,
  37,  37,  37,  37,  37,  37,  37,  37,  37,  37,
  37,  48,  50,  37,  37,  52,  53,  37,  37,  54,
  55,  37,  56,  57,  37,  37,  37,  37,  44,  44,
  45,  45,  46,  46,  47,  41,  41,  41,  42,  42,
  43,  58,  58,  59,  59,  49,  49,  39,  39,  39,
  39,  40,  60,  60,  60,  51,  51,   1,   5,  18 };
short yyr2[]={

   0,   2,   0,   2,   1,   1,   1,   0,   3,   0,
   3,   0,   6,   6,   0,   6,   0,   1,   1,   3,
   0,   4,   1,   1,   1,   0,   3,   0,   3,   0,
   1,   1,   3,   1,   3,   1,   3,   3,   3,   3,
   3,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   3,   5,   1,   3,   1,   3,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   3,   1,   1,   1,   3,   1,   1,   1,
   1,   1,   3,   1,   1,   1,   1,   1,   3,   1,
   3,   3,   1,   2,   2,   2,   2,   2,   2,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   2,   1,   1,
   2,   0,   0,   5,   3,   0,   0,   5,   4,   0,
   0,   6,   0,   0,   6,   3,   2,   2,   2,   4,
   2,   4,   2,   4,   2,   1,   2,   2,   4,   6,
   6,   1,   3,   3,   3,   1,   3,   1,   1,   1,
   1,   6,   1,   1,   1,   1,   3,   3,   4,   1 };
short yychk[]={

-1000,  -1,  -2, 256, 258,  -3,  -4,  -5,  -6, 283,
 -12, 275, 273, 282,  -2, -10, 359, 256,  -7,  -9,
 -16, 258, 259, -13, -15, -19, -18, -20, 256, -21,
 -22, -25, -26, -27, -29, -31, -33, -35, -36, -37,
 293, 280, 312, 317, 328, -38, -39, -40, -41, -42,
 -43, -44, -45, -46, -47, 266, 259, 279, 263, 336,
 326, 327, 319, 322, 311, 320, 350, 362, 360, 313,
 324, 365, 338, 343, 348, 330, 335, 323, 366, 352,
 342, 310, 260, 261, 262, 257, 274, 285, 287, 276,
 265, 291, 290, 272, 284,  -8, 259,  -8, 259, 336,
 -14, -17, 277, 278, 286, 269, 271, 359, 319, 352,
 -23, -24, 358, 295, 294, 289, 364, 292, 356, 355,
 318, 329, 321, 367, 351, 339, 363, 325, 361, 341,
 314, 296, 297, 298, 299, 300, 301, 302, 303, 304,
 305, 306, 307, 308, 309, 312, -28, 330, 331, 332,
 333, 334, 335, 343, 344, 345, 346, 347, 348, 323,
 342, -30, 317, 328, -32, 350, 320, 366, 338, -34,
 362, 324, 360, 340, 313, 310, 293, 327, 326, 336,
 322, -36, -36, -36, -36, -36, -36, -18, -19, -48,
 -51, -19, -52, 274, 259, -19, -19, -18, -18, -18,
 -18, -18, -18, 316, 336, -11,  -8, -15,  -8, -18,
 -15, -20, -21, -21, -21, -21, -21, -21, -25, -25,
 -27, -29, -31, -33, -33, -36, -36, -18, -54, -56,
 259, -49, -19, 353, 359, -49, 288, 281, 268, 268,
 268, 259, -11, 357, 271, 359, 359, 264, 354, -60,
 315, 349, 337, -49, -49, -50, 316, -51, -53, -18,
 326, -18, -18, -18, 357, -25, -18, -55, -57, 357,
 -19, 354, 270, -58, -59, 267, -18, 354, 353, 357,
 -18, 353, 359, 315, 315, -59, -18, -18 };
short yydef[]={

   2,  -2,   0,   2,   1,   3,   4,   5,   6,  11,
   0,   7,   9,  14,   0,   0,  20,  -2,   0,   0,
   0, 197,   0,  25,   0,   0,  30,  31, 199,  33,
  35,  70,  73,  75,  91,  95, 101, 107, 109, 112,
   0,   0,   0,   0,   0,   0, 138, 139, 140, 141,
 142, 143, 144, 145, 146,   0, 148, 149,  29, 151,
  29, 155,   0, 119, 120, 121, 122, 123, 124, 125,
 126, 127, 128, 129, 130, 131, 132, 133, 134, 135,
 136, 137, 187, 188, 189, 190, 175,  29,  29,   0,
   0,   0,   0,   0,   0,   8,  18,  10,   0,  16,
  -2,   0,   0,  22,  23,  24, 198,  -2,   0,   0,
   0,   0,   0,   0,   0,   0,  41,  42,  43,  44,
  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,
  55,  56,  57,  58,  59,  60,  61,  62,  63,  64,
  65,  66,  67,  68,  69,   0,   0,  77,  78,  79,
  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
  90,   0,  93,  94,   0,  97,  98,  99, 100,   0,
 103, 104, 105, 106,   0,   0,   0,   0, 159, 162,
   0, 113, 114, 115, 116, 117, 118, 147, 150,  29,
   0, 195,  29, 166, 167, 176, 177,   0,   0, 168,
 170, 172, 174,   0,  16,   0,  17,   0,   0,   0,
  28,  32,  34,  36,  37,  38,  39,  40,  71,  74,
  76,  92,  96, 102, 108, 110, 111,   0,  29,  29,
 165, 152, 185, 154,  29, 156,   0,   0,   0,   0,
   0,  19,   0,  12,  13,  21,  26,   0, 158,   0,
 192, 193, 194, 160, 163,   0,  29, 196,   0, 178,
   0, 169, 171, 173,  15,  72,   0,   0,   0, 153,
 186, 157,   0,   0, 181,   0,   0, 191, 161, 164,
 179, 180,   0,   0,   0, 182, 183, 184 };
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
		if( ++yyps> &yys[YYMAXDEPTH] ) { syserr( "yacc stack overflow" ); return(1); }
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

			yyerror( yychar, yylval, yystate );
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
# line 47 "ucon.g"
{gout(globfile);} break;
case 4:
# line 52 "ucon.g"
{
		if (!nocode)
		   rout(globfile, STR0(yypvt[-0]));
		nocode = 0;
		loc_init();
		} break;
case 5:
# line 58 "ucon.g"
{
		if (!nocode)
		   codegen(yypvt[-0]);
		nocode = 0;
		treeinit();
		loc_init();
		} break;
case 7:
# line 67 "ucon.g"
{idflag = F_GLOBAL;} break;
case 9:
# line 68 "ucon.g"
{
#ifdef CMP
                idflag = F_BUILTIN|F_GLOBAL;
#endif
#ifdef INT
		idflag = F_GLOBAL;
		err("external declaration not allowed", 0);
#endif
		} break;
case 11:
# line 78 "ucon.g"
{idflag = F_ARGUMENT;} break;
case 12:
# line 78 "ucon.g"
{
		install(STR0(yypvt[-3]),F_RECORD|F_GLOBAL,(int)yypvt[-1]);
		yyval = yypvt[-3];
		} break;
case 13:
# line 83 "ucon.g"
{
		yyval = PROCNODE(yypvt[-5],yypvt[-2],yypvt[-1],yypvt[-0]);
		} break;
case 14:
# line 87 "ucon.g"
{idflag = F_ARGUMENT;} break;
case 15:
# line 87 "ucon.g"
{
		yyval = yypvt[-3];
		install(STR0(yypvt[-3]),F_PROC|F_GLOBAL,(int)yypvt[-1]);
		} break;
case 16:
# line 92 "ucon.g"
{yyval = (int)0;} break;
case 17:
# line 93 "ucon.g"
{yyval = (int)yypvt[-0];} break;
case 18:
# line 96 "ucon.g"
{
		install(STR0(yypvt[-0]),idflag,0);
		yyval = (int)1;
		} break;
case 19:
# line 100 "ucon.g"
{
		install(STR0(yypvt[-0]),idflag,0);
		yyval = (int)yypvt[-2] + 1;
		} break;
case 22:
# line 108 "ucon.g"
{idflag = F_DYNAMIC;} break;
case 23:
# line 109 "ucon.g"
{idflag = F_STATIC;} break;
case 24:
# line 110 "ucon.g"
{idflag = F_DYNAMIC;} break;
case 25:
# line 112 "ucon.g"
{yyval = EMPTYNODE;} break;
case 26:
# line 113 "ucon.g"
{yyval = yypvt[-1];} break;
case 27:
# line 115 "ucon.g"
{yyval = EMPTYNODE;} break;
case 28:
# line 116 "ucon.g"
{yyval = SLISTNODE(yypvt[-1], yypvt[-2], yypvt[-0]);} break;
case 29:
# line 118 "ucon.g"
{yyval = EMPTYNODE;} break;
case 32:
# line 122 "ucon.g"
{yyval = CONJNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 34:
# line 125 "ucon.g"
{yyval = SCANNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 36:
# line 128 "ucon.g"
{binop: yyval = BINOPNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 37:
# line 129 "ucon.g"
{yyval = AUGOPNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 38:
# line 130 "ucon.g"
{yyval = SCANNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 39:
# line 131 "ucon.g"
{yyval = CONJNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 40:
# line 132 "ucon.g"
{yyval = ACTIVNODE(yypvt[-1],yypvt[-0],yypvt[-2]);} break;
case 71:
# line 166 "ucon.g"
{yyval = TONODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 72:
# line 167 "ucon.g"
{yyval = TOBYNODE(yypvt[-3],yypvt[-4],yypvt[-2],yypvt[-0]);} break;
case 74:
# line 170 "ucon.g"
{yyval = ALTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 76:
# line 173 "ucon.g"
{goto binop;} break;
case 92:
# line 191 "ucon.g"
{goto binop;} break;
case 96:
# line 197 "ucon.g"
{goto binop;} break;
case 102:
# line 205 "ucon.g"
{goto binop;} break;
case 108:
# line 213 "ucon.g"
{goto binop;} break;
case 110:
# line 216 "ucon.g"
{yyval = LIMITNODE(yypvt[-2],yypvt[-0]);} break;
case 111:
# line 217 "ucon.g"
{yyval = ACTIVNODE(yypvt[-1], yypvt[-0], yypvt[-2]);} break;
case 113:
# line 220 "ucon.g"
{yyval = ACTIVNODE(yypvt[-1], yypvt[-0], EMPTYNODE);} break;
case 114:
# line 221 "ucon.g"
{yyval = NOTNODE(yypvt[-0]);} break;
case 115:
# line 222 "ucon.g"
{yyval = BARNODE(yypvt[-0]);} break;
case 116:
# line 223 "ucon.g"
{yyval = BARNODE(BARNODE(yypvt[-0]));} break;
case 117:
# line 224 "ucon.g"
{yyval = BARNODE(BARNODE(BARNODE(yypvt[-0])));} break;
case 118:
# line 225 "ucon.g"
{yyval = UNOPNODE(yypvt[-1],yypvt[-0]);} break;
case 147:
# line 256 "ucon.g"
{yyval = CREATENODE(yypvt[-1],yypvt[-0]);} break;
case 148:
# line 257 "ucon.g"
{VAL0(yypvt[-0]) = putloc(STR0(yypvt[-0]),0);} break;
case 149:
# line 258 "ucon.g"
{yyval = NEXTNODE(yypvt[-0]);} break;
case 150:
# line 259 "ucon.g"
{yyval = BREAKNODE(yypvt[-1],yypvt[-0]);} break;
case 151:
# line 260 "ucon.g"
{
#ifdef EXT
                  pushcs(0);
#endif
                  } break;
case 152:
# line 264 "ucon.g"
{
#ifdef EXT
                  popcs();
#endif
                  } break;
case 153:
# line 268 "ucon.g"
{
		  yyval = INVOKNODE(yypvt[-4],EMPTYNODE,yypvt[-2]);} break;
case 154:
# line 270 "ucon.g"
{yyval = yypvt[-1];} break;
case 155:
# line 271 "ucon.g"
{
#ifdef EXT
                  pushcs(0);
#endif
                  } break;
case 156:
# line 275 "ucon.g"
{
#ifdef EXT
                  popcs();
#endif
                  } break;
case 157:
# line 279 "ucon.g"
{
		yyval = LISTNODE(yypvt[-4],yypvt[-2]);} break;
case 158:
# line 281 "ucon.g"
{yyval = BINOPNODE(yypvt[-2],yypvt[-3],yypvt[-1]);} break;
case 159:
# line 282 "ucon.g"
{
#ifdef EXT
                 pushcs(1);
#endif
                 } break;
case 160:
# line 286 "ucon.g"
{
#ifdef EXT
                 popcs();
#else
                 err("missing semicolon or operator", 0);
#endif
                 } break;
case 161:
# line 292 "ucon.g"
{
		yyval = INVOKNODE(yypvt[-4],yypvt[-5],LISTNODE(yypvt[-4],yypvt[-2]));
		} break;
case 162:
# line 295 "ucon.g"
{
#ifdef EXT
                 pushcs(0);
#endif
                 } break;
case 163:
# line 299 "ucon.g"
{
#ifdef EXT
                 popcs();
#endif
                 } break;
case 164:
# line 303 "ucon.g"
{
		yyval = INVOKNODE(yypvt[-4],yypvt[-5],yypvt[-2]);
		} break;
case 165:
# line 306 "ucon.g"
{yyval = FIELDNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 166:
# line 307 "ucon.g"
{yyval = KEYNODE(yypvt[-1], K_FAIL);} break;
case 167:
# line 308 "ucon.g"
{
		if ((i = klocate(STR0(yypvt[-0]))) == NULL)
		   err("invalid keyword",STR0(yypvt[-0]));
		yyval = KEYNODE(yypvt[-1], i);
		} break;
case 168:
# line 314 "ucon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 169:
# line 315 "ucon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 170:
# line 317 "ucon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 171:
# line 318 "ucon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 172:
# line 320 "ucon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 173:
# line 321 "ucon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 174:
# line 323 "ucon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 175:
# line 325 "ucon.g"
{yyval = RETNODE(yypvt[-0],EMPTYNODE);} break;
case 176:
# line 326 "ucon.g"
{yyval = RETNODE(yypvt[-1],yypvt[-0]);} break;
case 177:
# line 327 "ucon.g"
{yyval = SUSPNODE(yypvt[-1],yypvt[-0]);} break;
case 178:
# line 329 "ucon.g"
{yyval = IFNODE(yypvt[-3],yypvt[-2],yypvt[-0],EMPTYNODE);} break;
case 179:
# line 330 "ucon.g"
{yyval = IFNODE(yypvt[-5],yypvt[-4],yypvt[-2],yypvt[-0]);} break;
case 180:
# line 332 "ucon.g"
{yyval = CASENODE(yypvt[-5],yypvt[-4],yypvt[-1]);} break;
case 182:
# line 335 "ucon.g"
{yyval = CLISTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 183:
# line 337 "ucon.g"
{yyval = CCLSNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 184:
# line 338 "ucon.g"
{yyval = CCLSNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 185:
# line 340 "ucon.g"
{
#ifdef EXT
                 yyval = cswitch(yypvt[-0],yypvt[-0]);
#endif
                 } break;
case 186:
# line 345 "ucon.g"
{
#ifdef EXT
                 yyval = ELISTNODE(yypvt[-1],yypvt[-2],cswitch(yypvt[-0],yypvt[-1]));
#else
                yyval = ELISTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);
#endif
                } break;
case 187:
# line 353 "ucon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_INTLIT,0);} break;
case 188:
# line 354 "ucon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_REALLIT,0);} break;
case 189:
# line 355 "ucon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_STRLIT,VAL1(yypvt[-0]));} break;
case 190:
# line 356 "ucon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_CSETLIT,VAL1(yypvt[-0]));} break;
case 191:
# line 358 "ucon.g"
{yyval = SECTNODE(yypvt[-2],yypvt[-5],yypvt[-3],yypvt[-1]);} break;
case 196:
# line 365 "ucon.g"
{yyval = SLISTNODE(yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		}
		goto yystack;  /* stack new state and value */

	}
