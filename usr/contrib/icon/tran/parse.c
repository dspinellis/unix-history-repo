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
# define FAIL 273
# define GLOBAL 274
# define IF 275
# define INITIAL 276
# define LINK 277
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

# line 123 "icon.g"
#include "itran.h"
#include "sym.h"
#include "tree.h"
#include "../h/keyword.h"
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

# line 134 "icon.g"
int argcnt;
int idflag;
int i;
#ifdef XPX
int cstack[50];			/* context stack expression lists */
int stacktop = 0;		/* stack top */
nodeptr cswitch();
#endif XPX
# define YYERRCODE 256

# line 469 "icon.g"


#ifdef XPX
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

nodeptr cswitch(x,y)
	nodeptr x, y;
	{
	if (cstack[stacktop]) return(CREATENODE(y,x));
	else return(x);
}
#endif XPX
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 21,
	359, 32,
	-2, 30,
-1, 104,
	359, 32,
	-2, 30,
-1, 111,
	359, 32,
	-2, 30,
	};
# define YYNPROD 204
# define YYLAST 642
short yyact[]={

  32,  89, 170,  60,  86,  87,  88,  62, 207,  94,
  59, 281, 287, 112, 175, 240,  97,  90, 288,  93,
 172,  21, 111,  61,  45, 285, 275, 270,  98,  91,
 177,  92, 169, 249,  96,  95, 284,  44, 112, 277,
 254, 239, 233, 256, 208, 113, 103, 112, 171, 166,
 176, 251, 174, 252,  85,  68,  46,  73, 290, 266,
 167,  47, 112,  66,  69, 258,  67,  81,  74, 243,
  64,  65,  48, 283,  79, 246, 278, 257, 245,  80,
  63, 244,  76, 112, 184, 262,  84,  77, 182, 181,
 207, 242,  78,  25,  70, 289,  83, 250, 183, 110,
 253, 280, 100, 247,  72, 198,  71, 112, 236,  75,
  82,  32,  89, 101,  60,  86,  87,  88,  62, 197,
  94,  59, 112,  26,  20, 112, 112,  97,  90, 112,
  93,   3, 112, 180,  61,  45, 194,  40,  17,  98,
  91,  18,  92, 209,  38,  96,  95, 109,  44,  31,
 179,  37,  28, 178, 106,  39, 107, 210,  16,  35,
 255, 279, 274, 235, 108,  85,  68,  46,  73, 273,
  24, 234,  47,  30,  66,  69, 264,  67,  81,  74,
  99,  64,  65,  48, 102,  79,  12, 196,   2,   9,
  80,  63,  14,  76,  13,  10, 237,  84,  77, 261,
 193,  58,  57,  78,  56,  70,  55,  83,  54,  53,
  52,  51,  50,  49,  43,  72,  41,  71, 173, 168,
  75,  82,  89, 165,  60,  86,  87,  88,  62, 150,
  94,  59,  36, 191, 115,   4, 114,  97,  90,  34,
  93, 105,  23, 104,  61,  45,  27,  11,  19,  98,
  91,  12,  92,  22,   9,  96,  95, 211,  44,  13,
  10,  15, 215, 212, 214,   8,   7, 201, 202, 203,
 204, 205, 206,  42,   6,  85,  68,  46,  73, 222,
 213,   5,  47,   1,  66,  69,   0,  67,  81,  74,
   0,  64,  65,  48,   0,  79,   0,   0,   0,   0,
  80,  63, 224,  76,   0,   0,   0,  84,  77, 223,
 225, 227,   0,  78,   0,  70, 228,  83, 185, 186,
 187, 188, 189, 190, 226,  72,   0,  71,   0, 119,
  75,  82, 121,   0, 118, 117, 135, 136, 137, 138,
 139, 140, 141, 142, 143, 144, 145, 146, 147, 148,
   0,   0, 248, 149, 134, 232,   0,   0, 124,  33,
   0, 126,   0,   0, 163, 131,   0,   0,   0, 125,
   0, 151, 152, 153, 154, 155, 156, 263, 238, 129,
   0, 133,   0, 164, 157, 158, 159, 160, 161, 162,
 291, 128,   0, 241,   0, 123, 122,   0, 116,   0,
  29, 132,   0, 130, 120,   0,   0, 127,   0,   0,
   0,   0,   0, 271,   0,   0, 265,   0, 267, 268,
 269,   0,   0,   0,   0,   0,   0,   0,   0, 272,
   0, 259, 260,   0,   0,   0,   0,   0,   0,   0,
 282, 192,   0, 195,   0,   0,   0,   0,   0,   0,
   0,   0, 286, 229, 230,   0,   0,   0,   0,   0,
   0,   0, 282, 292, 293,   0,   0,   0,   0,   0,
 199, 200,   0, 216, 217, 218, 219, 220, 221,   0,
   0,   0,   0,  29,   0,   0,   0,   0,   0,   0,
  29,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 231,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 195,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0, 276 };
short yypact[]={

-125,-1000, -23,-1000,-1000,-1000,-1000,-1000,-1000,-121,
-1000,-235,-1000,-1000, -88,-223,-1000,-1000,-1000,-136,
-1000,-145,-157,-146,-1000,-121,-290,-122,-172,-337,
-236,-307,-1000,-1000,  40,-1000,  41,-268,-318,-310,
-1000,-160,-1000,-238, -35, -35, -35, -35, -35, -35,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-145,
-1000,-1000,-145,-1000,-145,-1000,-154,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-145,-145,-145,-145,-145,-145,-145,-145,-226,
-1000,-292,-1000,-157,-145,-157,-145,-1000,-1000,-1000,
-1000,-145, -35, -35, -35, -35, -35, -35, -35, -35,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000, -35,
 -35,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, -35,-1000,-1000, -35,-1000,
-1000,-1000,-1000, -35,-1000,-1000,-1000,-1000, -35, -35,
 -35,-145,-311,-1000,-151,-1000,-1000,-1000,-1000,-1000,
-1000,-236,-1000,-145,-312,-344,-145,-1000,-1000,-1000,
-1000,-197,-212,-187,-190,-193,-236,-156,-157,-324,
-226,-174,-308,-306,-1000,-307,-1000,-1000,-1000,-1000,
-1000,-1000,-164,-1000,-268,-318,-310,-1000,-1000,-1000,
-1000,-314,-272,-1000,-145,-145,-1000,-231,-1000,-1000,
-145,-231,-145,-267,-145,-145,-145,-1000,-330,-1000,
-1000,-1000,-1000, -35,-1000,-145,-1000,-1000,-1000,-231,
-231,-331,-145,-1000,-315,-194,-256,-236,-236,-236,
-1000,-1000,-281,-317,-332,-1000,-1000,-1000,-145,-341,
-1000,-220,-257,-1000,-1000,-1000,-236,-1000,-256,-145,
-145,-1000,-236,-236 };
short yypgo[]={

   0, 283, 188, 281, 274, 266, 265, 261, 158, 253,
 157, 248, 143, 247, 246, 243, 152, 242, 241, 173,
 378, 149, 359, 239, 236, 234, 159, 232, 151, 229,
 144, 223, 155, 219, 137, 218, 216, 273, 214, 213,
 212, 211, 210, 209, 208, 206, 204, 202, 201, 200,
 196, 199, 136, 187, 176, 171, 169, 163, 162, 161,
 101, 160 };
short yyr1[]={

   0,   1,   2,   2,   3,   3,   3,   3,   7,   7,
   8,   8,   9,   6,  11,   4,   5,  17,  13,  12,
  12,  10,  10,  14,  14,  18,  18,  18,  15,  15,
  16,  16,  20,  20,  19,  19,  21,  21,  22,  22,
  22,  22,  22,  22,  24,  24,  24,  24,  25,  25,
  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,
  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,
  25,  25,  25,  23,  23,  23,  26,  26,  27,  27,
  29,  29,  29,  29,  29,  29,  29,  29,  29,  29,
  29,  29,  29,  29,  28,  28,  31,  31,  30,  30,
  33,  33,  33,  33,  32,  32,  35,  35,  35,  35,
  34,  34,  36,  36,  36,  37,  37,  37,  37,  37,
  37,  37,  39,  39,  39,  39,  39,  39,  39,  39,
  39,  39,  39,  39,  39,  39,  39,  39,  39,  39,
  39,  38,  38,  38,  38,  38,  38,  38,  38,  38,
  38,  38,  38,  38,  49,  51,  38,  38,  53,  54,
  38,  38,  38,  55,  56,  38,  57,  58,  38,  38,
  38,  38,  45,  45,  46,  46,  47,  47,  48,  42,
  42,  42,  43,  43,  44,  59,  59,  60,  60,  50,
  50,  40,  40,  40,  40,  41,  61,  61,  61,  52,
  52,   1,   5,  19 };
short yyr2[]={

   0,   2,   0,   2,   1,   1,   1,   2,   1,   3,
   1,   1,   0,   3,   0,   6,   6,   0,   6,   0,
   1,   1,   3,   0,   4,   1,   1,   1,   0,   3,
   0,   3,   0,   1,   1,   3,   1,   3,   1,   3,
   3,   3,   3,   3,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   3,   5,   1,   3,   1,   3,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   3,   1,   1,   1,   3,
   1,   1,   1,   1,   1,   3,   1,   1,   1,   1,
   1,   3,   1,   3,   3,   1,   2,   2,   2,   2,
   2,   2,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   2,   1,   1,   2,   0,   0,   5,   3,   0,   0,
   5,   4,   3,   0,   0,   6,   0,   0,   6,   3,
   2,   2,   2,   4,   2,   4,   2,   4,   2,   1,
   2,   2,   4,   6,   6,   1,   3,   3,   3,   1,
   3,   1,   1,   1,   1,   6,   1,   1,   1,   1,
   3,   3,   4,   1 };
short yychk[]={

-1000,  -1,  -2, 256, 258,  -3,  -4,  -5,  -6, 277,
 283, -13, 274, 282,  -2,  -7,  -8, 259, 262, -11,
 359, 256,  -9, -17, 258, 316, 259, -14, -16, -20,
 -19, -21, 256, -22, -23, -26, -27, -28, -30, -32,
 -34, -36, -37, -38, 293, 280, 312, 317, 328, -39,
 -40, -41, -42, -43, -44, -45, -46, -47, -48, 266,
 259, 279, 263, 336, 326, 327, 319, 322, 311, 320,
 350, 362, 360, 313, 324, 365, 338, 343, 348, 330,
 335, 323, 366, 352, 342, 310, 260, 261, 262, 257,
 273, 285, 287, 275, 265, 291, 290, 272, 284, -10,
 259, 259,  -8, 336, -15, -18, 276, 278, 286, 269,
 271, 359, 319, 352, -24, -25, 358, 295, 294, 289,
 364, 292, 356, 355, 318, 329, 321, 367, 351, 339,
 363, 325, 361, 341, 314, 296, 297, 298, 299, 300,
 301, 302, 303, 304, 305, 306, 307, 308, 309, 312,
 -29, 330, 331, 332, 333, 334, 335, 343, 344, 345,
 346, 347, 348, 323, 342, -31, 317, 328, -33, 350,
 320, 366, 338, -35, 362, 324, 360, 340, 313, 310,
 293, 327, 326, 336, 322, -37, -37, -37, -37, -37,
 -37, -19, -20, -49, -52, -20, -53, 273, 259, -20,
 -20, -19, -19, -19, -19, -19, -19, 316, 336, -12,
 -10, -16, -10, -19, -16, -21, -22, -22, -22, -22,
 -22, -22, -26, -26, -28, -30, -32, -34, -34, -37,
 -37, -20, -19, 353, -55, -57, 259, -50, -20, 353,
 359, -50, 288, 281, 268, 268, 268, 259, -12, 357,
 271, 359, 359, 264, 354, -61, 315, 349, 337, -50,
 -50, -51, 316, -52, -54, -19, 326, -19, -19, -19,
 357, -26, -19, -56, -58, 357, -20, 354, 270, -59,
 -60, 267, -19, 354, 353, 357, -19, 353, 359, 315,
 315, -60, -19, -19 };
short yydef[]={

   2,  -2,   0,   2,   1,   3,   4,   5,   6,   0,
  14,   0,  12,  17,   0,   7,   8,  10,  11,   0,
  23,  -2,   0,   0, 201,   0,   0,  28,   0,   0,
  33,  34, 203,  36,  38,  73,  76,  78,  94,  98,
 104, 110, 112, 115,   0,   0,   0,   0,   0,   0,
 141, 142, 143, 144, 145, 146, 147, 148, 149,   0,
 151, 152,  32, 154,  32, 158,   0, 122, 123, 124,
 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
 135, 136, 137, 138, 139, 140, 191, 192, 193, 194,
 179,  32,  32,   0,   0,   0,   0,   0,   0,  13,
  21,   0,   9,  19,  -2,   0,   0,  25,  26,  27,
 202,  -2,   0,   0,   0,   0,   0,   0,   0,   0,
  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,
  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,
  64,  65,  66,  67,  68,  69,  70,  71,  72,   0,
   0,  80,  81,  82,  83,  84,  85,  86,  87,  88,
  89,  90,  91,  92,  93,   0,  96,  97,   0, 100,
 101, 102, 103,   0, 106, 107, 108, 109,   0,   0,
   0,  32, 163, 166,   0, 116, 117, 118, 119, 120,
 121, 150, 153,  32,   0, 199,  32, 170, 171, 180,
 181,   0,   0, 172, 174, 176, 178,   0,  19,   0,
  20,   0,   0,   0,  31,  35,  37,  39,  40,  41,
  42,  43,  74,  77,  79,  95,  99, 105, 111, 113,
 114,   0,  33, 162,  32,  32, 169, 155, 189, 157,
  32, 159,   0,   0,   0,   0,   0,  22,   0,  15,
  16,  24,  29,   0, 161,   0, 196, 197, 198, 164,
 167,   0,  32, 200,   0, 182,   0, 173, 175, 177,
  18,  75,   0,   0,   0, 156, 190, 160,   0,   0,
 185,   0,   0, 195, 165, 168, 183, 184,   0,   0,
   0, 186, 187, 188 };
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
# line 144 "icon.g"
{gout(globfile);} break;
case 4:
# line 149 "icon.g"
{
		if (!nocode)
			rout(globfile, STR0(yypvt[-0]));
		nocode = 0;
		loc_init();
		} break;
case 5:
# line 155 "icon.g"
{
		if (!nocode)
			codegen(yypvt[-0]);
		nocode = 0;
		treeinit();
		loc_init();
		} break;
case 10:
# line 168 "icon.g"
{addlfile(STR0(yypvt[-0]));} break;
case 11:
# line 169 "icon.g"
{addlfile(STR0(yypvt[-0]));} break;
case 12:
# line 171 "icon.g"
{idflag = F_GLOBAL;} break;
case 14:
# line 173 "icon.g"
{idflag = F_ARGUMENT;} break;
case 15:
# line 173 "icon.g"
{
		install(STR0(yypvt[-3]),F_RECORD|F_GLOBAL,(int)yypvt[-1]);
		yyval = yypvt[-3];
		} break;
case 16:
# line 178 "icon.g"
{
		yyval = (nodeptr)PROCNODE(yypvt[-5],yypvt[-2],yypvt[-1],yypvt[-0]);
		} break;
case 17:
# line 182 "icon.g"
{idflag = F_ARGUMENT;} break;
case 18:
# line 182 "icon.g"
{
		yyval = yypvt[-3];
		install(STR0(yypvt[-3]),F_PROC|F_GLOBAL,(int)yypvt[-1]);
		} break;
case 19:
# line 187 "icon.g"
{yyval = (int)0;} break;
case 20:
# line 188 "icon.g"
{yyval = (nodeptr)yypvt[-0];} break;
case 21:
# line 191 "icon.g"
{
		install(STR0(yypvt[-0]),idflag,0);
		yyval = (nodeptr)1;
		} break;
case 22:
# line 195 "icon.g"
{
		install(STR0(yypvt[-0]),idflag,0);
		yyval = (nodeptr)((int)yypvt[-2] + 1);
		} break;
case 25:
# line 203 "icon.g"
{idflag = F_DYNAMIC;} break;
case 26:
# line 204 "icon.g"
{idflag = F_STATIC;} break;
case 27:
# line 205 "icon.g"
{idflag = F_DYNAMIC;} break;
case 28:
# line 207 "icon.g"
{yyval = EMPTYNODE;} break;
case 29:
# line 208 "icon.g"
{yyval = yypvt[-1];} break;
case 30:
# line 210 "icon.g"
{yyval = EMPTYNODE;} break;
case 31:
# line 211 "icon.g"
{yyval = SLISTNODE(yypvt[-1], yypvt[-2], yypvt[-0]);} break;
case 32:
# line 213 "icon.g"
{yyval = EMPTYNODE;} break;
case 35:
# line 217 "icon.g"
{yyval = CONJNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 37:
# line 220 "icon.g"
{yyval = SCANNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 39:
# line 223 "icon.g"
{binop: yyval = BINOPNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 40:
# line 224 "icon.g"
{yyval = AUGOPNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 41:
# line 225 "icon.g"
{yyval = SCANNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 42:
# line 226 "icon.g"
{yyval = CONJNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 43:
# line 227 "icon.g"
{yyval = ACTIVNODE(yypvt[-1],yypvt[-0],yypvt[-2]);} break;
case 74:
# line 261 "icon.g"
{yyval = TONODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 75:
# line 262 "icon.g"
{yyval = TOBYNODE(yypvt[-3],yypvt[-4],yypvt[-2],yypvt[-0]);} break;
case 77:
# line 265 "icon.g"
{yyval = ALTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 79:
# line 268 "icon.g"
{goto binop;} break;
case 95:
# line 286 "icon.g"
{goto binop;} break;
case 99:
# line 292 "icon.g"
{goto binop;} break;
case 105:
# line 300 "icon.g"
{goto binop;} break;
case 111:
# line 308 "icon.g"
{goto binop;} break;
case 113:
# line 311 "icon.g"
{yyval = LIMITNODE(yypvt[-2],yypvt[-0]);} break;
case 114:
# line 312 "icon.g"
{yyval = ACTIVNODE(yypvt[-1], yypvt[-0], yypvt[-2]);} break;
case 116:
# line 315 "icon.g"
{yyval = ACTIVNODE(yypvt[-1], yypvt[-0], EMPTYNODE);} break;
case 117:
# line 316 "icon.g"
{yyval = NOTNODE(yypvt[-0]);} break;
case 118:
# line 317 "icon.g"
{yyval = BARNODE(yypvt[-0]);} break;
case 119:
# line 318 "icon.g"
{yyval = BARNODE(yypvt[-0]);} break;
case 120:
# line 319 "icon.g"
{yyval = BARNODE(yypvt[-0]);} break;
case 121:
# line 320 "icon.g"
{yyval = UNOPNODE(yypvt[-1],yypvt[-0]);} break;
case 150:
# line 351 "icon.g"
{yyval = CREATENODE(yypvt[-1],yypvt[-0]);} break;
case 151:
# line 352 "icon.g"
{VAL0(yypvt[-0]) = putloc(STR0(yypvt[-0]),0);} break;
case 152:
# line 353 "icon.g"
{yyval = NEXTNODE(yypvt[-0]);} break;
case 153:
# line 354 "icon.g"
{yyval = BREAKNODE(yypvt[-1],yypvt[-0]);} break;
case 154:
# line 355 "icon.g"
{
#ifdef XPX
		pushcs(0);
#endif XPX
		} break;
case 155:
# line 359 "icon.g"
{
#ifdef XPX
		popcs();
#endif XPX
		} break;
case 156:
# line 363 "icon.g"
{
			if ((yypvt[-2])->n_type == N_ELIST)
			yyval = INVOKNODE(yypvt[-4],EMPTYNODE,yypvt[-2]);
			else
			yyval = yypvt[-2]; } break;
case 157:
# line 368 "icon.g"
{yyval = yypvt[-1];} break;
case 158:
# line 369 "icon.g"
{
#ifdef XPX
		pushcs(0);
#endif XPX
		} break;
case 159:
# line 373 "icon.g"
{
#ifdef XPX
		popcs();
#endif XPX
		} break;
case 160:
# line 377 "icon.g"
{
		yyval = LISTNODE(yypvt[-4],yypvt[-2]);} break;
case 161:
# line 379 "icon.g"
{yyval = BINOPNODE(yypvt[-2],yypvt[-3],yypvt[-1]);} break;
case 162:
# line 380 "icon.g"
{yyval = INVOKNODE(yypvt[-1],yypvt[-2],LISTNODE(yypvt[-1],EMPTYNODE));} break;
case 163:
# line 381 "icon.g"
{
#ifdef XPX
		pushcs(1);
#endif XPX
		} break;
case 164:
# line 385 "icon.g"
{
#ifdef XPX
		popcs();
#else XPX
                 err("missing semicolon or operator", 0);
#endif XPX
		} break;
case 165:
# line 391 "icon.g"
{
		yyval = INVOKNODE(yypvt[-4],yypvt[-5],LISTNODE(yypvt[-4],yypvt[-2]));
		} break;
case 166:
# line 394 "icon.g"
{
#ifdef XPX
		pushcs(0);
#endif XPX
		} break;
case 167:
# line 398 "icon.g"
{
#ifdef XPX
		popcs();
#endif XPX
		} break;
case 168:
# line 402 "icon.g"
{
		yyval = INVOKNODE(yypvt[-4],yypvt[-5],yypvt[-2]);
		} break;
case 169:
# line 405 "icon.g"
{yyval = FIELDNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 170:
# line 406 "icon.g"
{yyval = KEYNODE(yypvt[-1], K_FAIL);} break;
case 171:
# line 407 "icon.g"
{
		if ((i = klocate(STR0(yypvt[-0]))) == NULL)
			err("invalid keyword",STR0(yypvt[-0]));
		yyval = KEYNODE(yypvt[-1], i);
		} break;
case 172:
# line 413 "icon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 173:
# line 414 "icon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 174:
# line 416 "icon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 175:
# line 417 "icon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 176:
# line 419 "icon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 177:
# line 420 "icon.g"
{yyval = LOOPNODE(yypvt[-3],yypvt[-2],yypvt[-0]);} break;
case 178:
# line 422 "icon.g"
{yyval = LOOPNODE(yypvt[-1],yypvt[-0],EMPTYNODE);} break;
case 179:
# line 424 "icon.g"
{yyval = RETNODE(yypvt[-0],EMPTYNODE);} break;
case 180:
# line 425 "icon.g"
{yyval = RETNODE(yypvt[-1],yypvt[-0]);} break;
case 181:
# line 426 "icon.g"
{yyval = SUSPNODE(yypvt[-1],yypvt[-0]);} break;
case 182:
# line 428 "icon.g"
{yyval = IFNODE(yypvt[-3],yypvt[-2],yypvt[-0],EMPTYNODE);} break;
case 183:
# line 429 "icon.g"
{yyval = IFNODE(yypvt[-5],yypvt[-4],yypvt[-2],yypvt[-0]);} break;
case 184:
# line 431 "icon.g"
{yyval = CASENODE(yypvt[-5],yypvt[-4],yypvt[-1]);} break;
case 186:
# line 434 "icon.g"
{yyval = CLISTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 187:
# line 436 "icon.g"
{yyval = CCLSNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 188:
# line 437 "icon.g"
{yyval = CCLSNODE(yypvt[-1],yypvt[-2],yypvt[-0]);} break;
case 189:
# line 439 "icon.g"
{
#ifdef XPX
		yyval = cswitch(yypvt[-0],yypvt[-0]);
#endif XPX
		} break;
case 190:
# line 444 "icon.g"
{
#ifdef XPX
		yyval = ELISTNODE(yypvt[-1],yypvt[-2],cswitch(yypvt[-0],yypvt[-1]));
#else XPX
                yyval = ELISTNODE(yypvt[-1],yypvt[-2],yypvt[-0]);
#endif XPX
		} break;
case 191:
# line 452 "icon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_INTLIT,0);} break;
case 192:
# line 453 "icon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_REALLIT,0);} break;
case 193:
# line 454 "icon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_STRLIT,VAL1(yypvt[-0]));} break;
case 194:
# line 455 "icon.g"
{VAL0(yypvt[-0]) = putlit(STR0(yypvt[-0]),F_CSETLIT,VAL1(yypvt[-0]));} break;
case 195:
# line 457 "icon.g"
{yyval = (nodeptr)SECTNODE(yypvt[-2],yypvt[-5],yypvt[-3],yypvt[-1]);} break;
case 200:
# line 464 "icon.g"
{yyval = SLISTNODE(yypvt[-1], yypvt[-2], yypvt[-0]);} break;
		}
		goto yystack;  /* stack new state and value */

	}
