
# line 15 "grammar.z"
/* SCANNER/PARSER GLOBALS & TABLES */
# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	<pv.h>
# include	"parser.h"
# include	<sccs.h>

SCCSID(@(#)grammar.y	7.2	4/7/82)

# ifdef		xPTR1
# define	YYDEBUG
# endif

int				i;
struct atstash			*aptr;
char				permbuf[3];
/* space for two names, their null bytes and the seperator */
char				modbuf[(2 * (MAXNAME + 1)) + 1];
static char			hqmbuf[2];

extern DESC			Reldesc;
extern int			Opflag;
extern QTREE			*Lastree;
extern QTREE			*Tidnode;
extern int			Rsdmno;
extern int			Resrng;
extern int			Qrymod;
extern int			Permcomd;
extern char			*Trname;
extern int			Qlflag;
extern struct atstash 		Faketid;

# ifdef	DISTRIB
extern struct atstash		Fakesid;
# endif

extern int			Patflag;
extern char			*Indexname;

extern QTREE			*tree();
extern QTREE			*tlprepend();
extern QTREE			*addresdom();
extern QTREE			*xdot();
extern QTREE			*norml();
extern struct atstash		*attlookup();
extern int			rngent();
extern int			rnglook();
extern PARRNG			Parrng[];

# line 71 "grammar.z"
typedef union 
{
	int				type_type;	/* OPERATOR TYPES ETC. */
	QTREE				*tree_type;
	int				rng_type;
	char				char_type;
	int				int_type;
	short				*I2_type;
	long				*I4_type;
	float				*F4_type;
	double				*F8_type;
	char				*string_type;
} YYSTYPE;
# define APPEND 257
# define COPY 258
# define CREATE 259
# define DELETE 260
# define DESTROY 261
# define HELP 262
# define INDEX 263
# define MODIFY 264
# define PRINT 265
# define RANGE 266
# define REPLACE 267
# define RETRIEVE 268
# define SAVE 269
# define DEFINE 270
# define PERMIT 271
# define VIEW 272
# define INTEGRITY 273
# define ALL 274
# define BY 275
# define FROM 276
# define IN 277
# define INTO 278
# define UNIQUE 279
# define AT 280
# define IS 281
# define OF 282
# define ON 283
# define ONTO 284
# define TO 285
# define UNTIL 286
# define WHERE 287
# define NAME 288
# define SCONST 289
# define I2CONST 290
# define I4CONST 291
# define F4CONST 292
# define F8CONST 293
# define COMMA 294
# define LPAREN 295
# define PERIOD 296
# define RPAREN 297
# define COLON 298
# define BGNCMNT 299
# define ENDCMNT 300
# define UAOP 301
# define BAOP 302
# define BAOPH 303
# define BDOP 304
# define EOP 305
# define LBOP 306
# define LUOP 307
# define FOP 308
# define FBOP 309
# define AGOP 310
# define unaryop 311
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 1091 "grammar.z"

# include	"scanner.h"
# include	"tables.y"
# include	"yyerror.y"
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 35,
	295, 45,
	-2, 49,
-1, 195,
	296, 128,
	-2, 134,
	};
# define YYNPROD 203
# define YYLAST 485
short yyact[]={

 183, 314, 195, 194, 190, 191, 192, 193, 270, 180,
 225, 230, 142, 284, 331, 197, 320, 225, 287, 234,
 326, 181, 187, 188, 189, 195, 194, 190, 191, 192,
 193, 234, 237, 231, 230, 271, 297, 316, 197, 232,
 231, 230, 235, 233, 207, 187, 188, 189, 144, 296,
 241, 232, 231, 230, 235, 233, 232, 231, 230, 204,
 240, 321, 232, 231, 230, 232, 231, 230, 295, 301,
  70, 271, 232, 231, 230, 232, 231, 230, 232, 231,
 230, 267, 210, 200, 266, 211, 199, 122, 135, 134,
 162, 136,  60,  62, 239, 144, 238, 108, 168, 120,
 132,  87, 323,  76, 119,  81, 286, 106, 208, 122,
 127, 126, 125,  61,  73, 178, 307,  77,  78, 306,
 332, 330, 325, 318, 311,  61, 253, 293, 178, 159,
  72,  77,  78,  69, 157,  68, 280, 265, 328, 244,
 319, 292, 285, 304, 261,  61, 221, 245, 246, 147,
  61, 264, 173, 245, 222, 117, 166, 161, 150, 137,
 124, 123,  80,  56, 206, 144, 258, 260,  59,  57,
  58, 104, 103, 141, 329, 324, 218,  85, 105, 130,
 308, 227,  86,  83,  84, 223, 216, 236,  99,  98,
 154, 156, 112, 158, 100,  96,  66,  64, 203, 251,
 302, 263, 243,  65, 202, 205, 171, 164, 249, 291,
 248, 220, 176,  19,  38,  39,  40,  41,  42,  43,
  53,  46,  47,  48,  49,  50,  51,  45, 177, 201,
 272, 273, 274, 275,  93, 179, 198,  92, 276, 277,
 278, 279, 196, 146,  94,  91, 114, 113, 115,  89,
  75,  90, 110, 109, 111, 107, 116,   2,  67,  54,
 175, 174,  36,  32, 283, 257, 259, 215, 256, 214,
 165,  30,  44,  28,  79,  27,  74,  26,  71, 250,
 252, 254,  25, 255,  24,  22, 155, 153, 247, 152,
  21,  95, 298, 143, 163,  82,  29, 312, 315, 290,
 289, 305, 133, 288, 262, 217, 131,  88,  31,  52,
  37, 102, 118, 101,  35, 317,  97,  34,  63,  23,
 121,  55,  20,  33, 315, 327, 322,  18,  17,  16,
  15,  14,  13,  12,  11,  10,   9,   8,   7, 129,
 128,   6,   5,   4,   3, 149, 186, 185, 313, 229,
 184, 148, 252, 300, 138, 182, 145, 139, 219, 140,
 167,   1,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0, 151,   0,   0, 160,   0,   0,
   0,   0, 169,   0, 209,   0, 212,   0,   0,   0,
 170,   0,   0,   0, 172,   0,   0,   0,   0, 213,
   0,   0,   0,   0, 224,   0,   0, 310,   0,   0,
 309,   0,   0,   0,   0,   0, 226, 228,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 149,   0,   0,
   0,   0,   0, 148, 242,   0,   0,   0,   0,   0,
   0,   0,   0, 268, 303,   0,   0,   0,   0,   0,
   0, 269,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 294,
 281, 299,   0,   0, 282 };
short yypact[]={

 -43, -43,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-115,-143,-143, -80,-138,-160,-171,-126,-190,-100,
-143, -23,-143, -87, -89,-107,-143,-198,-1000,-1000,
-1000,-1000,-1000, -19, -91, -25,-1000,-1000,-1000,-1000,
-1000,-1000,-133,-1000,-1000,-133,-1000,-1000,-1000,-1000,
-191,-1000,-196,-133,-1000,-1000,-1000,-185,-127,-128,
-1000,-182,-1000,-1000,-183,-1000,-1000,-1000,-1000,-184,
-1000,-143,-133,-1000,-1000,-1000,-1000,-106,-194,-1000,
-1000,-1000,-1000,-1000,-1000,-185,-129,-133,-1000,-1000,
-1000,-198,-133,-1000,-1000,-1000,-113,-122,-139,-1000,
-1000,-1000,-130,-1000,-1000,-1000,-1000,-1000,-198,-143,
-143,-122,-143,-1000,-1000,-161,-157,-131,-207, -74,
-132,-197, -23,-133,-1000,-1000,-1000, -75,-198,-122,
-1000,-175,-1000,-286,-1000,-211,-1000, -77,-1000,-237,
 -76,-122,-253,-186, -77,-212, -77,-1000,-1000,-1000,
-1000,-1000,-1000,-286,-1000, -97,-1000,-109,-142,-1000,
-1000,-134,-122,-1000,-1000,-162,-1000,-1000,-1000,-296,
-286,-286,-1000,-250,-1000,-1000,-263,-199,-201,-235,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-246,-1000,
-139,-263,-1000,-1000,-135,-140,-1000, -68,-143,-163,
-143,-1000,-143,-296,-121,-144,-1000, -79,-137,-213,
-1000,-1000,-1000,-1000,-162,-286,-289,-262,-1000,-263,
-263,-263,-263,-1000,-1000,-1000,-1000,-263,-263,-263,
-263,-141,-1000,-223,-1000,-1000,-1000,-153,-1000,-1000,
 -77,-1000,-1000,-1000, -77,-1000,-1000,-146,-1000,-188,
-1000,-280, -67,-147,-1000,-1000,-1000,-142,-1000,-1000,
-1000,-1000,-223,-1000,-292,-269,-226,-229,-245,-239,
-1000,-163,-143,-225,-1000, -81,-144,-145,-122,-103,
-103,-166,-1000,-1000,-1000,-1000,-263,-263,-260,-1000,
-1000,-146,-167,-1000,-1000,-1000,-1000,-1000,-148,-1000,
-1000,-282,-236,-192,-1000,-223,-1000,-1000,-1000,-110,
-168,-1000,-277,-263,-150,-111,-1000,-1000,-1000,-169,
-284,-170,-1000 };
short yypgo[]={

   0, 361, 360, 358, 211, 255, 356, 243,  12, 235,
 355,   0, 350, 349, 348,   1, 242, 236, 347, 346,
  70, 257, 344, 343, 342, 341, 338, 337, 336, 335,
 334, 333, 332, 331, 330, 329, 328, 327, 323, 322,
 321, 256, 319, 318, 317, 316, 314, 313, 311, 310,
 309, 308, 307, 306, 305, 304, 303, 249, 302, 300,
 119, 299, 116, 296, 295, 294, 229, 293, 290, 289,
 288, 287, 199, 286, 285, 284, 258, 282, 278, 277,
 276, 275, 274, 250, 273, 272, 271, 270, 269, 268,
 267, 266, 167, 265, 264,  13, 263, 262, 261, 260,
 228 };
short yyr1[]={

   0,   1,   1,   1,  21,  21,  21,  21,  21,  21,
  21,  21,  21,  21,  21,  21,  21,  21,  21,  21,
  21,  33,  38,  22,  39,  40,  40,  40,  40,  40,
  25,  42,  43,  43,  43,  43,  34,  44,  45,  45,
  45,  45,  35,  46,  47,  47,  47,  48,  48,  48,
  37,  49,  50,  31,  51,  52,  52,  57,  57,  57,
  57,  57,  53,  58,  58,  58,   2,   2,   3,   3,
   4,  54,  54,  55,  55,  55,  56,  56,  56,  56,
  61,  62,  59,  60,  29,  63,  64,  64,  64,  64,
  64,  65,  65,  41,   5,   6,   6,   7,   7,   7,
  66,  66,   8,   8,  67,   9,   9,   9,   9,  10,
  13,  13,  13,  11,  11,  11,  11,  11,  11,  11,
  11,  11,  12,  12,  14,  14,  15,  16,  17,  18,
  18,  18,  18,  18,  18,  18,  19,  23,  68,  69,
  69,  71,  71,  72,  72,  20,  73,  73,  70,  70,
  24,  74,  26,  26,  75,  77,  77,  78,  78,  78,
  27,  27,  27,  79,  81,  81,  81,  80,  80,  80,
  83,  83,  82,  82,  28,  84,  85,  30,  86,  87,
  88,  88,  90,  91,  91,  92,  92,  89,  89,  93,
  94,  94,  95,  76,  76,  32,  96,  36,  97,  98,
  99,  99, 100 };
short yyr2[]={

   0,   2,   1,   0,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   5,   1,   5,   1,   1,   1,   1,   1,   0,
   4,   1,   1,   1,   1,   0,   5,   1,   1,   1,
   1,   0,   4,   1,   2,   0,   1,   1,   1,   0,
   3,   2,   2,   8,   2,   1,   3,   1,   1,   1,
   1,   1,   2,   1,   1,   1,   3,   0,   1,   3,
   1,   2,   2,   2,   2,   0,   2,   2,   2,   2,
   0,   0,   8,   4,   5,   2,   1,   1,   1,   1,
   0,   1,   0,   1,   3,   1,   3,   3,   1,   3,
   1,   1,   2,   0,   1,   3,   2,   3,   1,   3,
   1,   1,   1,   1,   1,   3,   3,   3,   3,   2,
   4,   6,   7,   5,   1,   3,   1,   3,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   7,   1,   1,
   0,   3,   5,   1,   1,   1,   3,   5,   1,   1,
   5,   1,   2,   2,   1,   3,   3,   1,   3,   1,
   2,   1,   2,   1,   2,   2,   2,   1,   3,   1,
   1,   1,   1,   3,   4,   5,   1,   6,   1,   1,
   2,   0,   1,   1,   3,   1,   3,   2,   0,   1,
   1,   3,   3,   1,   3,   2,   1,   4,   1,   3,
   1,   1,   1 };
short yychk[]={

-1000,  -1, -21, -22, -23, -24, -25, -26, -27, -28,
 -29, -30, -31, -32, -33, -34, -35, -36, -37, 256,
 -39, -68, -74, -42, -75, -77, -79, -81, -84, -63,
 -86, -51, -96, -38, -44, -46, -97, -49, 257, 258,
 259, 260, 261, 262, -85, 270, 264, 265, 266, 267,
 268, 269, -50, 263, -21, -40, 278, 284, 285, 283,
 -20, 288, -20, -43, 277, 283, 276, -76, 273, 271,
 -20, -78, 290, 274, -80, -83, 274, 288, 289, -82,
 288, 295, -64, 283, 284, 277, 282, -20, -52, -57,
 274, 268, 260, 257, 267, -76, 282, -45, 278, 277,
 283, -47, -48, 279, 278, 285, -20,  -5, 295, 272,
 271, 273, 283, 272, 271, 273, -41, 288, -41, 295,
 295, -41, 294, 288, 288, 294, 294, 294, -76, -41,
 285, -53, 294, -58, 283, 282, 285, 288, -41,  -5,
 -41, 286,  -8, -67, 287,  -6,  -7, 288, -16, -17,
 288,  -5, -69, -71, -20, -73, -20,  -8, -20, 290,
 -83, 288, 297, -65, 281, -87, 288,  -2, 295, -57,
 -41, 281,  -5,  -8, -98, -99, -20,-100, 290,  -9,
 295, 307, -10, -11, -12, -18, -19, 308, 309, 310,
 290, 291, 292, 293, 289, 288, -16, 301, -17, 297,
 294, -66, 281, 275, 296, 281,  -8, 297, 294, -66,
 294, 297, -66,  -9, -88, -90, 283, -54, 285,  -3,
  -4, 288, 288,  -8,-100, 306,  -9, -11,  -9, -13,
 303, 302, 301, 305, 281, 304, -11, 295, 295, 295,
 295, 296,  -7, -11, 274, 288, 288, -70, 278, 276,
 -20, -72, -20, 289, -20, -20, -89, -93, 287, -91,
 -92, 288, -55, 280, 288, 274, 297, 294,-100,  -9,
 297, 297, -11, -11, -11, -11, -11, -11, -11, -11,
 289, -66, -66, -94, -95, 288, 294, 298, -56, -59,
 -61, 276, 288, 274,  -4, 297, 294, 275,  -8, -72,
 -20, 294, 281, -92, 288,  -8, -60, -62, 283, -60,
 -62, 290, -11, -14, -15, -11, 297, -95, 290, 288,
 298, 297,  -8, 294, 285, 290, 297, -15, 288, 285,
 290, 298, 290 };
short yydef[]={

   3,  -2,   2,   4,   5,   6,   7,   8,   9,  10,
  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
  29,   0,   0,  35,   0,   0, 161,   0,   0,  90,
   0,   0,   0,   0,  41,  -2,   0,   0,  24, 138,
 151,  31, 154, 163,   0,   0, 178, 196,  22,  37,
  43, 198,   0, 176,   1,   0,  25,  26,  27,  28,
   0, 145,   0,   0,  32,  33,  34, 152,   0,   0,
 193, 153, 157, 159, 160, 167, 169, 170, 171, 162,
 172,   0,   0,  86,  87,  88,  89,   0,   0,  55,
  57,  58,  59,  60,  61, 195,   0,   0,  38,  39,
  40,   0,   0,  46,  47,  48,   0, 103,   0, 164,
 165, 166,   0,  52,  54,  85,  51,  93,   0, 140,
   0, 103,   0, 155, 156,   0,   0,   0,   0,  92,
   0,  67,   0,   0,  63,  64,  65,   0,   0, 103,
  44,   0,  50,   0, 104,   0,  95, 128,  98,   0,
   0, 103,   0, 139,   0,   0,   0,  30, 194, 158,
 168, 173, 174,   0,  91, 181, 179,   0,   0,  56,
  62,   0, 103,  42, 197,   0, 200, 201, 202, 102,
   0,   0, 108,   0, 113, 114,   0,   0,   0,   0,
 129, 130, 131, 132, 133,  -2, 135, 136,   0,  94,
   0,   0, 100, 101,   0,   0,  23,   0,   0,   0,
   0, 150,   0,  84, 188,   0, 182,  75,   0,   0,
  68,  70,  21,  36,   0,   0,   0,   0, 106,   0,
   0,   0,   0, 110, 111, 112, 119,   0,   0,   0,
   0,   0,  96,  97,  99, 127, 175,   0, 148, 149,
   0, 141, 143, 144,   0, 146, 177,   0, 189, 180,
 183, 185,  80,   0,  71,  72,  66,   0, 199, 107,
 105, 118, 109, 115, 116, 117,   0,   0,   0, 103,
 137,   0,   0, 187, 190,   0,   0,   0, 103,  81,
  81,   0,  73,  74,  69, 120,   0,   0,   0, 142,
 147,   0,   0, 184, 186,  53,  76,  78,   0,  77,
  79,   0,   0, 103, 124, 126, 123, 191, 192,   0,
   0, 121,   0,   0,   0,   0, 122, 125,  83,   0,
   0,   0,  82 };
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
# line 156 "grammar.z"

		{
#			ifdef	xPTR1
			tTfp(38, 0, "*** [program stmnt] parsed.\n");
#			endif

			if (endquelst(Opflag) < 0)
				return (-1);
		} break;
case 2:
# line 165 "grammar.z"

		{
#			ifdef	xPTR1
			tTfp(38, 1, "*** [stmnt] parsed.\n");
#			endif

			if (endquelst(Opflag) < 0)
				return (-1);
		} break;
case 3:
# line 175 "grammar.z"
{
#			ifdef	xPTR1
			tTfp(38, 2, "*** [(NULL)] parsed.\n");
#			endif
		} break;
case 20:
# line 198 "grammar.z"
{
#			ifdef	xPTR1
			tTfp(38, 0, "*** [error] parsed.\n");
#			endif
		} break;
case 21:
# line 205 "grammar.z"

		{
			if ((i = openr(&Reldesc, -1, yypvt[-0].string_type)) < 0)
				syserr("relname: error in openr '%d'", i);
			if (i > 0)
			{
				/* invalid relation name */
				par_error(RNGEXIST, WARN, yypvt[-0].string_type, 0);
				YYERROR;
			}
			else
				rngent(R_EXTERNAL, yypvt[-2].string_type, &Reldesc);
		} break;
case 22:
# line 219 "grammar.z"

		{
			Opflag = mdRANGE;
		} break;
case 23:
# line 224 "grammar.z"

		{
			/* make root node */
			Lastree = tree(yypvt[-1].tree_type, yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 24:
# line 230 "grammar.z"

		{
			Opflag = mdAPP;
		} break;
case 30:
# line 241 "grammar.z"

		{
			/* make root node for delete, with a TIDNODE at leftmost */
			Lastree = tree(tree(NULL, Tidnode, RESDOM, sizeof(struct resdomnode), NULL), yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 31:
# line 247 "grammar.z"

		{
			Opflag = mdDEL;
		} break;
case 36:
# line 257 "grammar.z"

		{
			/* make root node for replace */
			Lastree = tree(yypvt[-1].tree_type, yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 37:
# line 263 "grammar.z"

		{
			Opflag = mdREPL;
		} break;
case 42:
# line 273 "grammar.z"

		{
			/* make root node for retrieve */
			Lastree = tree(yypvt[-1].tree_type, yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 43:
# line 279 "grammar.z"

		{
			Opflag = mdRETR;
		} break;
case 44:
# line 284 "grammar.z"

		{
			/* set up pipe block and save relname for create */
#			ifdef	xPTR2
			tTfp(38, 4, "retclause: Rsdmno %d", Rsdmno);
#			endif
			Rsdmno = 0;
			setp(PV_STR, "0");	/* relstat = nil */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.reldum.relid));
		} break;
case 45:
# line 294 "grammar.z"

		{
			/* no result relation, output to terminal */
			Rsdmno = 0;
			Resrng = -1;
		} break;
case 46:
# line 300 "grammar.z"

		{
			Opflag = mdRET_UNI;
			Rsdmno = 0;
			Resrng = -1;
		} break;
case 50:
# line 311 "grammar.z"

		{
			Lastree = tree(yypvt[-1].tree_type, yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 51:
# line 316 "grammar.z"

		{
			Rsdmno = 0;
			setp(PV_STR, "0040");	/* relstat = S_VIEW */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.reldum.relid));
		} break;
case 52:
# line 323 "grammar.z"

		{
			Opflag = mdVIEW;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			}
		} break;
case 53:
# line 333 "grammar.z"

		{
			Lastree = tree(yypvt[-4].tree_type, yypvt[-0].tree_type, ROOT, sizeof(struct rootnode), 1);
		} break;
case 54:
# line 338 "grammar.z"

		{
			Opflag = mdPROT;
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			}
		} break;
case 57:
# line 351 "grammar.z"

		{
			permcom(-1);	/* means 'all' commands */
		} break;
case 58:
# line 355 "grammar.z"

		{
			permcom(mdRETR);
		} break;
case 59:
# line 359 "grammar.z"

		{
			permcom(mdDEL);
		} break;
case 60:
# line 363 "grammar.z"

		{
			permcom(mdAPP);
		} break;
case 61:
# line 367 "grammar.z"

		{
			permcom(mdREPL);
		} break;
case 62:
# line 372 "grammar.z"

		{
			/* put command vector into list now since this always happens */
			setp(PV_INT, Permcomd);
			Permcomd = 0;		/* reset command map */
			setp(PV_STR, trim_relname(Parrng[Resrng].vardesc.reldum.relid));
			bmove(Parrng[Resrng].vardesc.reldum.relowner, permbuf, 2);
			permbuf[2] = 0;
			setp(PV_STR, permbuf);
		} break;
case 66:
# line 387 "grammar.z"

		{
			yyval.tree_type = yypvt[-1].tree_type;
		} break;
case 67:
# line 391 "grammar.z"

		{
			yyval.tree_type = NULL;
		} break;
case 69:
# line 397 "grammar.z"

		{
			/*
			** attach bulk of permit tl to leftmost node of new elem
			*/
			if (!Err_current)
				yyval.tree_type = tlprepend(yypvt[-2].tree_type, yypvt[-0].tree_type);
		} break;
case 70:
# line 406 "grammar.z"

		{
			/* Resrng is set by the "relation" production */
			if (!Err_current)
			{
				Trname = yypvt[-0].string_type;
				aptr = attlookup(Resrng, Trname);
				yyval.tree_type = tree(NULL, NULL, VAR, sizeof(struct varnode), Resrng, aptr);
				yyval.tree_type = addresdom(NULL, yyval.tree_type);
			}
		} break;
case 71:
# line 418 "grammar.z"

		{
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 72:
# line 422 "grammar.z"

		{
			setp(PV_STR, "all");
		} break;
case 73:
# line 427 "grammar.z"

		{
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 74:
# line 431 "grammar.z"

		{
			setp(PV_STR, "all");
		} break;
case 75:
# line 435 "grammar.z"

		{
			setp(PV_STR, "all");		/* default is all */
		} break;
case 80:
# line 445 "grammar.z"

		{
			setp(PV_INT, 0);
			setp(PV_INT, 1440);
		} break;
case 81:
# line 451 "grammar.z"

		{
			setp(PV_STR, "sun");
			setp(PV_STR, "sat");
		} break;
case 82:
# line 457 "grammar.z"

		{
			setp(PV_INT, timeofday(yypvt[-6].I2_type, yypvt[-4].I2_type));
			setp(PV_INT, timeofday(yypvt[-2].I2_type, yypvt[-0].I2_type));
		} break;
case 83:
# line 463 "grammar.z"

		{
			setp(PV_STR, yypvt[-2].string_type);
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 84:
# line 469 "grammar.z"

		{
			Lastree = tree(NULL, norml(yypvt[-0].tree_type), ROOT, sizeof(struct rootnode), 1);
			Qlflag--;	/* turn off here */
		} break;
case 85:
# line 475 "grammar.z"

		{
			Opflag = mdINTEG;
			Qlflag++;	/* OK to turn on here because integrity doesn't have a targ list */
			if (!Qrymod)
			{
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			}
		} break;
case 93:
# line 495 "grammar.z"

		{
#			ifdef	xPTR2
			tTfp(38, 3, "res rel name/var: '%s'\n", yypvt[-0].string_type);
#			endif
			switch (Opflag)
			{
			  case mdRETR:
			  case mdVIEW:
				/* result better not be a rel name */
				if ((i = openr(&Reldesc, -1, yypvt[-0].string_type)) < 0)
					syserr("relation: err openr '%d'", i);
				if (i == 0)
				{
					/* reln exists */
					if (bequal(Reldesc.reldum.relowner, Usercode, 2))
					{
						/* same owner, can't duplicate name */
						par_error(RESEXIST, WARN, yypvt[-0].string_type, 0);
						YYERROR;
					}
					else if (!Err_current)
					{
						/* owned by dba -- purge range table */
						rngdel(yypvt[-0].string_type);
					}
				}
				if (!Err_current)
				{
					bmove(Usercode, Reldesc.reldum.relowner, 2);
					pmove(yypvt[-0].string_type, Reldesc.reldum.relid, MAXNAME, ' ');
					Resrng = rngent(R_INTERNAL, "", &Reldesc);
				}
				break;

			  case mdAPP:
				/* result is a rel name */
				if (!Err_current)
				{
					Resrng = rnglook(yypvt[-0].string_type, LOOKREL);
					if (Resrng < 0)
					{
						if ((i = openr(&Reldesc, -1, yypvt[-0].string_type)) < 0)
							syserr("relation: err openr '%d'", i);
						if (i)
						{
							/* invalid relation name */
							par_error(RESAPPEX, WARN, yypvt[-0].string_type, 0);
							YYERROR;
						}
						Resrng = rngent(R_INTERNAL, "", &Reldesc);
					}
					else
						ctlmod_decl(Resrng);
					checkupd(Resrng);
				}
				break;

			  case mdPROT:
			  case mdINTEG:
#			  ifdef	DISTRIB
			  case mdDISTRIB:
#			  endif
				/* the result is a tuple variable */
				Resrng = rnglook(yypvt[-0].string_type, LOOKVAR);
				if (Resrng < 0)
				{
					/* variable not declared */
					par_error(NOVBLE, WARN, yypvt[-0].string_type, 0);
					YYERROR;
				}
				else
					ctlmod_decl(Resrng);
				break;

			  case mdREPL:
			  case mdDEL:
				/* the result is a tuple variable */
				Resrng = rnglook(yypvt[-0].string_type, LOOKVAR);
				if (Resrng < 0)
					/* variable not declared */
				{
					par_error(NOVBLE, WARN, yypvt[-0].string_type, 0);
					YYERROR;
				}
				else
					ctlmod_decl(Resrng);

				checkupd(Resrng);
				Tidnode = tree(NULL, NULL, VAR, sizeof(struct varnode), Resrng, &Faketid);
				break;
			}
		} break;
case 94:
# line 589 "grammar.z"

		{
			if (Patflag)
			{
				/* no patt match in targ list */
				par_error(NOPATMAT, WARN, 0);
			}
			yyval.tree_type = yypvt[-1].tree_type;

			/*
			** replace must have tid node as left branch
			**	(so does delete but it doesn't have a targ list)
			*/
			if (Opflag == mdREPL && !Err_current)
			{
				yyval.tree_type = tlprepend(tree(NULL, Tidnode, RESDOM, sizeof(struct resdomnode), 0), yyval.tree_type);
			}
		} break;
case 96:
# line 609 "grammar.z"

		{
			/*
			** attach bulk of targ list to leftmost node
			** of new element
			*/
			if (!Err_current)
				yyval.tree_type = tlprepend(yypvt[-2].tree_type, yypvt[-0].tree_type);
		} break;
case 97:
# line 619 "grammar.z"

		{
			Trname = yypvt[-2].string_type;
			/* make a new resdom entry for targ list */
			if (!Err_current)
				yyval.tree_type = addresdom(NULL, yypvt[-0].tree_type);
		} break;
case 98:
# line 626 "grammar.z"

		{
			/* makes a new resdom entry for targ list */
			if (!Err_current)
				yyval.tree_type = addresdom(NULL, yypvt[-0].tree_type);
		} break;
case 99:
# line 632 "grammar.z"

		{
			if (Opflag == mdREPL)
			{
				/* ALL not defined for REPLACE */
				par_error(REPALL, WARN,
				    trim_relname(Qt.qt_rangev[yypvt[-2].rng_type].rngvdesc->relvname), 0);
				YYERROR;
			}
			/* makes set of new resdom entries for targ list */
			else if (!Err_current)
				yyval.tree_type = xdot(yypvt[-2].rng_type);
		} break;
case 102:
# line 649 "grammar.z"

		{
			yyval.tree_type = norml(yypvt[-0].tree_type);
			Qlflag--;
		} break;
case 103:
# line 654 "grammar.z"

		{
			/* null qualification */
			yyval.tree_type = norml(NULL);
		} break;
case 104:
# line 660 "grammar.z"

		{
			Qlflag++;
		} break;
case 105:
# line 665 "grammar.z"

		{
			yyval.tree_type = yypvt[-1].tree_type;
		} break;
case 106:
# line 669 "grammar.z"

		{
			yyval.tree_type = tree(NULL, yypvt[-0].tree_type, UOP, 2, yypvt[-1].type_type);
		} break;
case 107:
# line 673 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-2].tree_type, yypvt[-0].tree_type, yypvt[-1].type_type, sizeof (struct rootnode) -2, 0);
		} break;
case 109:
# line 679 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-2].tree_type, yypvt[-0].tree_type, BOP, 2, yypvt[-1].type_type);
		} break;
case 115:
# line 690 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-2].tree_type, yypvt[-0].tree_type, BOP, 2, yypvt[-1].type_type);
		} break;
case 116:
# line 694 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-2].tree_type, yypvt[-0].tree_type, BOP, 2, yypvt[-1].type_type);
		} break;
case 117:
# line 698 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-2].tree_type, yypvt[-0].tree_type, BOP, 2, yypvt[-1].type_type);
		} break;
case 118:
# line 702 "grammar.z"

		{
			yyval.tree_type = yypvt[-1].tree_type;
		} break;
case 119:
# line 706 "grammar.z"

		{
			yyval.tree_type = tree(NULL, yypvt[-0].tree_type, UOP, 2, yypvt[-1].type_type);
		} break;
case 120:
# line 710 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-1].tree_type, NULL, UOP, 2, yypvt[-3].type_type);
		} break;
case 121:
# line 714 "grammar.z"

		{
			yyval.tree_type = tree(yypvt[-3].tree_type, yypvt[-1].tree_type, BOP, 2, yypvt[-5].type_type);
		} break;
case 122:
# line 719 "grammar.z"

		{
#			ifdef	xPTR2
			tTfp(39, 0, "agg func\n");
#			endif
			windup(yypvt[-2].tree_type);
			yyval.tree_type = tree(tree(yypvt[-2].tree_type, tree(NULL, yypvt[-4].tree_type, AOP, 6, yypvt[-6].type_type), BYHEAD, sizeof(struct resdomnode), 0), yypvt[-1].tree_type, AGHEAD, sizeof(struct rootnode), 0);
			tlprepend(tree(NULL, NULL, TREE, 0), yyval.tree_type);
		} break;
case 123:
# line 728 "grammar.z"

		{
			yyval.tree_type = tree(tree(NULL, yypvt[-2].tree_type, AOP, 6, yypvt[-4].type_type), yypvt[-1].tree_type,  AGHEAD, sizeof(struct rootnode), 0);
		} break;
case 125:
# line 734 "grammar.z"

		{
			yyval.tree_type = tlprepend(yypvt[-2].tree_type, yypvt[-0].tree_type);
		} break;
case 126:
# line 739 "grammar.z"

		{
			yyval.tree_type = tree(NULL, yypvt[-0].tree_type, RESDOM, sizeof(struct resdomnode), Rsdmno);
		} break;
case 127:
# line 744 "grammar.z"

		{
#			ifdef	xPTR2
			tTfp(39, 1, "attrib %12s.%12s found\n",
			Qt.qt_rangev[yypvt[-2].rng_type].rngvdesc->relvname, yypvt[-0].string_type);
#			endif

			/* remember attribute name */
			Trname = yypvt[-0].string_type;

			/* look up attribute */
			aptr = attlookup(yypvt[-2].rng_type, Trname);
			yyval.tree_type = tree(NULL, NULL, VAR, sizeof(struct varnode), yypvt[-2].rng_type, aptr);
		} break;
case 128:
# line 759 "grammar.z"

		{
			yyval.rng_type = rnglook(yypvt[-0].string_type, LOOKVAR);
			if (yyval.rng_type < 0)
			{
				/* variable not declared */
				par_error(NOVBLE, WARN, yypvt[-0].string_type, 0);
				YYERROR;
			}
			else
				ctlmod_decl(yyval.rng_type);
		} break;
case 129:
# line 772 "grammar.z"

		{
			yyval.tree_type = tree(NULL, NULL, INT, 2, yypvt[-0].I2_type);
		} break;
case 130:
# line 776 "grammar.z"

		{
			yyval.tree_type = tree(NULL, NULL, INT, 4, yypvt[-0].I4_type);
		} break;
case 131:
# line 780 "grammar.z"

		{
			yyval.tree_type = tree(NULL, NULL, FLOAT, 4, yypvt[-0].F4_type);
		} break;
case 132:
# line 784 "grammar.z"

		{
			yyval.tree_type = tree(NULL, NULL, FLOAT, 8, yypvt[-0].F8_type);
		} break;
case 133:
# line 788 "grammar.z"

		{
			if (patmat(yypvt[-0].string_type) && !Qlflag)
				Patflag = 1;
			yyval.tree_type = tree(NULL, NULL, CHAR, length(yypvt[-0].string_type), yypvt[-0].string_type);
		} break;
case 134:
# line 794 "grammar.z"

		{
			yyval.tree_type = tree(NULL, NULL, COP, 2, yypvt[-0].string_type);
		} break;
case 136:
# line 800 "grammar.z"

		{
			if (yypvt[-0].type_type == opADD)
				yyval.type_type = opPLUS;
			else
				if (yypvt[-0].type_type == opSUB)
					yyval.type_type = opMINUS;
		} break;
case 137:
# line 809 "grammar.z"

		{
#			ifdef	xPTR2
			tTfp(39, 3, "copy %12s,%12s\n", yypvt[-5].string_type, yypvt[-0].string_type);
#			endif

			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 138:
# line 818 "grammar.z"

		{
			Opflag = mdCOPY;
		} break;
case 144:
# line 830 "grammar.z"

		{
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 145:
# line 835 "grammar.z"

		{
			if (!Err_current)
			{
				setp(PV_STR, yypvt[-0].string_type);
				if (Opflag == mdDESTROY || Opflag == mdCREATE
#					ifdef	DISTRIB
					|| Opflag == mdDCREATE
#					endif
								)
					rngdel(yypvt[-0].string_type);
			}
		} break;
case 148:
# line 852 "grammar.z"

		{
			setp(PV_STR, "\0");
			setp(PV_STR, "i");
		} break;
case 149:
# line 857 "grammar.z"

		{
			setp(PV_STR, "\0");
			setp(PV_STR, "f");
		} break;
case 151:
# line 865 "grammar.z"

		{
			Opflag = mdCREATE;

			/* set up parameters for regular create */
			setp(PV_STR, "0");		/* relstat = nil */
		} break;
case 154:
# line 876 "grammar.z"

		{
			Opflag = mdDESTROY;
		} break;
case 155:
# line 881 "grammar.z"

		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			setp(PV_STR, "6");
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 156:
# line 890 "grammar.z"

		{
			Opflag = mdREMQM;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			setp(PV_STR, "5");
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 157:
# line 900 "grammar.z"

		{
			i = iocv(*(yypvt[-0].I2_type));
			setp(PV_STR, i);
		} break;
case 158:
# line 905 "grammar.z"

		{
			i = iocv(*(yypvt[-0].I2_type));
			setp(PV_STR, i);
		} break;
case 161:
# line 913 "grammar.z"

		{
			setp(PV_STR, "2");	/* all relns */
		} break;
case 163:
# line 919 "grammar.z"

		{
			Opflag = mdHELP;
		} break;
case 164:
# line 924 "grammar.z"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			smove("4", hqmbuf);
		} break;
case 165:
# line 932 "grammar.z"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			smove("5", hqmbuf);
		} break;
case 166:
# line 940 "grammar.z"

		{
			Opflag = mdDISPLAY;
			if (!Qrymod)
				/* no qrymod in database */
				par_error(NOQRYMOD, WARN, 0);
			smove("6", hqmbuf);
		} break;
case 169:
# line 951 "grammar.z"

		{
			setp(PV_STR, "3");
		} break;
case 170:
# line 956 "grammar.z"

		{
			/* relation */
			setp(PV_STR, "0");
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 171:
# line 962 "grammar.z"

		{
			/* manual page */
			setp(PV_STR, "1");
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 172:
# line 969 "grammar.z"

		{
			setp(PV_STR, hqmbuf);
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 173:
# line 974 "grammar.z"

		{
			setp(PV_STR, hqmbuf);
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 174:
# line 980 "grammar.z"

		{
			if (Rsdmno > MAXKEYS)
				/* too many attributes in key */
				par_error(INDEXTRA, WARN, 0);
		} break;
case 175:
# line 987 "grammar.z"

		{
			/* init INDEX command */
			Rsdmno = 0;
			setp(PV_STR, yypvt[-2].string_type);
			setp(PV_STR, yypvt[-0].string_type);
			Indexname = yypvt[-0].string_type;
		} break;
case 176:
# line 996 "grammar.z"

		{
			Opflag = mdINDEX;
		} break;
case 178:
# line 1003 "grammar.z"

		{
			Opflag = mdMODIFY;
			Rsdmno = 0;
		} break;
case 179:
# line 1009 "grammar.z"

		{
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 182:
# line 1016 "grammar.z"

		{
			setp(PV_STR, "name");
		} break;
case 185:
# line 1024 "grammar.z"

		{
			setp(PV_STR, yypvt[-0].string_type);
		} break;
case 186:
# line 1028 "grammar.z"

		{
			concat(yypvt[-2].string_type, ztack(":", yypvt[-0].string_type), modbuf);
			setp(PV_STR, modbuf);
		} break;
case 189:
# line 1037 "grammar.z"

		{
			setp(PV_STR, "\0");
		} break;
case 192:
# line 1045 "grammar.z"

		{
			setp(PV_STR, yypvt[-2].string_type);
			i = iocv(*(yypvt[-0].I2_type));
			setp(PV_STR, i);
		} break;
case 193:
# line 1052 "grammar.z"

		{
			Rsdmno++;
		} break;
case 194:
# line 1056 "grammar.z"

		{
			Rsdmno++;
		} break;
case 196:
# line 1063 "grammar.z"

		{
			Opflag = mdPRINT;
		} break;
case 198:
# line 1070 "grammar.z"

		{
			Opflag = mdSAVE;
		} break;
case 202:
# line 1080 "grammar.z"

		{
			i = iocv(*(yypvt[-0].I2_type));

#			ifdef	xPTR3
			tTfp(39, 4, "day_year: %s\n", i);
#			endif

			setp(PV_STR, i);
		} break;
		}
		goto yystack;  /* stack new state and value */

	}
