
# line 34 "grammar.y"
	/* STANDARD SCANNER & PARSER GLOBALS */

# include	"constants.h"
# include	<stdio.h>
# include	"globals.h"
# include	<sccs.h>

SCCSID(@(#)grammar.y	7.1	2/5/81)



# line 45 "grammar.y"
typedef union 
{
	struct disp_node	*u_dn;
} YYSTYPE;
# define APPEND 257
# define COPY 258
# define CREATE 259
# define DEFINE 260
# define DELETE 261
# define DESTROY 262
# define HELP 263
# define INDEX 264
# define MODIFY 265
# define PRINT 266
# define INTEGRITY 267
# define RANGE 268
# define REPLACE 269
# define RETRIEVE 270
# define SAVE 271
# define UNIQUE 272
# define PERMIT 273
# define VIEW 274
# define INGRES 275
# define EXIT 276
# define PARAM 277
# define TYPE 278
# define ALLOC 279
# define STRUCT 280
# define STRUCT_VAR 281
# define ALL 282
# define BY 283
# define FROM 284
# define IN 285
# define INTO 286
# define IS 287
# define OF 288
# define ON 289
# define ONTO 290
# define TO 291
# define WHERE 292
# define UNTIL 293
# define AT 294
# define NAME 295
# define SCONST 296
# define I2CONST 297
# define I4CONST 298
# define F8CONST 299
# define C_CODE 300
# define COMMA 301
# define LPAREN 302
# define RPAREN 303
# define PERIOD 304
# define QUOTE 305
# define BGNCMNT 306
# define ENDCMNT 307
# define LBRACE 308
# define RBRACE 309
# define LBRKT 310
# define RBRKT 311
# define NONREF 312
# define SEMICOL 313
# define POINTER 314
# define COLON 315
# define UOP 316
# define BOP 317
# define BDOP 318
# define EOP 319
# define LBOP 320
# define LUOP 321
# define FOP 322
# define FBOP 323
# define AOP 324
# define unaryop 325
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;

# line 112 "grammar.y"
	struct cvar	*cvarp;
# define YYERRCODE 256

# line 1226 "grammar.y"


# include	"tokens.y"
short yyexca[] ={
-1, 1,
	0, -1,
	257, 8,
	258, 8,
	259, 8,
	260, 8,
	261, 8,
	262, 8,
	263, 8,
	264, 8,
	265, 8,
	266, 8,
	268, 8,
	269, 8,
	270, 8,
	271, 8,
	275, 8,
	276, 8,
	277, 8,
	-2, 0,
-1, 61,
	302, 128,
	-2, 304,
-1, 188,
	272, 268,
	302, 268,
	-2, 265,
-1, 277,
	287, 176,
	304, 182,
	316, 176,
	317, 176,
	318, 176,
	319, 176,
	-2, 191,
-1, 353,
	304, 182,
	-2, 176,
	};
# define YYNPROD 307
# define YYLAST 767
short yyact[]={

 300, 431, 465, 408, 305, 222, 215, 214, 153, 326,
 380, 295, 339, 359, 234, 294, 272, 248, 319, 265,
 230, 111, 350, 361, 360, 421, 361, 360, 124, 272,
 276, 361, 432, 350, 194, 272, 181, 196, 384, 383,
 359, 343, 361, 360, 358, 357, 386, 426, 361, 360,
 110, 118, 194, 126, 138, 142, 216, 146, 194, 126,
 155, 117, 131, 167, 192, 169, 170, 178, 173, 361,
 360, 358, 357, 361, 360,  93, 293, 290, 291, 292,
 361, 360, 201,  93, 101, 211, 117, 154, 272, 397,
   7, 196, 113, 249, 117, 212, 296,  95,  11,  12,
  93, 281, 297, 298, 299,  95, 202, 194,  93, 293,
 290, 291, 292, 117, 117, 201, 329, 113, 328, 103,
 282, 217,  95,  16, 201, 113, 219,  93,  93, 296,
  95, 194, 101, 272, 126, 297, 298, 299, 274, 238,
 237, 129, 231, 117, 113, 263, 229, 130, 140,  95,
  95, 216, 194,  93, 260, 117, 134,  93, 241, 306,
 194, 239, 221, 205, 245, 435, 117, 134, 236,  93,
 141,  90, 247, 173, 113,  95, 216, 204, 165,  95,
  93, 134, 306, 166, 117, 231, 113, 250, 251, 129,
 231,  95, 218, 253, 254, 130, 117, 113,  93, 164,
 466, 267,  95, 117, 271, 237, 189, 264, 190, 258,
  93, 141, 268, 165, 271, 113, 223,  93, 166, 301,
  95, 138, 342, 309, 370, 310, 341, 113, 304, 117,
 134, 312,  95, 313, 113, 451, 140, 277, 117,  95,
 315, 307, 329,  93, 328, 117,  17, 323, 316,   4,
 317, 318,  93, 174, 164,  16, 324,  20, 334,  93,
 113, 330, 331,   6,  89,  95, 335, 272, 148, 113,
 182, 342, 327, 171,  95, 341, 113, 347, 123, 121,
 346,  95, 193, 122, 340,  16,  17,  20, 381, 135,
 269, 106, 343, 270, 109, 107, 108,  55, 208, 227,
 353, 353, 228, 225, 226, 129, 161,   7, 353, 351,
 354, 130, 140, 159, 158,  11,  12, 160, 112, 371,
 184, 185, 186,  80, 115, 220, 382, 372, 379,  92,
  91, 443, 187, 188, 114,  94,  13, 377, 162, 267,
 233, 389, 139, 378, 425, 307, 392, 382, 394, 390,
 268,  97, 385, 179, 391, 337, 156, 393, 387, 344,
 199, 353, 104,  99, 125, 348, 137, 128, 127, 126,
 183, 353, 404, 409, 289, 126, 176, 353, 353, 288,
 395, 287, 412, 102, 416, 353, 353, 353, 353, 307,
 415, 353, 382, 417, 411, 414, 363, 405, 419, 352,
 406, 307, 418, 413, 150, 286, 195, 362, 151, 375,
 356, 424, 436, 197, 430, 285, 152, 149, 136,  94,
 439,  15, 145, 144, 143, 175, 168, 191, 284, 134,
 409, 447, 448, 445, 197, 452,  98, 353, 283, 266,
  14, 396, 353, 453, 420, 353, 252, 353, 355, 280,
 207, 455, 256, 458,  96, 461, 349, 279, 464, 338,
 388, 469, 345, 470, 460, 210, 261, 468, 345, 198,
 471, 100, 307, 475, 474, 473, 398, 399, 353, 478,
 467, 477,  19, 479, 400, 401, 402, 403,  18,  10,
 472, 422, 307, 467, 476, 427, 367,  88, 307, 321,
 307, 200, 314, 437, 275, 332, 333, 203, 273, 139,
 308, 320,  92, 376, 442, 213, 243, 240,  94, 180,
  71, 206, 242, 209, 259, 177,  70, 336,  69, 193,
  68, 454, 255, 172,  67, 262, 438,  65, 325,  64,
  63, 440, 322, 246, 441, 462, 444,  62, 163,  61,
 257, 157,  60, 200,  59,  58, 450, 434, 200, 244,
  73,  74,  75,  80,  76,  77,  78,  79,  81,  82,
 235,  83,  84,  85,  86, 459, 449, 463,  87,  66,
  72,  92, 433, 410, 373, 232, 147,  94,  57, 446,
 311, 428, 407,  56, 369, 423, 224,  54, 133, 132,
 429,  53, 303, 302,  52, 120, 119,  51,  50,  49,
 105,  48, 278,  47, 206,  46, 209,  45,  44,  43,
  42,  41,  40,  39,  38,  37, 200,  36,  35,  34,
  33,  32,  31,  30,  29,  28, 456,  27,  26, 457,
  25,  24,  23,   8,   5,  22,  21,   3,   2,   1,
 116,   9,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 278, 278,   0,   0,   0,
   0,   0,   0,   0, 364, 365, 366,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 368,   0,   0,   0,
 374,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 278,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 278 };
short yypact[]={

-1000,   7,-1000, 303,-1000,-1000,-1000,-1000,-1000,-1000,
-142,-1000,-1000, -33, -23,-1000,-1000,-1000,-224,-176,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,   5,-195,
-195,  -6, -78,-126,-195,  38,-195, 147,-195,-195,
  28, -73,-195,   5,-195,-195,-1000, -43,  28,-108,
-241,-272,  63,-1000,-1000,-1000,-1000,-1000,-1000, -83,
-1000,-1000,-1000, -80,-1000,-1000,-1000,-1000, -18,-249,
-1000,-1000,-273,-1000,-212,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-178,-195,-1000,-1000,-1000,-1000,
-178,-1000,-273,-118,-273,-167,-219,-1000,-178,-116,
-195,-1000,-1000,-1000,-194,-195,-1000,-1000,-1000,-1000,
-1000,-1000,-194,-195,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000, -71,  14,-1000,-195,-106,-149,-1000,-1000,
-1000,-1000,-1000,-1000,-194, -71,-178,-195,-1000,-1000,
-1000,-178,-178,-195,-1000,-1000,-1000,-121,-209,-209,
-209,-1000, -43,-1000,-1000,-209,-209,-1000,-1000,-1000,
-1000,-1000,-1000, -38,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-209,-1000,-212,-1000,-1000,-1000,-1000,-155,-116,
-195,-1000,-1000, -36,-1000,-1000,-1000,-1000,-1000,-219,
-157,-1000,-1000,-195,-1000,-220,-1000,-1000,-195,-115,
 -85,-194,-195,-1000,-195,-1000,-1000,-1000,-1000,-178,
-195,-1000,-195, 147,-1000,-1000,-1000,-1000,-1000,-195,
-116,-1000,-116,-116,-1000,-1000,-195,-1000,-116,-1000,
-168,-1000,-1000,-116,-116,-210,-210,-195,-116,-1000,
-1000,-1000,-212,-1000,-1000,-215,-194, -12,-1000,-170,
 -42, -71,-1000,-1000,-1000,-170,-298,-1000,-220,-220,
-1000,-1000,-247,-1000,-1000,-1000,-187,-178,-178,-178,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-263,-1000,-1000,-194,-1000,-1000,-1000,-1000,-1000,-178,
 -63,-116, -84,-178,-1000,-1000,-1000,-1000,-1000,-215,
-194,-195,-1000,-138,-1000, -85,-1000,-1000,-1000,-1000,
-1000,-1000,-270,-271,-209,-1000,-267,-1000,-195,-187,
 -52,-1000,-1000,-1000, -42,-195, -85,-195,-1000,-220,
-1000,-287,-274,-1000,-1000,-187,-187,-1000,-1000,-1000,
-1000,-1000,-1000,-187,-187,-187,-187,-138,-195,-220,
-1000,-1000,-195,-101,-195,-1000,-1000, -61,-1000,-138,
-1000,-1000,-1000,-1000,-1000,-116,-1000,-1000,-290,-1000,
-1000, -85, -71,-1000,-1000,-1000,-1000,-1000,-293,-285,
-268,-268,-243,-236,-1000,-170,-298,-141,-1000,-283,
-129,-195,-1000,-170,-1000,-187,-1000,-1000,-1000,-195,
-187,-1000,-1000,-187,-215,-187,-1000,-1000,-1000,-195,
-195,-195,-1000, -49, -52,-1000,-1000,-1000,-290,-1000,
-290,-268,-1000,-141,-290,-1000,-194, -71,-1000, -84,
-138,-1000,-1000,-1000,-1000,-215,-187,-195, -97,-116,
-195,-283,-1000,-290, -71,-1000,-1000,-1000,-1000,-106,
-138, -97,-195,-106,-1000,-1000,-138,-283,-138,-1000 };
short yypgo[]={

   0, 651,  21, 318, 334, 324, 298, 650,   0, 649,
 648, 647, 646, 645, 249, 644, 643, 642, 641, 640,
 638, 637, 635, 634, 633, 632, 631, 630, 629, 628,
 627, 625, 624, 623, 622, 621, 620, 619, 618, 617,
 615, 613, 611, 362, 360,   7, 610, 609, 396, 290,
  89,   9,  10, 288, 608, 607, 606, 605, 604,  28,
 364, 603, 368, 367, 602,   8, 601, 599, 598, 289,
 192, 366, 597,   5, 297, 596, 594,  30, 593,  20,
  14, 592, 591,   3,   1,   6, 589,   2, 588, 586,
 585, 584, 583, 582, 576, 575, 268, 570, 557, 556,
   4, 555, 554, 552, 356, 551, 549, 306, 548, 547,
 543, 542, 540,  17, 539, 538, 537, 534, 273, 533,
 530, 528, 526, 525, 520, 519, 338, 516,  18, 511,
 499,  15,  12, 120, 353, 270, 497, 489, 264, 336,
 440, 421, 488, 363, 482, 471, 469, 466, 330, 406,
 465, 457, 456, 449, 448, 444,  19, 439,  11, 438,
 428, 415, 410, 405, 381, 379, 374, 344, 331, 284 };
short yyr1[]={

   0,   9,   9,  10,  10,  10,  10,  10,  11,  14,
  14,  14,  16,   1,  12,  12,  12,  12,  12,  12,
  12,  12,  12,  12,  12,  12,  12,  12,  12,  12,
  13,  13,  13,  13,  13,  13,  13,  13,  13,  17,
  43,  18,  18,  52,  52,  19,  20,  56,  21,  21,
  60,  60,  61,  61,  22,  22,  22,  22,  67,  67,
  69,  69,  68,  68,  23,  24,  25,  25,  81,  81,
  83,  83,  82,  82,  86,  86,  87,  87,  26,  88,
  89,  89,  96,  96,  96,  96,  96,  90,  90,  90,
  91,  91,  92,  92,  92,  93,  93,  93,  94,  94,
  95,  95,  27,  28,  29, 104,  30, 107,  31, 111,
  32,  33,  34, 115, 115,  35,  36,  37, 118, 118,
 119, 119,  38,  39,  40,  40, 122, 126, 126, 127,
 128, 128, 129, 130, 123, 123, 134, 125, 125, 135,
 124,  41,  15,  15, 137, 137, 137, 137, 137, 137,
 140, 139, 141, 141, 141, 142, 143, 146, 146, 147,
 147, 138, 138, 148,   2,   2,   2,   2,   4,   4,
   4,   7,   6,   6, 150, 150, 131,   3,   3,   3,
   5, 149,   8,  59,  59, 100, 100,  64,  64, 113,
  45,  45,  45,  77,  77,  77,  77,  77, 153, 153,
  49,  49,  53,  44, 156, 156, 157, 157, 133, 133,
 133, 133, 133, 133, 133, 133, 160, 159, 161, 161,
 161, 161, 161, 161, 168, 168, 158, 158, 152, 151,
 155, 154, 154, 154, 163, 164, 165, 162, 162, 167,
 166, 112,  42,  47, 114,  51,  51,  54, 116,  74,
  55,  58,  66,  72, 117,  62, 132, 132,  78,  63,
 101, 102, 103, 120, 106, 121, 109, 144, 136,  71,
  65,  46,  46,  46,  46,  46,  98,  84,  70,  57,
  57,  57,  57,  99,  75,  75,  75,  75,  73,  76,
  76, 145,  48,  97,  80, 169, 105, 105, 105, 105,
  50,  79, 108, 108, 108, 110,  85 };
short yyr2[]={

   0,   2,   0,   2,   2,   1,   1,   1,   0,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   4,
   2,   7,   6,   1,   1,   5,   3,   2,   2,   4,
   1,   1,   1,   1,   1,   2,   2,   3,   1,   3,
   1,   1,   1,   1,   7,   6,   7,   4,   1,   3,
   1,   3,   2,   0,   3,   5,   1,   1,  10,   2,
   1,   3,   1,   1,   1,   1,   1,   1,   1,   1,
   3,   0,   2,   1,   0,   2,   2,   0,   8,   0,
   4,   0,   2,   4,   4,   2,   4,   2,   4,   3,
   5,   4,   5,   1,   1,   3,   1,   2,   1,   2,
   1,   1,   4,   4,   2,   2,   4,   1,   0,   3,
   1,   3,   4,   0,   4,   1,   0,   4,   1,   0,
   4,   6,   3,   2,   1,   1,   2,   2,   1,   2,
   1,   1,   2,   1,   2,   2,   3,   2,   0,   3,
   1,   1,   3,   1,   1,   2,   2,   1,   2,   2,
   2,   1,   2,   2,   1,   1,   1,   1,   2,   2,
   1,   1,   1,   1,   3,   1,   1,   1,   3,   1,
   2,   2,   0,   3,   2,   3,   1,   0,   3,   5,
   3,   5,   1,   3,   1,   3,   3,   1,   1,   1,
   1,   3,   3,   2,   4,   6,   5,   7,   1,   1,
   1,   1,   1,   1,   3,   1,   3,   3,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   2,   1,   1,   2,   1,   1,   1,   2,   1,
   1,   1,   1,   2,   1,   1,   1,   1,   1,   1,
   1,   2,   1,   2,   1,   2,   1,   1,   2,   1,
   1,   1,   1,   1,   1,   0,   1,   1,   1,   1,
   1,   1,   0,   1,   1,   1,   1,   1,   1,   1,
   0,   1,   1,   1,   1,   1,   1,   1,   1,   0,
   1,   1,   1,   1,   0,   1,   1 };
short yychk[]={

-1000,  -9, -10, -11, -14, -15, 256, 300, -16,  -1,
-137, 308, 309,-139,-140,-141, 278, 279,-142,-144,
 280, -12, -13, -17, -18, -19, -20, -21, -22, -23,
 -24, -25, -26, -27, -28, -29, -30, -31, -32, -33,
 -34, -35, -36, -37, -38, -39, -40, -41, -42, -47,
 -54, -55, -58, -66, -72, -74, -78, -88,-101,-102,
-103,-106,-109,-112,-114,-116, 276,-117,-120,-121,
-122,-124, 277, 257, 258, 259, 261, 262, 263, 264,
 260, 265, 266, 268, 269, 270, 271, 275,-136,-138,
 313,-148,  -3, 295,  -5, 317,-140,-139,-141,-143,
-145, 308,-143, 295, -43, -46, 286, 290, 291, 289,
  -8,  -2,  -3, 312,  -4,  -5,  -7, 281,  -8, -56,
 -57, 285, 289, 284, -59, -60,  -8, -62, -63, 267,
 273, -65, -67, -68, 282, -69, -60, -71,  -8, -53,
 274, 296,  -8, -62, -63, -71,  -8, -89, -96, 270,
 257, 261, 269, -65, -59,  -8,-104,-105, 286, 285,
 289,-107,-126,-108, 272, 286, 291,  -8, -43,  -8,
  -8,-118,-119,  -8, 296,-104,-107,-123, 308,-134,
-125, 308,-135, -74, 257, 258, 259, 269, 270, 289,
 288,-126, 313, -70, 301,-149, 310,  -3,-146, -44,
 -48, 302,  -8, -48, 295, 281,-149,  -4,  -6,-149,
-150, 304, 314, -48, -45, -85, 292,  -8, -70,  -8,
 -70, -59, -73, 287, -75, 289, 290, 285, 288,  -8,
 -79, 291, -90, -70, -80, -97, -79, 289, 288, -73,
 -44,  -8, -44,-127, -48,  -8,-110, 293,-113, 302,
-113,-113,-118,-113,-113,-134,-135, -71,-113,-148,
 309,-147,-139, 300, -45,-156,-157,  -8,-158, -49,
 -50,  -8, 303,  -6, 295, -49, -77,  -2, -48,-151,
-153, 321,-133,-159,-160,-161,-163,-164,-165,-166,
 297, 298, 299, 296,-131,-158, 316, 322, 323, 324,
  -8,  -8, -61, -64, -65,-100, 297,  -2, -69,  -8,
  -8, -44,  -8,  -8, -96,  -8, -45, -45, -45,-128,
-129,-130,-111,  -8, -45,-115, -51,  -2, 286, 284,
 -45, -45, -14, -14,  -8, -45,-138, -50, -70,-132,
-169, 287, 283, 304, -50, -70, -51, -73, -50,-152,
 320, -77,-133,  -2, -77,-154,-162, 319, 318, 287,
 317, 316,-133, -48, -48, -48, -48, -70, -48, -76,
 287, -45, -80, -91, -48, -50, -70,-131,  -2,-100,
 -52, -53,  -8, 309, 309,-113, 313,-156,-133,  -8,
 -65, -51,  -8, -52,  -8, -77, -50, -50,-133,-133,
-133,-133,-133,-133,-100, -59, -77, -81, -83,  -8,
 -92, -79, -65, -59,-128,-132,-100, -45, -52, -73,
-155, 318, -50, -70, -45,-167, 283, -50, -82, -70,
 -85, -84, 315, -93, -98, 294,  -8, -50,-133,  -8,
-133,-133, -50,-168,-133, -83, -86,  -8,  -8, -94,
 -99, 284,  -8, -65, -50, -45, -70, -70, -73, -95,
 -80,-100, -50,-133,  -8, -87, 297,  -2, -45,  -8,
 -84, -73, -79,-100, -87,  -8, -79,-100, -84,-100 };
short yydef[]={

   2,  -2,   1,   0,   5,   6,   7,   9,  10,  11,
   0,  12,  13, 144, 145, 148, 151, 150, 153,   0,
 267,   3,   4,  14,  15,  16,  17,  18,  19,  20,
  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
  31,  32,  33,  34,  35,  36,  37,  38, 275,   0,
   0, 282,   0,  54,   0,   0,   0,   0,   0,   0,
 299,  -2,   0, 275,   0,   0, 116,   0, 299, 304,
 136, 139,   0, 242, 243, 247, 250, 251, 252,   0,
 249, 258, 260,   0, 262, 264, 266, 254, 128,   0,
 143, 161, 163, 177,   0, 180, 146, 147, 149, 152,
 158, 291, 154, 155,   0,   0, 271, 272, 273, 274,
   0, 182, 164,   0, 167,   0,   0, 171,   0, 192,
   0, 279, 280, 281,  48,   0, 183,  50,  51, 255,
 259,  55,  56,   0, 270,  58,  62,  63,  60,  61,
 269, 202,   0,   0,  79,   0,   0,   0,  80,  82,
  83,  84,  85,  86, 102,   0,   0,   0, 296, 297,
 298,   0,   0,   0, 127, 302, 303,   0,   0,   0,
   0, 117, 118, 120, 121,   0,   0, 124, 136, 135,
 125, 139, 138,   0, 241, 244, 248, 263,  -2, 253,
 261,   0, 142,   0, 278, 179, 181, 178,   0, 192,
   0, 292,  40,   0, 165, 166, 169, 168, 170,   0,
   0, 174, 175,   0,  46, 197, 306,  47,   0,   0,
   0,  57,   0, 288,   0, 284, 285, 286, 287,   0,
   0, 301,   0,   0,  87,  88,  89, 294, 293,   0,
 192, 105, 192, 192, 133, 107,   0, 305, 192, 189,
   0, 115, 119, 192, 192,   0,   0,   0, 192, 162,
 156, 157,   0, 160,  39,   0, 204,   0, 207,   0,
   0,   0, 300, 172, 173,   0, 190,  -2, 197, 197,
 196, 229,   0, 208, 209, 210,   0,   0,   0,   0,
 218, 219, 220, 221, 222, 223, 234, 235, 236, 240,
   0, 184,  49,  52,  53, 187, 185, 186,  59,   0,
 290, 192,  67,  91,  81, 103, 104, 106, 126,   0,
 130,   0, 108,   0, 111,   0, 113, 114, 245, 246,
 122, 123,   0,   0,   0, 140,   0, 203,   0,   0,
   0, 256, 257, 295,   0,   0,   0,   0,  45, 197,
 228,   0,   0,  -2, 194,   0,   0, 231, 232, 233,
 237, 238, 213,   0,   0,   0,   0,   0,   0, 197,
 289, 110,   0,  94,   0, 129, 133,   0, 176,   0,
 112,  43,  44, 134, 137, 192, 159, 205, 206, 226,
 227,   0,   0,  42, 200, 195, 193, 212, 198, 211,
   0,   0,   0, 192, 188,   0,  65,  73,  68,  70,
  97,   0,  93,   0, 131,   0, 109, 141,  41,   0,
   0, 230, 214,   0,   0,   0, 239,  64,  66,   0,
   0,   0, 277,  99,   0, 276,  92,  90, 132, 201,
 199,   0, 216, 192, 225,  69,  72,   0,  71, 101,
   0, 283,  95,  96, 215,   0,   0,   0,   0, 192,
   0,   0, 217, 224,   0,  74,  76,  77,  78,   0,
   0,   0,   0,   0,  75, 100,   0,   0,   0,  98 };
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
# line 119 "grammar.y"
 
		{
			/* for each "statement", free the symbol space
			 * used by that query (lookahed space must not
			 * be freed), and catch up on output lines
			 */
			symspfree();
			equate_lines();
		} break;
case 3:
# line 130 "grammar.y"
 
		{
			w_sync();
			/* the purpose of the actions for startquel
			 * and this action is to make each query
			 * a single compound C statement (if (x) "{query}")
			 */
			w_op("}");
		} break;
case 4:
# line 139 "grammar.y"

		{
			end_quote();
			w_op("}");
		} break;
case 8:
# line 148 "grammar.y"

			w_op("{"); break;
case 10:
# line 158 "grammar.y"

			Block_level += 1; break;
case 11:
# line 160 "grammar.y"

		{
			if (Block_level == 0)
				yyserror("extra '}'", yypvt[-0].u_dn);
			else if ((Block_level -= 1) == 0)
			{
				freecvar(&C_locals);
				freecvar(&F_locals);
			}
		} break;
case 12:
# line 175 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 13:
# line 178 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 76:
# line 284 "grammar.y"

			w_con(I2CONST, yypvt[-0].u_dn->d_elm); break;
case 77:
# line 286 "grammar.y"

		{
			if (yypvt[-0].u_dn)
			{
				if (!Cvarp)
					w_key(yypvt[-0].u_dn->d_elm);
				else if (Fieldp && Fieldp->c_type == opINT
					|| Cvarp->c_type == opINT)
						w_var(Cv_display, opINT);
				else if (Fieldp && Fieldp->c_type == opSTRING 
					|| Cvarp->c_type == opSTRING)
						w_var(Cv_display, opIDSTRING);
				else
					yyserror("in MODIFY, qual var must be in or string",
					yypvt[-0].u_dn);
			}
			else
				yyserror("bad modify qualification", 0);
			free_display(Cv_display);
			Cvarp = Fieldp = 0;
		} break;
case 82:
# line 317 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 83:
# line 319 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 84:
# line 321 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 85:
# line 323 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 114:
# line 384 "grammar.y"

		{
			if (yypvt[-0].u_dn && Cvarp)
			{
				if (Fieldp && Fieldp->c_type != opSTRING
				   || !Fieldp && Cvarp->c_type != opSTRING)
					yyserror("string var expected for from/into in COPY",
					yypvt[-0].u_dn);
				else
					w_var(Cv_display, opIDSTRING);
			}
			else
				yyserror("into/from expected in COPY", yypvt[-0].u_dn);
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
		} break;
case 116:
# line 405 "grammar.y"
 
		{ 
			Opflag = mdEXIT;
			w_new("IIexit();"); 
		} break;
case 117:
# line 412 "grammar.y"
 
			w_op(");"); break;
case 118:
# line 415 "grammar.y"

			w_op("0"); break;
case 120:
# line 419 "grammar.y"

			w_op(","); break;
case 121:
# line 421 "grammar.y"

		{
			w_string(yypvt[-0].u_dn->d_elm, 0);
			w_op(",");
		} break;
case 124:
# line 434 "grammar.y"

			w_flush(); break;
case 125:
# line 436 "grammar.y"

			w_flush(); break;
case 126:
# line 439 "grammar.y"

		{
			w_new("IIsetup();");
		} break;
case 127:
# line 444 "grammar.y"

		{
			Opflag = mdTUPRET;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 128:
# line 449 "grammar.y"

			Opflag = mdTUPRET; break;
case 133:
# line 459 "grammar.y"

			Opflag = mdCTLELM; break;
case 134:
# line 462 "grammar.y"

			w_op("}"); break;
case 135:
# line 464 "grammar.y"

			w_op("}"); break;
case 136:
# line 467 "grammar.y"

		{
			w_new("while(IIn_get(");
			w_file();
			w_op(")){");
			w_ret();
			free_ret();
			w_op("if(IIerrtest())continue;");
			equate_lines();
		} break;
case 137:
# line 478 "grammar.y"

			w_op("}"); break;
case 138:
# line 480 "grammar.y"

			w_op("}"); break;
case 139:
# line 483 "grammar.y"

		{
			w_new("while(IIgettup(");
			w_file();
			w_op(")){");
			equate_lines();
		} break;
case 140:
# line 491 "grammar.y"

		{
			w_new("IIsetup();");
		} break;
case 142:
# line 504 "grammar.y"

		{
			w_op(yypvt[-0].u_dn->d_elm);
			Type_spec = 0;
		} break;
case 143:
# line 509 "grammar.y"
 
		{ 
			w_op(yypvt[-0].u_dn->d_elm);
			Type_spec = 0;
		} break;
case 148:
# line 519 "grammar.y"

		{
			Struct_flag = 0;
			Type_spec = opSTRUCT;
		} break;
case 149:
# line 524 "grammar.y"

		{
			Struct_flag = 0;
			Type_spec = opSTRUCT;
		} break;
case 150:
# line 530 "grammar.y"

		{
			Opflag = mdDECL;
			w_key(yypvt[-0].u_dn->d_elm);
			/* in case the default "int" should be assumed,
			 * the Type_spec is set up for it, if a previous
			 * type hasn't been given
			 */
			if (!Type_spec)
				Type_spec = opINT;
		} break;
case 151:
# line 542 "grammar.y"

		{
			Opflag = mdDECL;
			w_key(yypvt[-0].u_dn->d_elm);
			Type_spec = Opcode;
		} break;
case 155:
# line 553 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 156:
# line 556 "grammar.y"

		{
			w_op(yypvt[-0].u_dn->d_elm);
			Type_spec = 0;
		} break;
case 159:
# line 565 "grammar.y"

		{
			w_op(yypvt[-0].u_dn->d_elm);
			Type_spec = 0;
		} break;
case 163:
# line 575 "grammar.y"

		{
			if (Type_spec == opSTRING)
				Indir_level -= 1;
			if (Struct_flag)
				decl_field(yypvt[-0].u_dn->d_elm, Type_spec,
					Indir_level, Block_level);
			else
				decl_cvar(yypvt[-0].u_dn->d_elm, Type_spec,
					Indir_level, Block_level);
			free_display(Cv_display);
			Indir_level = Field_indir = 0;
			Fieldp = Cvarp = 0;
		} break;
case 164:
# line 590 "grammar.y"

		{
			yyval.u_dn = yypvt[-0].u_dn;
			if (Cvarp && Cvarp->c_indir != Indir_level)
			{
				yyserror("bad indirection on a C variable", yypvt[-0].u_dn);
				yyval.u_dn = 0;
			}
			Indir_level = Field_indir = 0;
		} break;
case 165:
# line 600 "grammar.y"

		{
			enter_display(Cv_display, salloc(yypvt[-1].u_dn->d_elm));
			Cvarp = Fieldp = 0;
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 166:
# line 606 "grammar.y"

		{
			enter_display(Cv_display, salloc(yypvt[-1].u_dn->d_elm));
			Cvarp = Fieldp = 0;
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 167:
# line 612 "grammar.y"

		{
			if (!Fieldp)
			{
				yyserror("undeclared field", yypvt[-0].u_dn);
				yyval.u_dn = yypvt[-1].u_dn;
			}
			else if (Fieldp->c_indir != Field_indir)
			{
				yyserror("bad indirection on a structure's field",
				yypvt[-0].u_dn);
				yyval.u_dn = 0;
			}
			if (Cvarp->c_indir != Indir_level)
			{
				yysemerr("bad indirection a structure variable",
				Cvarp->c_indir);
				yyval.u_dn = 0;
			}
			Indir_level = Field_indir = 0;
		} break;
case 168:
# line 634 "grammar.y"

		{
			if (yypvt[-1].u_dn->d_elm[1] == '*')
				Field_indir += 1;
			Field_indir += 1;
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 169:
# line 641 "grammar.y"

			Field_indir += 1; break;
case 171:
# line 645 "grammar.y"

		{
			Cvarp = getcvar(yypvt[-0].u_dn->d_elm);
			enter_display(Cv_display, yypvt[-0].u_dn->d_elm);
		} break;
case 172:
# line 651 "grammar.y"

		{
			Indir_level += 1;
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 173:
# line 656 "grammar.y"

		{
			enter_display(Cv_display, yypvt[-0].u_dn->d_elm);
			Fieldp = getfield(yypvt[-0].u_dn->d_elm);
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 174:
# line 663 "grammar.y"

			enter_display(Cv_display, yypvt[-0].u_dn->d_elm); break;
case 175:
# line 665 "grammar.y"

		{
			enter_display(Cv_display, yypvt[-0].u_dn->d_elm);
			Indir_level += 1;
		} break;
case 176:
# line 673 "grammar.y"
	
		{
			if (yypvt[-0].u_dn)
			{
				if (!Fieldp && ! Cvarp)
				{
					if (!Field_indir && !Indir_level
					  && (sequal(yypvt[-0].u_dn->d_elm, "dba")
					    || sequal(yypvt[-0].u_dn->d_elm, "usercode")))
						/* constant operator COP */
						w_key(yypvt[-0].u_dn->d_elm);
					else
						yyserror("C var expected", yypvt[-0].u_dn);
				}
				else if (Opflag == mdCTLELM)
				{
					w_con(NAME,
					  Fieldp ? Fieldp->c_id: Cvarp->c_id);
					enter_ret(Cv_display,
					  Fieldp ? Fieldp->c_type: Cvarp->c_type);
				}
				else
					w_var(Cv_display,
					  Fieldp ? Fieldp->c_type: Cvarp->c_type);
			}
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
			Indir_level = Field_indir = 0;
		} break;
case 177:
# line 702 "grammar.y"

		{
			if (Opflag == mdDECL)
				w_con(NAME, yypvt[-0].u_dn->d_elm);
			else
			{
				Cvarp = getcvar(yypvt[-0].u_dn->d_elm);
				enter_display(Cv_display, salloc(yypvt[-0].u_dn->d_elm));
			}
		} break;
case 178:
# line 712 "grammar.y"

		{
			if (yypvt[-1].u_dn->d_elm [1] == '*')
				Indir_level += 1;
			Indir_level += 1;
			yyval.u_dn = yypvt[-0].u_dn;
		} break;
case 179:
# line 719 "grammar.y"

		{
			Indir_level += 1;
		} break;
case 180:
# line 724 "grammar.y"

		{
			if (!sequal(yypvt[-0].u_dn->d_elm, "*") && !sequal((struct disp_node *)(yypvt[-0].u_dn)->d_elm, "**"))
				yyserror(Opflag == mdDECL ?
				"invalid operator in declaration":
				"invalid operator in C variable",
				yypvt[-0].u_dn);
			if (Opflag == mdDECL)
				w_op(yypvt[-0].u_dn->d_elm);
			else
				enter_display(Cv_display, salloc(yypvt[-0].u_dn->d_elm));
		} break;
case 181:
# line 737 "grammar.y"

		{
			if (Opflag == mdDECL)
				eat_display(0, '[', ']');
			else
				eat_display(Cv_display, '[', ']');
		} break;
case 182:
# line 750 "grammar.y"

		{
			if (yypvt[-0].u_dn)
			{
				if (Cvarp)
				{
					if (Fieldp && Fieldp->c_type != opSTRING
					   || !Fieldp && Cvarp->c_type != opSTRING)
						yyserror("string var expected", yypvt[-0].u_dn);
					else if (Opflag == mdFILENAME)
						w_var(Cv_display, opSTRING);
					else if (Opflag == mdINGRES)
						w_display(Cv_display);
					else
						w_var(Cv_display, opIDSTRING);
				}
				else if (Opflag == mdINGRES)
					w_string(yypvt[-0].u_dn->d_elm, 0);
				else if (Opflag == mdFILENAME)
					yyserror("file for a COPY must be a string or string variable",
					yypvt[-0].u_dn);
				else
					w_key(yypvt[-0].u_dn->d_elm);
			}
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
		} break;
case 185:
# line 783 "grammar.y"

			w_con(I2CONST, yypvt[-0].u_dn->d_elm); break;
case 186:
# line 785 "grammar.y"

		{
			if (yypvt[-0].u_dn)
			{
				if (Cvarp)
					if (Fieldp && Fieldp->c_type == opINT
					   || Cvarp->c_type == opINT)
						w_var(Cv_display, opINT);
					else
						yyserror("integer variable required",
						yypvt[-0].u_dn);
				else
					yyserror("integer variable required", yypvt[-0].u_dn);
			}
			free_display(Cv_display);
		} break;
case 189:
# line 807 "grammar.y"

		{
			w_op("(");
			end_quote();
			if (Opflag == mdTUPRET)
				w_key("IIw_left");
			else
				w_key("IIw_right");
			eat_display(0, '(', ')');
			w_op(";");
			begin_quote();
			w_op(")");

		} break;
case 191:
# line 824 "grammar.y"

		{
			if (!yypvt[-0].u_dn || !Cvarp)
				yyserror("C var (string) expected", yypvt[-0].u_dn);
			else if (Fieldp && Fieldp->c_type == opSTRING
				|| Cvarp->c_type == opSTRING)
			{
				end_quote();
				w_op("IIwrite(");
				w_display(Cv_display);
				w_op(");");
			}
			else
				yyserror("var must be string valued for qualification",
				yypvt[-0].u_dn);
			free_display(Cv_display);
			Cvarp = Fieldp = 0;
		} break;
case 202:
# line 857 "grammar.y"
 
			w_con(SCONST, yypvt[-0].u_dn->d_elm); break;
case 218:
# line 887 "grammar.y"
 
			w_con(I2CONST, yypvt[-0].u_dn->d_elm); break;
case 219:
# line 889 "grammar.y"
 
			w_con(I4CONST, yypvt[-0].u_dn->d_elm); break;
case 220:
# line 891 "grammar.y"
 
			w_con(F8CONST, yypvt[-0].u_dn->d_elm); break;
case 221:
# line 893 "grammar.y"
 
			w_con(SCONST, yypvt[-0].u_dn->d_elm); break;
case 227:
# line 902 "grammar.y"

		{
			if (Opflag != mdVIEW && Opflag != mdRETRIEVE
			   && Opflag != mdAPPEND)
				yyserror(
				"'all' applied to this range variable illegal in this kind of statement",
				yypvt[-2].u_dn);
		} break;
case 228:
# line 911 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 229:
# line 914 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 230:
# line 917 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 231:
# line 920 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 232:
# line 922 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 233:
# line 924 "grammar.y"
 
			w_op("="); break;
case 234:
# line 927 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 235:
# line 930 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 236:
# line 933 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 237:
# line 936 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 238:
# line 938 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 239:
# line 941 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 240:
# line 944 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 241:
# line 952 "grammar.y"

		{
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
			Opflag = mdAPPEND;
		} break;
case 242:
# line 959 "grammar.y"

		{
			Opflag = mdAPPEND;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 243:
# line 966 "grammar.y"

		{
			Opflag = mdCOPY;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 244:
# line 973 "grammar.y"

		{
			Opflag = mdCOPY;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 245:
# line 980 "grammar.y"
 
		{
			w_key(yypvt[-0].u_dn->d_elm);
			Opflag = mdFILENAME;
		} break;
case 246:
# line 985 "grammar.y"
 
		{
			w_key(yypvt[-0].u_dn->d_elm);
			Opflag = mdFILENAME;
		} break;
case 247:
# line 991 "grammar.y"

		{
			Opflag = mdCREATE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 248:
# line 998 "grammar.y"

		{
			Opflag = mdCREATE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 249:
# line 1005 "grammar.y"

		{
			Opflag = mdDEFINE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 250:
# line 1012 "grammar.y"

		{
			Opflag = mdDELETE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 251:
# line 1019 "grammar.y"

		{
			Opflag = mdDESTROY;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 252:
# line 1026 "grammar.y"

		{
			Opflag = mdHELP;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 253:
# line 1033 "grammar.y"

		{
			Opflag = mdINDEX;
			begin_quote();
			w_key(yypvt[-1].u_dn->d_elm);
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 254:
# line 1041 "grammar.y"

		{
			Opflag = mdINGRES;
			w_new("IIingres(");
		} break;
case 255:
# line 1047 "grammar.y"

		{
			if (Opflag == mdDEFINE)
				Opflag = mdINTEGRITY;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 256:
# line 1054 "grammar.y"

		{ 
			if (Opflag == mdCTLELM)
				Opflag = mdTUPRET;
			w_op("=");
		} break;
case 257:
# line 1060 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 258:
# line 1063 "grammar.y"

		{
			Opflag = mdMODIFY;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 259:
# line 1070 "grammar.y"

		{
			if (Opflag == mdDEFINE)
				Opflag = mdINTEGRITY;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 260:
# line 1077 "grammar.y"
  
		{ 
			Opflag = mdPRINT;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 261:
# line 1084 "grammar.y"
 
		{
			Opflag = mdRANGE;
			begin_quote();
			w_key(yypvt[-1].u_dn->d_elm);
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 262:
# line 1092 "grammar.y"

		{
			Opflag = mdREPLACE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 263:
# line 1099 "grammar.y"

		{
			begin_quote();
			Opflag = mdREPLACE;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 264:
# line 1106 "grammar.y"

		{
			Opflag = mdRETRIEVE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 265:
# line 1113 "grammar.y"

		{
			Opflag = mdRETRIEVE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 266:
# line 1120 "grammar.y"

		{
			Opflag = mdSAVE;
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 267:
# line 1127 "grammar.y"

		{
			Opflag = mdDECL;
			Struct_flag = 1;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 268:
# line 1134 "grammar.y"

		{
			begin_quote();
			w_key(yypvt[-0].u_dn->d_elm);
			Opflag = mdTUPRET;
		} break;
case 269:
# line 1141 "grammar.y"

		{
			if (Opflag == mdDEFINE)
				Opflag = mdVIEW;
			w_key(yypvt[-0].u_dn->d_elm);
		} break;
case 270:
# line 1153 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 276:
# line 1162 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 277:
# line 1165 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 278:
# line 1168 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 283:
# line 1176 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 288:
# line 1184 "grammar.y"
 
			w_op("="); break;
case 291:
# line 1190 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 292:
# line 1193 "grammar.y"
 
			w_op(yypvt[-0].u_dn->d_elm); break;
case 293:
# line 1196 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 294:
# line 1199 "grammar.y"

			w_key(yypvt[-0].u_dn->d_elm); break;
case 295:
# line 1202 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 300:
# line 1210 "grammar.y"

			w_op(yypvt[-0].u_dn->d_elm); break;
case 301:
# line 1213 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 305:
# line 1220 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
case 306:
# line 1223 "grammar.y"
 
			w_key(yypvt[-0].u_dn->d_elm); break;
		}
		goto yystack;  /* stack new state and value */

	}
