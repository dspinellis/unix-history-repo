# define SEOS 1
# define SCOMMENT 2
# define SLABEL 3
# define SUNKNOWN 4
# define SHOLLERITH 5
# define SICON 6
# define SRCON 7
# define SDCON 8
# define SBITCON 9
# define SOCTCON 10
# define SHEXCON 11
# define STRUE 12
# define SFALSE 13
# define SNAME 14
# define SNAMEEQ 15
# define SFIELD 16
# define SSCALE 17
# define SINCLUDE 18
# define SLET 19
# define SASSIGN 20
# define SAUTOMATIC 21
# define SBACKSPACE 22
# define SBLOCK 23
# define SCALL 24
# define SCHARACTER 25
# define SCLOSE 26
# define SCOMMON 27
# define SCOMPLEX 28
# define SCONTINUE 29
# define SDATA 30
# define SDCOMPLEX 31
# define SDIMENSION 32
# define SDO 33
# define SDOUBLE 34
# define SELSE 35
# define SELSEIF 36
# define SEND 37
# define SENDFILE 38
# define SENDIF 39
# define SENTRY 40
# define SEQUIV 41
# define SEXTERNAL 42
# define SFORMAT 43
# define SFUNCTION 44
# define SGOTO 45
# define SASGOTO 46
# define SCOMPGOTO 47
# define SARITHIF 48
# define SLOGIF 49
# define SIMPLICIT 50
# define SINQUIRE 51
# define SINTEGER 52
# define SINTRINSIC 53
# define SLOGICAL 54
# define SOPEN 55
# define SPARAM 56
# define SPAUSE 57
# define SPRINT 58
# define SPROGRAM 59
# define SPUNCH 60
# define SREAD 61
# define SREAL 62
# define SRETURN 63
# define SREWIND 64
# define SSAVE 65
# define SSTATIC 66
# define SSTOP 67
# define SSUBROUTINE 68
# define STHEN 69
# define STO 70
# define SUNDEFINED 71
# define SWRITE 72
# define SLPAR 73
# define SRPAR 74
# define SEQUALS 75
# define SCOLON 76
# define SCOMMA 77
# define SCURRENCY 78
# define SPLUS 79
# define SMINUS 80
# define SSTAR 81
# define SSLASH 82
# define SPOWER 83
# define SCONCAT 84
# define SAND 85
# define SOR 86
# define SNEQV 87
# define SEQV 88
# define SNOT 89
# define SEQ 90
# define SLT 91
# define SGT 92
# define SLE 93
# define SGE 94
# define SNE 95

# line 97 "gram.in"
#	include "defs"

#ifdef SDB
#	include <a.out.h>
char *stabline();
#	ifdef UCBVAXASM
		char *stabdline();
#	endif
#endif

static int nstars;
static int ndim;
static int vartype;
static ftnint varleng;
static struct { ptr lb, ub; } dims[MAXDIM+1];
static struct Labelblock *labarray[MAXLABLIST];
static int lastwasbranch = NO;
static int thiswasbranch = NO;
extern ftnint yystno;

ftnint convci();
double convcd();
struct Addrblock *nextdata(), *mkbitcon();
struct Constblock *mklogcon(), *mkaddcon(), *mkrealcon();
struct Constblock *mkstrcon(), *mkcxcon();
struct Listblock *mklist();
struct Listblock *mklist();
struct Impldoblock *mkiodo();
struct Extsym *comblock();

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
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 20,
	1, 31,
	-2, 205,
-1, 24,
	1, 35,
	-2, 205,
-1, 144,
	1, 219,
	-2, 170,
-1, 162,
	1, 238,
	77, 238,
	-2, 170,
-1, 219,
	76, 156,
	-2, 123,
-1, 233,
	73, 205,
	-2, 202,
-1, 258,
	1, 257,
	-2, 127,
-1, 262,
	1, 266,
	77, 266,
	-2, 129,
-1, 324,
	76, 157,
	-2, 125,
-1, 335,
	1, 240,
	14, 240,
	73, 240,
	77, 240,
	-2, 171,
-1, 386,
	90, 0,
	91, 0,
	92, 0,
	93, 0,
	94, 0,
	95, 0,
	-2, 137,
-1, 410,
	1, 260,
	77, 260,
	-2, 127,
-1, 412,
	1, 262,
	77, 262,
	-2, 127,
-1, 414,
	1, 264,
	77, 264,
	-2, 127,
-1, 461,
	77, 260,
	-2, 127,
	};
# define YYNPROD 271
# define YYLAST 1253
short yyact[]={

 209, 334, 228, 432, 431, 430, 333, 424, 375, 423,
 259, 253, 374, 240, 294, 280, 265, 260, 224, 277,
 182, 190, 178, 296, 110, 189,   5,  17, 115,  96,
 172, 200, 251, 193, 257, 114, 188, 113, 302, 186,
 429, 247, 122, 171, 112, 428, 100, 292, 100,  98,
 102, 300, 301, 302, 107, 152, 153, 154, 155, 245,
 246, 247, 150, 151, 100, 124, 125, 126, 127, 468,
 129, 287, 261, 149, 466, 149, 286, 156, 157, 300,
 301, 302, 308, 307, 306, 305, 304, 123, 309, 311,
 310, 313, 312, 314, 206, 446, 492, 156, 157, 300,
 301, 302, 308, 307, 306, 305, 304, 208, 309, 311,
 310, 313, 312, 314, 114, 279, 113,  91, 199, 152,
 153, 154, 155, 227, 174, 175, 150, 151, 100, 156,
 157, 426,  92,  93,  94, 478, 173,  95, 403, 210,
 230, 232, 100, 477, 473, 211, 149, 181, 407, 244,
 149, 489, 100, 156, 157, 245, 246, 247, 248, 444,
 149,  97, 445, 217, 220, 216, 148, 215, 346, 212,
  97, 149, 418, 156, 157, 245, 246, 247, 248, 417,
 147, 416, 147, 347, 266, 267, 268, 227, 222, 421,
 143, 380, 422, 156, 157, 226, 203, 271, 272, 283,
 264, 255, 409, 210, 299, 252, 274, 273, 225, 229,
 229, 282, 275, 176, 404, 493, 397, 403, 288, 279,
 394, 291, 322, 290, 377, 372, 299, 378, 330, 367,
 299, 361, 368, 320, 362, 336, 328, 244, 337, 329,
 149, 353, 352, 351, 256, 149, 149, 149, 149, 149,
 244, 244, 196, 147, 205, 165, 108, 147, 106, 262,
 262, 105, 299, 332, 158, 160, 164, 147, 104, 103,
 244, 101, 395, 379, 355, 298, 218, 316, 147,   4,
 483, 356, 318, 319, 482, 348, 357, 358, 349, 350,
 360, 321, 324, 219, 327, 380, 359, 481, 474, 299,
 316, 480, 475, 366, 331, 470, 396, 392, 454, 371,
 402, 281, 236, 179, 289, 339, 249, 299, 234, 299,
 299, 231, 299, 363, 238, 299, 250, 221, 299, 219,
 187, 202, 299, 198, 159, 135, 316, 269, 149, 244,
 299, 100, 244, 244, 244, 244, 244, 147, 400, 100,
 326, 405, 147, 147, 147, 147, 147, 451, 262, 408,
 335, 376, 411, 413, 415, 156, 157, 245, 246, 247,
 248, 433, 382, 383, 384, 385, 386, 387, 388, 389,
 390, 391, 420, 299, 299, 299, 299, 299, 299, 299,
 299, 299, 299, 447, 371, 201, 442, 144, 453, 162,
 255, 456, 225, 419, 131, 458, 338, 244, 192, 457,
 100, 341, 342, 343, 344, 345, 197,  89,  29, 258,
 258, 408,   6,  99, 242, 237, 299, 411, 413, 415,
 433, 111, 465, 460, 459, 467,  78, 469, 425,  77,
 462, 463, 464,  76,  75, 147, 262, 262, 262, 299,
 117, 299, 448, 450, 161, 472, 299, 476, 471, 317,
 156, 157, 300, 301, 302, 434,  74,  73, 229, 433,
 455,  72, 487, 486, 484,  57,  50, 223, 437, 490,
 299,  48, 317,  46,  45,  42, 299, 449,  31, 299,
 303, 325, 491, 323, 494, 425,  99,  99,  99,  99,
 195, 177, 373, 194, 406, 365, 180, 207, 183, 184,
 185, 262, 262, 262, 364, 435, 370, 369, 354, 156,
 157, 245, 246, 247, 434, 128, 284, 229,  52, 183,
 213, 214, 479,  35, 293, 109,  25, 437,  24, 485,
  23, 437,  22,  21,  20, 233, 488, 235,  19, 276,
 130,  88,   9, 156, 157, 300, 301, 302, 308, 307,
 306,   8, 229, 434, 309, 311, 310, 313, 312, 314,
 152, 153, 154, 155,   7,   3, 437, 150, 151, 100,
 146,  99, 146,   2, 278,   1,   0,   0,   0,   0,
 152, 153, 154, 155,   0,   0,   0, 150, 151, 100,
   0, 111,   0, 295, 297,   0, 410, 412, 414,   0,
   0,   0,   0, 401,   0, 191,   0, 183, 156, 157,
 300, 301, 302, 308, 307, 306, 305, 304,   0, 309,
 311, 310, 313, 312, 314,   0, 191,   0, 227,   0,
   0,   0,   0,   0, 156, 157, 226,   0,   0, 452,
   0,   0,   0, 146, 210,   0,   0, 146, 227,   0,
   0,   0,   0,   0, 156, 157, 340, 146, 254,   0,
   0, 461, 412, 414, 210,   0,   0,   0, 146,   0,
   0,   0,   0, 152, 153, 154, 155, 183,   0,   0,
 150, 151, 100, 317,   0, 285, 399,   0,   0,   0,
 191, 156, 157, 300, 301, 302, 308, 307, 306, 305,
 304,   0, 309, 311, 310, 313, 312, 314,   0,   0,
   0, 156, 157, 300, 301, 302, 308, 307,   0,   0,
   0, 398, 309, 311, 310, 313, 312, 314, 156, 157,
 245, 246, 247, 248,   0,   0,   0, 146,   0,   0,
   0, 239, 146, 146, 146, 146, 146, 156, 157, 270,
 254,   0,   0, 254, 254,   0,   0, 278, 156, 157,
 300, 301, 302, 308,   0, 436,   0, 443,   0,   0,
 393,   0,   0, 295,   0, 156, 157, 300, 301, 302,
 308, 307, 306, 305, 304, 191, 309, 311, 310, 313,
 312, 314,   0,   0, 156, 157, 300, 301, 302, 308,
   0,   0,   0,   0, 443, 309, 311, 310, 313, 312,
 314, 443, 443, 443, 152, 153, 154, 155,   0,   0,
   0, 150, 151, 100, 436,   0,   0,   0, 436, 152,
 153, 154, 155,   0,   0, 146, 150, 151, 100, 243,
   0, 381,   0,   0,   0, 254, 156, 157, 300, 301,
 302, 308, 307, 306, 305, 304,   0, 309, 311, 310,
 313, 312, 314, 436,   0,   0, 427,   0,   0,   0,
 191, 152, 153, 154, 155,   0,   0,   0, 150, 151,
 100,   0, 227,   0,   0,   0,   0,   0, 156, 157,
 315,   0,   0,   0,  12,   0,   0, 239, 210,   0,
   0,   0,   0, 156, 157, 241,   0, 254,  10,  53,
  43,  70,  82,  14,  58,  67,  87,  36,  63,  44,
  40,  65,  69,  30,  64,  33,  32,  11,  84,  34,
  18,  39,  37,  27,  16,  54,  55,  56,  47,  51,
  41,  85,  61,  38,  66,  86,  28,  59,  81,  13,
  90,  79,  62,  49,  83,  26,  71,  60,  15,   0,
   0,  68,  80,   0,   0, 152, 153, 154, 155,   0,
   0,   0, 150, 151, 100,   0,   0,   0,   0,   0,
   0, 116,   0, 119, 120, 121,   0,   0,   0,   0,
   0,   0,   0,   0, 132, 133,   0,   0, 134,   0,
 136, 137, 138,   0,   0, 139, 140, 141,   0, 142,
 152, 153, 154, 155,   0,   0,   0, 150, 151, 100,
   0,   0,   0,   0,   0,   0,   0,   0, 166, 167,
 168, 169, 170, 227, 152, 153, 154, 155,   0, 156,
 157, 150, 151, 100,   0,   0,   0,   0,   0, 210,
   0, 152, 153, 154, 155, 441, 440, 439, 150, 151,
 100,   0,   0,   0,   0,   0, 152, 153, 154, 155,
   0,   0,   0, 150, 151, 100,   0,   0, 263,   0,
   0,   0,   0,   0, 156, 157,   0,   0,   0,   0,
   0,   0,   0,   0, 210,   0,   0, 152, 153, 154,
 155,   0, 204,   0, 150, 151, 100,   0, 156, 157,
  53,  43,   0,  82,   0,  58,   0,  87, 210, 438,
  44,   0,   0,   0,   0, 156, 157,   0,   0,  84,
   0,   0,   0,   0, 239,   0,  54,  55,  56,  47,
 156, 157,  85,   0,   0,   0,  86,   0,  59,  81,
   0,   0,  79,   0,  49,  83,   0,   0,  60,   0,
 118,   0,   0,  80,   0, 145,   0, 152, 153, 154,
 155, 156, 157, 163, 150, 151, 100,  70,   0,   0,
   0,  67,   0,   0,  63,   0,   0,  65,  69,   0,
  64,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,  61,   0,
  66,   0,   0,   0,   0,   0,   0,   0,  62,   0,
   0,   0,  71,   0,   0,   0,   0,  68,   0,   0,
   0,   0,   0,   0,   0, 145,   0,   0,   0,   0,
   0, 156, 157 };
short yypact[]={

-1000,  23, 421, 900,-1000,-1000,-1000,-1000,-1000,-1000,
 412,-1000,-1000,-1000,-1000,-1000,-1000,  93, 396, 194,
 192, 191, 184, 181,  84, 179,  32,-1000,-1000,-1000,
-1000,1101,-1000,-1000,-1000,   6,-1000,-1000,-1000,-1000,
-1000,-1000, 396,-1000,-1000,-1000,-1000,-1000, 262,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,1172, 261,1102, 261, 178,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000, 396, 396, 396, 396,-1000, 396,-1000, 240,-1000,
-1000, 396, -47, 396, 396, 396, 257, 335,-1000, 175,
-1000,-1000,-1000,-1000, 402, 260, 389,-1000,-1000, 258,
-1000,-1000,-1000,1039,  32, 396, 396, 257, 335,-1000,
 201, 256, 389,-1000, 254, 114, 970, 970, 248, 389,
 396, 245, 396,-1000,-1000, 834,-1000,-1000, 659,1071,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 834,
 128, 167,-1000,-1000,1015,1015,-1000,-1000,-1000,-1000,
 678,-1000,-1000,-1000, 240, 240, 396,-1000,-1000, 138,
 238,  84,-1000, 238,-1000,-1000,-1000, 396,-1000,  -6,
-1000,-1000, 335,-1000, 241,1166,  32, -35, 396, 396,
-1000,-1000, 970,  18, 819,-1000,-1000,-1000,-1000, 970,
 970,-1000, 396,-1000,-1000,-1000,-1000,-1000, 970, 970,
 280, 970,-1000, 162,-1000,  18, 389, 970,-1000,  18,
-1000, 970,-1000,  84, 389,-1000, 286, 161,-1000,1071,
-1000,-1000, 585,-1000,1071,1071,1071,1071,1071, -42,
  94, 106, 327,-1000,-1000, 327, 327,-1000, 166, 165,
 164,  18,-1000,1015,-1000,-1000,-1000,-1000,-1000, 659,
-1000,-1000,-1000, 240, 238,-1000, 157,-1000,-1000,-1000,
   6,-1000, 396,-1000, 155,-1000,-1000, 335, 148, 347,
-1000,-1000,-1000, 150,-1000, 198,-1000, 116, 777, 970,
 970, 970, 970, 970, 970, 970, 970, 970, 970,-1000,
-1000,-1000,-1000,-1000,-1000, 233, 706, 143, -45, 725,
-1000,  18, 196, 232,  18, 139, 396, 622,-1000, 565,
-1000, 539, 237, 140,-1000,-1000,-1000, 834,  74,  18,
-1000, -22, -42, -42, -42, 440,-1000, 327, 106, 125,
 106,1015,1015,1015, 104, 102,  95,-1000,-1000,-1000,
   6,-1000,  34,-1000, 115,  50,-1000,-1000, 396, -37,
1056,-1000, 335,  85,-1000,  15,-1000,-1000, 396, 970,
 970, 288, -30, -45, -45, -45, 689, 474, 474, 642,
 725, 381,-1000,-1000, 970, 970, 235, 970,-1000, 389,
-1000,-1000, 389, 389,  84,-1000, 659,-1000,-1000, 327,
-1000,-1000,-1000,-1000,-1000,-1000,1015,1015,1015,-1000,
-1000,-1000,  50,-1000,-1000,  -2,-1000,-1000,-1000,1056,
-1000,-1000, -12, 876,-1000,-1000,-1000,-1000, 970,-1000,
-1000,-1000, 231, 220,-1000, 347, 347,-1000,  18,  67,
  18,-1000, 224, 228, 970,  18,  66,  61,-1000, 970,
 227, 224, 223, 210, 206,-1000,  50,-1000,1056,-1000,
-1000,-1000,-1000, 970,-1000,-1000,  75, 389,-1000,  18,
-1000,-1000,-1000,-1000,-1000,  18,-1000,-1000,  18, 970,
  19, 141, 389,-1000,-1000 };
short yypgo[]={

   0, 585, 583, 575, 574, 561, 552, 551, 960, 117,
  43,  30,  22,  27, 404, 549,  19, 548, 544, 543,
 542, 540, 538, 536, 535,  28, 534,  29,  15,  42,
 533, 528,  72,  20,  44,  39, 526, 507, 525,  36,
  25, 517, 516,   5,   4,   3,   0,  94, 515,  24,
  14,  21,  23, 514, 505,   9,   7,   6,   1,  31,
  33, 503, 502, 500,  12,   8, 493, 491, 254, 107,
 490,   2, 166, 324, 418, 488, 487, 485, 484, 483,
 481, 477, 476,  18, 475, 471, 190, 467, 466, 454,
  32, 444,  34, 443, 439,  16, 436, 425,  13, 424,
  11,  10,  17 };
short yyr1[]={

   0,   1,   1,   2,   2,   2,   2,   2,   2,   2,
   3,   4,   4,   4,   4,   4,   4,   9,  11,  14,
  10,  10,  12,  12,  12,  15,  15,  16,  16,   7,
   5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
   5,  17,  17,  13,  30,  31,  31,  31,  31,  31,
  31,  31,  31,  31,  31,  31,  29,  29,  29,  18,
  18,  18,  18,  34,  34,  19,  19,  20,  20,  21,
  21,  35,  36,  36,  22,  22,  38,  39,  42,  41,
  41,  43,  43,  44,  44,  44,  44,  24,  24,  49,
  49,  26,  26,  50,  33,  51,  51,  40,  40,  28,
  28,  54,  53,  53,  55,  55,  56,  56,  57,  57,
  58,  59,  23,  23,  60,  63,  61,  62,  62,  64,
  64,  65,  25,  66,  66,  67,  67,  32,  32,  32,
  68,  68,  68,  68,  68,  68,  68,  68,  68,  68,
  68,  68,  68,  68,  46,  46,  70,  70,  70,  70,
  70,  70,  37,  37,  37,  37,  71,  71,  45,  45,
  69,  69,  69,  69,  69,  69,  47,  48,  48,  48,
  72,  72,  73,  73,  73,  73,  73,  73,  73,  73,
   6,   6,   6,   6,   6,   6,   6,  75,  52,  74,
  74,  74,  74,  74,  74,  74,  74,  74,  74,  74,
  77,  78,  78,  78,  78,  27,  27,  80,  81,  81,
  83,  83,  82,  82,  76,  76,   8,  79,  84,  84,
  84,  84,  84,  84,  84,  84,  84,  84,  84,  84,
  85,  94,  94,  94,  87,  96,  96,  96,  89,  89,
  86,  86,  97,  97,  98,  98,  98,  98,  99,  88,
  91,  93,  93,  90,  90, 100, 100,  92,  92,  92,
 102, 102, 102, 102, 102, 102, 101, 101, 101, 101,
  95 };
short yyr2[]={

   0,   0,   3,   2,   2,   2,   3,   3,   2,   1,
   1,   3,   3,   4,   4,   5,   3,   0,   1,   1,
   0,   1,   0,   2,   3,   1,   3,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   2,   1,
   5,   6,   5,   2,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   0,   2,   4,   3,
   4,   5,   3,   1,   3,   3,   3,   3,   3,   3,
   3,   3,   1,   3,   3,   3,   0,   4,   0,   2,
   3,   1,   3,   1,   2,   1,   1,   1,   3,   1,
   1,   1,   3,   3,   2,   1,   5,   1,   3,   0,
   3,   0,   2,   3,   1,   3,   1,   1,   1,   3,
   1,   1,   3,   3,   4,   0,   2,   1,   3,   1,
   3,   1,   0,   0,   1,   1,   3,   1,   3,   1,
   1,   1,   3,   3,   3,   3,   2,   3,   3,   3,
   3,   3,   2,   3,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   6,   4,   9,   0,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   5,   1,   1,   1,
   1,   3,   1,   1,   3,   3,   3,   3,   2,   3,
   1,   4,   2,   2,   6,   2,   2,   5,   3,   4,
   5,   2,   1,   1,  10,   1,   3,   4,   3,   3,
   1,   3,   3,   7,   7,   0,   1,   3,   1,   3,
   1,   2,   1,   1,   1,   3,   0,   1,   2,   2,
   2,   2,   2,   3,   4,   4,   2,   3,   1,   3,
   3,   1,   1,   1,   3,   1,   1,   1,   1,   1,
   3,   3,   1,   3,   1,   1,   2,   2,   1,   3,
   3,   4,   4,   1,   3,   1,   5,   1,   1,   1,
   3,   3,   3,   3,   3,   3,   1,   5,   5,   5,
   0 };
short yychk[]={

-1000,  -1,  -2,  -3, 256,   3,   1,  -4,  -5,  -6,
  18,  37,   4,  59,  23,  68,  44, -13,  40, -17,
 -18, -19, -20, -21, -22, -23,  65,  43,  56, -74,
  33, -75,  36,  35,  39, -30,  27,  42,  53,  41,
  30,  50, -77,  20,  29, -78, -79,  48, -80,  63,
 -82,  49, -31,  19,  45,  46,  47, -84,  24,  57,
  67,  52,  62,  28,  34,  31,  54,  25,  71,  32,
  21,  66, -85, -87, -88, -91, -93, -94, -96,  61,
  72,  58,  22,  64,  38,  51,  55,  26,  -7,   5,
  -8,  -9,  -9,  -9,  -9,  44, -27,  77, -11, -14,
  14,  77, -27,  77,  77,  77,  77, -27,  77, -24,
 -49, -14, -34,  84,  82, -25,  -8, -74,  69,  -8,
  -8,  -8, -29,  81, -25, -25, -25, -25, -38, -25,
 -37, -14,  -8,  -8,  -8,  73,  -8,  -8,  -8,  -8,
  -8,  -8,  -8, -86, -73,  73, -37, -69, -72, -46,
  12,  13,   5,   6,   7,   8,  79,  80, -86,  73,
 -86, -89, -73,  81, -86,  77,  -8,  -8,  -8,  -8,
  -8, -10, -11, -10, -11, -11,  -9, -14, -12,  73,
 -14, -34, -33, -14, -14, -14, -35,  73, -39, -40,
 -51, -37,  73, -60, -61, -63,  77,  14,  73, -58,
 -59,   6,  73, -32,  73, -68, -47, -37, -69, -46,
  89, -33, -34, -14, -14, -35, -39, -60,  75,  73,
 -59,  73,  74, -81, -83, -32,  81,  73, -71, -32,
 -71,  73, -58, -14,  73, -14, -72, -97, -73,  73,
 -98,  81, -99,  15, -46,  81,  82,  83,  84, -72,
 -72, -90,  77,-100, -37,  73,  77, -92, -68,-101,
-102, -32, -47,  73, -92, -95, -95, -95, -95, -72,
  81, -12, -12, -11, -25,  74, -15, -16, -14,  81,
 -28,  73, -27, -28, -36, -37,  82,  77, -40,  73,
 -13, -49,  82, -26, -50, -14, -52, -14, -32, -46,
  81,  82,  83, -70,  88,  87,  86,  85,  84,  90,
  92,  91,  94,  93,  95,  81, -32, -68, -32, -32,
 -33, -32, -71, -66, -32, -67,  70, -32,  74,  77,
 -58, -32, -27, -57, -58,  74,  74,  77, -72, -32,
  81, -72, -72, -72, -72, -72,  74,  77, -90, -90,
 -90,  77,  77,  77, -68,-101,-102, -95, -95, -12,
 -28,  74,  77, -29, -53, -54, -33,  74,  77, -41,
 -42, -51,  77, -62, -64, -65,  14,  74,  77,  75,
  75,  74, -32, -32, -32, -32, -32, -32, -32, -32,
 -32, -32,  74,  74,  77,  76,  74,  77, -14,  74,
 -83,  74,  73,  77,  74, -98, -72,  74,-100,  77,
 -68,-101, -68,-101, -68,-101,  77,  77,  77, -29,
 -16,  74,  77, -55, -56, -32,  81, -37,  82,  77,
 -43, -44, -45, -46, -47, -48, -14, -69,  73,  11,
  10,   9, -52, -14,  74,  77,  80, -50, -32, -76,
 -32,  69, -68, -71,  73, -32, -58, -57, -58, -27,
 -52, -68, -52, -52, -52, -55,  76, -43,  81, -45,
  74, -64, -65,  77,  74,  74, -71,  77,  74, -32,
  74,  74,  74,  74, -56, -32, -44, -45, -32,  76,
 -58, -71,  77,  74, -58 };
short yydef[]={

   1,  -2,   0,   0,   9,  10,   2,   3,   4,   5,
   0, 216,   8,  17,  17,  17,  17, 205,   0,  30,
  -2,  32,  33,  34,  -2,  36,  37,  39, 122, 180,
 216,   0, 216, 216, 216,  56, 122, 122, 122, 122,
  76, 122,   0, 216, 216, 192, 193, 216, 195, 216,
 216, 216,  44, 200, 216, 216, 216, 217, 216, 212,
 213,  45,  46,  47,  48,  49,  50,  51,  52,  53,
  54,  55,   0,   0,   0,   0, 228, 216, 216, 216,
 216, 216, 231, 232, 233, 235, 236, 237,   6,  29,
   7,  20,  20,   0,   0,  17,   0, 206,  22,  18,
  19,   0,   0, 206,   0,   0,   0,   0, 115,  38,
  87,  89,  90,  63,   0,   0,   0, 182, 183,   0,
 185, 186,  43,   0,   0,   0,   0,   0,   0, 115,
   0, 152,   0, 191,   0,   0, 156, 156,   0,   0,
   0,   0,   0, 218,  -2,   0, 172, 173,   0,   0,
 160, 161, 162, 163, 164, 165, 144, 145, 220,   0,
 221, 222,  -2, 239, 226,   0, 270, 270, 270, 270,
   0,  11,  21,  12,  22,  22,   0, 122,  16,   0,
  99, 205,  62,  99,  66,  68,  70,   0,  75,   0,
  97,  95,   0, 113,   0,   0,   0,   0,   0,   0,
 110, 111,   0,  57,   0, 127, 129, 130, 131,   0,
   0,  59,   0,  65,  67,  69,  74, 112,   0,  -2,
   0,   0, 196,   0, 208, 210,   0,   0, 198, 157,
 199,   0, 201,  -2,   0, 207, 244,   0, 170,   0,
 242, 245,   0, 248,   0,   0,   0,   0,   0, 178,
 244, 223,   0, 253, 255,   0,   0, 227,  -2, 258,
 259,   0,  -2,   0, 229, 230, 234, 249, 250, 270,
 270,  13,  14,  22,  99,  23,   0,  25,  27,  28,
  56, 101,   0,  94,   0,  72,  78,   0,   0,   0,
 116,  88,  64,   0,  91,   0, 181,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 146,
 147, 148, 149, 150, 151,   0,   0, 127, 136, 142,
  60, 189,   0,   0,  -2, 124,   0,   0, 197,   0,
 211,   0,   0,   0, 108,  -2, 241,   0,   0, 246,
 247, 174, 175, 176, 177, 179, 240,   0, 225,   0,
 224,   0,   0,   0, 127,   0,   0, 251, 252,  15,
  56,  24,   0,  42,   0,   0,  61,  71,   0,   0,
   0,  98,   0,   0, 117, 119, 121,  40,   0,   0,
   0,   0, 132, 133, 134, 135,  -2, 138, 139, 140,
 141, 143,  58, 128,   0, 156, 154,   0, 190,   0,
 209, 187,   0,   0, 205, 243, 244, 171, 254,   0,
  -2, 261,  -2, 263,  -2, 265,   0,   0,   0,  41,
  26, 100,   0, 102, 104, 107, 106,  73,  77,   0,
  79,  81,  83,   0,  85,  86, 158, 159,   0, 167,
 168, 169,   0, 152, 114,   0,   0,  92,  93, 188,
 214, 184, 127,   0, 156, 126,   0,   0, 109,   0,
   0,  -2,   0,   0,   0, 103,   0,  80,   0,  84,
  96, 118, 120,   0, 166, 153,   0,   0, 203, 204,
 256, 267, 268, 269, 105, 107,  82,  83, 215, 156,
   0,   0,   0, 155, 194 };
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
			
case 3:
# line 151 "gram.in"
{ lastwasbranch = NO; } break;
case 5:
# line 154 "gram.in"
{ if(yypvt[-1] && (yypvt[-1]->labelno==dorange))
			enddo(yypvt[-1]->labelno);
		  if(lastwasbranch && thislabel==NULL)
			warn("statement cannot be reached");
		  lastwasbranch = thiswasbranch;
		  thiswasbranch = NO;
		  if(yypvt[-1])
			{
			if(yypvt[-1]->labtype == LABFORMAT)
				err("label already that of a format");
			else
				yypvt[-1]->labtype = LABEXEC;
			}
		} break;
case 6:
# line 169 "gram.in"
{ doinclude( yypvt[-0] ); } break;
case 7:
# line 171 "gram.in"
{ lastwasbranch = NO;  endproc(); } break;
case 8:
# line 173 "gram.in"
{ execerr("unclassifiable statement", 0);  flline(); } break;
case 9:
# line 175 "gram.in"
{ flline();  needkwd = NO;  inioctl = NO; 
		  yyerrok; yyclearin; } break;
case 10:
# line 180 "gram.in"
{
#ifdef SDB
		char buff[10];
		if( sdbflag )
			{
#	ifdef UCBVAXASM
			p2pass( stabdline(N_SLINE, lineno) );
#	else
			sprintf(buff,"LL%d", ++dbglabel);
			p2pass( stabline(0, N_SLINE, lineno, buff) );
			p2pi("LL%d:\n", dbglabel);
#	endif
			}
#endif

		if(yystno != 0)
			{
			yyval = thislabel =  mklabel(yystno);
			if( ! headerdone )
				puthead(NULL, procclass);
			if(thislabel->labdefined)
				execerr("label %s already defined",
					convic(thislabel->stateno) );
			else	{
				if(thislabel->blklevel!=0 && thislabel->blklevel<blklevel
				    && thislabel->labtype!=LABFORMAT)
					warn1("there is a branch to label %s from outside block",
					      convic( (ftnint) (thislabel->stateno) ) );
				thislabel->blklevel = blklevel;
				thislabel->labdefined = YES;
				if(thislabel->labtype != LABFORMAT)
					putlabel(thislabel->labelno);
				}
			}
		else    yyval = thislabel = NULL;
		} break;
case 11:
# line 219 "gram.in"
{startproc(yypvt[-0], CLMAIN); } break;
case 12:
# line 221 "gram.in"
{ if(yypvt[-0]) NO66("named BLOCKDATA");
		  startproc(yypvt[-0], CLBLOCK); } break;
case 13:
# line 224 "gram.in"
{ entrypt(CLPROC, TYSUBR, (ftnint) 0,  yypvt[-1], yypvt[-0]); } break;
case 14:
# line 226 "gram.in"
{ entrypt(CLPROC, TYUNKNOWN, (ftnint) 0, yypvt[-1], yypvt[-0]); } break;
case 15:
# line 228 "gram.in"
{ entrypt(CLPROC, yypvt[-4], varleng, yypvt[-1], yypvt[-0]); } break;
case 16:
# line 230 "gram.in"
{ if(parstate==OUTSIDE || procclass==CLMAIN
			|| procclass==CLBLOCK)
				execerr("misplaced entry statement", 0);
		  entrypt(CLENTRY, 0, (ftnint) 0, yypvt[-1], yypvt[-0]);
		} break;
case 17:
# line 238 "gram.in"
{ newproc(); } break;
case 18:
# line 242 "gram.in"
{ yyval = newentry(yypvt[-0]); } break;
case 19:
# line 246 "gram.in"
{ yyval = mkname(toklen, token); } break;
case 20:
# line 249 "gram.in"
{ yyval = NULL; } break;
case 22:
# line 254 "gram.in"
{ yyval = 0; } break;
case 23:
# line 256 "gram.in"
{ NO66(" () argument list");
		  yyval = 0; } break;
case 24:
# line 259 "gram.in"
{yyval = yypvt[-1]; } break;
case 25:
# line 263 "gram.in"
{ yyval = (yypvt[-0] ? mkchain(yypvt[-0],0) : 0 ); } break;
case 26:
# line 265 "gram.in"
{ if(yypvt[-0]) yypvt[-2] = yyval = hookup(yypvt[-2], mkchain(yypvt[-0],0)); } break;
case 27:
# line 269 "gram.in"
{ if(yypvt[-0]->vstg!=STGUNKNOWN && yypvt[-0]->vstg!=STGARG)
			dclerr("name declared as argument after use", yypvt[-0]);
		  yypvt[-0]->vstg = STGARG;
		} break;
case 28:
# line 274 "gram.in"
{ NO66("altenate return argument");
		  yyval = 0;  substars = YES; } break;
case 29:
# line 281 "gram.in"
{
		char *s;
		s = copyn(toklen+1, token);
		s[toklen] = '\0';
		yyval = s;
		} break;
case 37:
# line 296 "gram.in"
{ NO66("SAVE statement");
		  saveall = YES; } break;
case 38:
# line 299 "gram.in"
{ NO66("SAVE statement"); } break;
case 39:
# line 301 "gram.in"
{ fmtstmt(thislabel); setfmt(thislabel); } break;
case 40:
# line 303 "gram.in"
{ NO66("PARAMETER statement"); } break;
case 41:
# line 307 "gram.in"
{ settype(yypvt[-3], yypvt[-5], yypvt[-0]);
		  if(ndim>0) setbound(yypvt[-3],ndim,dims);
		} break;
case 42:
# line 311 "gram.in"
{ settype(yypvt[-2], yypvt[-4], yypvt[-0]);
		  if(ndim>0) setbound(yypvt[-2],ndim,dims);
		} break;
case 43:
# line 317 "gram.in"
{ varleng = yypvt[-0]; } break;
case 44:
# line 321 "gram.in"
{ varleng = (yypvt[-0]<0 || yypvt[-0]==TYLONG ? 0 : typesize[yypvt[-0]]); } break;
case 45:
# line 324 "gram.in"
{ yyval = TYLONG; } break;
case 46:
# line 325 "gram.in"
{ yyval = TYREAL; } break;
case 47:
# line 326 "gram.in"
{ yyval = TYCOMPLEX; } break;
case 48:
# line 327 "gram.in"
{ yyval = TYDREAL; } break;
case 49:
# line 328 "gram.in"
{ NOEXT("DOUBLE COMPLEX statement"); yyval = TYDCOMPLEX; } break;
case 50:
# line 329 "gram.in"
{ yyval = TYLOGICAL; } break;
case 51:
# line 330 "gram.in"
{ NO66("CHARACTER statement"); yyval = TYCHAR; } break;
case 52:
# line 331 "gram.in"
{ yyval = TYUNKNOWN; } break;
case 53:
# line 332 "gram.in"
{ yyval = TYUNKNOWN; } break;
case 54:
# line 333 "gram.in"
{ NOEXT("AUTOMATIC statement"); yyval = - STGAUTO; } break;
case 55:
# line 334 "gram.in"
{ NOEXT("STATIC statement"); yyval = - STGBSS; } break;
case 56:
# line 338 "gram.in"
{ yyval = varleng; } break;
case 57:
# line 340 "gram.in"
{
		  NO66("length specification *n");
		  if( ! ISICON(yypvt[-0]) )
			{
			yyval = 0;
			dclerr("length must be an integer constant", 0);
			}
		  else yyval = yypvt[-0]->const.ci;
		} break;
case 58:
# line 350 "gram.in"
{  NO66("length specification *(*)"); yyval = 0; } break;
case 59:
# line 354 "gram.in"
{ incomm( yyval = comblock(0, 0) , yypvt[-0] ); } break;
case 60:
# line 356 "gram.in"
{ yyval = yypvt[-1];  incomm(yypvt[-1], yypvt[-0]); } break;
case 61:
# line 358 "gram.in"
{ yyval = yypvt[-2];  incomm(yypvt[-2], yypvt[-0]); } break;
case 62:
# line 360 "gram.in"
{ incomm(yypvt[-2], yypvt[-0]); } break;
case 63:
# line 364 "gram.in"
{ yyval = comblock(0, 0); } break;
case 64:
# line 366 "gram.in"
{ yyval = comblock(toklen, token); } break;
case 65:
# line 370 "gram.in"
{ setext(yypvt[-0]); } break;
case 66:
# line 372 "gram.in"
{ setext(yypvt[-0]); } break;
case 67:
# line 376 "gram.in"
{ NO66("INTRINSIC statement"); setintr(yypvt[-0]); } break;
case 68:
# line 378 "gram.in"
{ setintr(yypvt[-0]); } break;
case 71:
# line 386 "gram.in"
{
		struct Equivblock *p;
		if(nequiv >= MAXEQUIV)
			many("equivalences", 'q');
		p  =  & eqvclass[nequiv++];
		p->eqvinit = 0;
		p->eqvbottom = 0;
		p->eqvtop = 0;
		p->equivs = yypvt[-1];
		} break;
case 72:
# line 399 "gram.in"
{ yyval = ALLOC(Eqvchain); yyval->eqvitem = yypvt[-0]; } break;
case 73:
# line 401 "gram.in"
{ yyval = ALLOC(Eqvchain); yyval->eqvitem = yypvt[-0]; yyval->nextp = yypvt[-2]; } break;
case 76:
# line 409 "gram.in"
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(0, CLMAIN);
			}
		  if(parstate < INDATA)
			{
			enddcl();
			parstate = INDATA;
			}
		} break;
case 77:
# line 423 "gram.in"
{ ftnint junk;
		  if(nextdata(&junk,&junk) != NULL)
			{
			err("too few initializers");
			curdtp = NULL;
			}
		  frdata(yypvt[-3]);
		  frrpl();
		} break;
case 78:
# line 434 "gram.in"
{ toomanyinit = NO; } break;
case 81:
# line 439 "gram.in"
{ dataval(NULL, yypvt[-0]); } break;
case 82:
# line 441 "gram.in"
{ dataval(yypvt[-2], yypvt[-0]); } break;
case 84:
# line 446 "gram.in"
{ if( yypvt[-1]==OPMINUS && ISCONST(yypvt[-0]) )
			consnegop(yypvt[-0]);
		  yyval = yypvt[-0];
		} break;
case 89:
# line 459 "gram.in"
{ int k;
		  yypvt[-0]->vsave = 1;
		  k = yypvt[-0]->vstg;
		if( ! ONEOF(k, M(STGUNKNOWN)|M(STGBSS)|M(STGINIT)) )
			dclerr("can only save static variables", yypvt[-0]);
		} break;
case 90:
# line 466 "gram.in"
{ yypvt[-0]->extsave = 1; } break;
case 93:
# line 474 "gram.in"
{ if(yypvt[-2]->vclass == CLUNKNOWN)
			{ yypvt[-2]->vclass = CLPARAM;
			  yypvt[-2]->paramval = yypvt[-0];
			}
		  else dclerr("cannot make %s parameter", yypvt[-2]);
		} break;
case 94:
# line 483 "gram.in"
{ if(ndim>0) setbounds(yypvt[-1], ndim, dims); } break;
case 95:
# line 487 "gram.in"
{ ptr np;
		  vardcl(np = yypvt[-0]->namep);
		  if(np->vstg == STGBSS)
			np->vstg = STGINIT;
		  else if(np->vstg == STGCOMMON)
			extsymtab[np->vardesc.varno].extinit = YES;
		  else if(np->vstg==STGEQUIV)
			eqvclass[np->vardesc.varno].eqvinit = YES;
		  else if(np->vstg != STGINIT)
			dclerr("inconsistent storage classes", np);
		  yyval = mkchain(yypvt[-0], 0);
		} break;
case 96:
# line 500 "gram.in"
{ chainp p; struct Impldoblock *q;
		q = ALLOC(Impldoblock);
		q->tag = TIMPLDO;
		q->varnp = yypvt[-1]->datap;
		p = yypvt[-1]->nextp;
		if(p)  { q->implb = p->datap; p = p->nextp; }
		if(p)  { q->impub = p->datap; p = p->nextp; }
		if(p)  { q->impstep = p->datap; p = p->nextp; }
		frchain( & (yypvt[-1]) );
		yyval = mkchain(q, 0);
		q->datalist = hookup(yypvt[-3], yyval);
		} break;
case 97:
# line 515 "gram.in"
{ curdtp = yypvt[-0]; curdtelt = 0; } break;
case 98:
# line 517 "gram.in"
{ yyval = hookup(yypvt[-2], yypvt[-0]); } break;
case 99:
# line 521 "gram.in"
{ ndim = 0; } break;
case 101:
# line 525 "gram.in"
{ ndim = 0; } break;
case 104:
# line 530 "gram.in"
{ if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = 0;
			  dims[ndim].ub = yypvt[-0];
			}
		  ++ndim;
		} break;
case 105:
# line 539 "gram.in"
{ if(ndim == maxdim)
			err("too many dimensions");
		  else if(ndim < maxdim)
			{ dims[ndim].lb = yypvt[-2];
			  dims[ndim].ub = yypvt[-0];
			}
		  ++ndim;
		} break;
case 106:
# line 550 "gram.in"
{ yyval = 0; } break;
case 108:
# line 555 "gram.in"
{ nstars = 1; labarray[0] = yypvt[-0]; } break;
case 109:
# line 557 "gram.in"
{ if(nstars < MAXLABLIST)  labarray[nstars++] = yypvt[-0]; } break;
case 110:
# line 561 "gram.in"
{
		if(yypvt[-0] == 0)
			execerr("illegal label", 0);
		else	{
			if(yypvt[-0]->labinacc)
				warn1("illegal branch to inner block, statement %s",
					convic( (ftnint) (yypvt[-0]->stateno) ));
			else if(yypvt[-0]->labdefined == NO)
				yypvt[-0]->blklevel = blklevel;
			yypvt[-0]->labused = YES;
			if(yypvt[-0]->labtype == LABFORMAT)
				err("may not branch to a format");
			else
				yypvt[-0]->labtype = LABEXEC;
			}
		} break;
case 111:
# line 580 "gram.in"
{ yyval = mklabel( convci(toklen, token) ); } break;
case 112:
# line 584 "gram.in"
{ NO66("IMPLICIT statement"); } break;
case 115:
# line 591 "gram.in"
{ needkwd = 1; } break;
case 116:
# line 592 "gram.in"
{ vartype = yypvt[-0]; } break;
case 119:
# line 600 "gram.in"
{ setimpl(vartype, varleng, yypvt[-0], yypvt[-0]); } break;
case 120:
# line 602 "gram.in"
{ setimpl(vartype, varleng, yypvt[-2], yypvt[-0]); } break;
case 121:
# line 606 "gram.in"
{ if(toklen!=1 || token[0]<'a' || token[0]>'z')
			{
			dclerr("implicit item must be single letter", 0);
			yyval = 0;
			}
		  else yyval = token[0];
		} break;
case 122:
# line 616 "gram.in"
{ switch(parstate)	
			{
			case OUTSIDE:	newproc();
					startproc(0, CLMAIN);
			case INSIDE:	parstate = INDCL;
			case INDCL:	break;

			default:
				dclerr("declaration among executables", 0);
			}
		} break;
case 123:
# line 629 "gram.in"
{ yyval = 0; } break;
case 125:
# line 634 "gram.in"
{ yyval = mkchain(yypvt[-0], 0); } break;
case 126:
# line 636 "gram.in"
{ yyval = hookup(yypvt[-2], mkchain(yypvt[-0],0) ); } break;
case 128:
# line 641 "gram.in"
{ yyval = yypvt[-1]; } break;
case 132:
# line 648 "gram.in"
{ yyval = mkexpr(yypvt[-1], yypvt[-2], yypvt[-0]); } break;
case 133:
# line 650 "gram.in"
{ yyval = mkexpr(OPSTAR, yypvt[-2], yypvt[-0]); } break;
case 134:
# line 652 "gram.in"
{ yyval = mkexpr(OPSLASH, yypvt[-2], yypvt[-0]); } break;
case 135:
# line 654 "gram.in"
{ yyval = mkexpr(OPPOWER, yypvt[-2], yypvt[-0]); } break;
case 136:
# line 656 "gram.in"
{ if(yypvt[-1] == OPMINUS)
			yyval = mkexpr(OPNEG, yypvt[-0], 0);
		  else 	yyval = yypvt[-0];
		} break;
case 137:
# line 661 "gram.in"
{ yyval = mkexpr(yypvt[-1], yypvt[-2], yypvt[-0]); } break;
case 138:
# line 663 "gram.in"
{ NO66(".EQV. operator");
		  yyval = mkexpr(OPEQV, yypvt[-2],yypvt[-0]); } break;
case 139:
# line 666 "gram.in"
{ NO66(".NEQV. operator");
		  yyval = mkexpr(OPNEQV, yypvt[-2], yypvt[-0]); } break;
case 140:
# line 669 "gram.in"
{ yyval = mkexpr(OPOR, yypvt[-2], yypvt[-0]); } break;
case 141:
# line 671 "gram.in"
{ yyval = mkexpr(OPAND, yypvt[-2], yypvt[-0]); } break;
case 142:
# line 673 "gram.in"
{ yyval = mkexpr(OPNOT, yypvt[-0], 0); } break;
case 143:
# line 675 "gram.in"
{ NO66("concatenation operator //");
		  yyval = mkexpr(OPCONCAT, yypvt[-2], yypvt[-0]); } break;
case 144:
# line 679 "gram.in"
{ yyval = OPPLUS; } break;
case 145:
# line 680 "gram.in"
{ yyval = OPMINUS; } break;
case 146:
# line 683 "gram.in"
{ yyval = OPEQ; } break;
case 147:
# line 684 "gram.in"
{ yyval = OPGT; } break;
case 148:
# line 685 "gram.in"
{ yyval = OPLT; } break;
case 149:
# line 686 "gram.in"
{ yyval = OPGE; } break;
case 150:
# line 687 "gram.in"
{ yyval = OPLE; } break;
case 151:
# line 688 "gram.in"
{ yyval = OPNE; } break;
case 152:
# line 692 "gram.in"
{ yyval = mkprim(yypvt[-0], 0, 0, 0); } break;
case 153:
# line 694 "gram.in"
{ NO66("substring operator :");
		  yyval = mkprim(yypvt[-5], 0, yypvt[-3], yypvt[-1]); } break;
case 154:
# line 697 "gram.in"
{ yyval = mkprim(yypvt[-3], mklist(yypvt[-1]), 0, 0); } break;
case 155:
# line 699 "gram.in"
{ NO66("substring operator :");
		  yyval = mkprim(yypvt[-8], mklist(yypvt[-6]), yypvt[-3], yypvt[-1]); } break;
case 156:
# line 704 "gram.in"
{ yyval = 0; } break;
case 158:
# line 709 "gram.in"
{ if(yypvt[-0]->vclass == CLPARAM)
			yyval = cpexpr(yypvt[-0]->paramval);
		} break;
case 160:
# line 715 "gram.in"
{ yyval = mklogcon(1); } break;
case 161:
# line 716 "gram.in"
{ yyval = mklogcon(0); } break;
case 162:
# line 717 "gram.in"
{ yyval = mkstrcon(toklen, token); } break;
case 163:
# line 718 "gram.in"
 { yyval = mkintcon( convci(toklen, token) ); } break;
case 164:
# line 719 "gram.in"
 { yyval = mkrealcon(TYREAL, convcd(toklen, token)); } break;
case 165:
# line 720 "gram.in"
 { yyval = mkrealcon(TYDREAL, convcd(toklen, token)); } break;
case 166:
# line 724 "gram.in"
{ yyval = mkcxcon(yypvt[-3],yypvt[-1]); } break;
case 167:
# line 728 "gram.in"
{ NOEXT("hex constant");
		  yyval = mkbitcon(4, toklen, token); } break;
case 168:
# line 731 "gram.in"
{ NOEXT("octal constant");
		  yyval = mkbitcon(3, toklen, token); } break;
case 169:
# line 734 "gram.in"
{ NOEXT("binary constant");
		  yyval = mkbitcon(1, toklen, token); } break;
case 171:
# line 740 "gram.in"
{ yyval = yypvt[-1]; } break;
case 174:
# line 746 "gram.in"
{ yyval = mkexpr(yypvt[-1], yypvt[-2], yypvt[-0]); } break;
case 175:
# line 748 "gram.in"
{ yyval = mkexpr(OPSTAR, yypvt[-2], yypvt[-0]); } break;
case 176:
# line 750 "gram.in"
{ yyval = mkexpr(OPSLASH, yypvt[-2], yypvt[-0]); } break;
case 177:
# line 752 "gram.in"
{ yyval = mkexpr(OPPOWER, yypvt[-2], yypvt[-0]); } break;
case 178:
# line 754 "gram.in"
{ if(yypvt[-1] == OPMINUS)
			yyval = mkexpr(OPNEG, yypvt[-0], 0);
		  else	yyval = yypvt[-0];
		} break;
case 179:
# line 759 "gram.in"
{ NO66("concatenation operator //");
		  yyval = mkexpr(OPCONCAT, yypvt[-2], yypvt[-0]); } break;
case 181:
# line 764 "gram.in"
{
		if(yypvt[-1]->labdefined)
			execerr("no backward DO loops", 0);
		yypvt[-1]->blklevel = blklevel+1;
		exdo(yypvt[-1]->labelno, yypvt[-0]);
		} break;
case 182:
# line 771 "gram.in"
{ exendif();  thiswasbranch = NO; } break;
case 184:
# line 774 "gram.in"
{ exelif(yypvt[-2]); lastwasbranch = NO; } break;
case 185:
# line 776 "gram.in"
{ exelse(); lastwasbranch = NO; } break;
case 186:
# line 778 "gram.in"
{ exendif(); lastwasbranch = NO; } break;
case 187:
# line 782 "gram.in"
{ exif(yypvt[-1]); } break;
case 188:
# line 786 "gram.in"
{ yyval = mkchain(yypvt[-2], yypvt[-0]); } break;
case 189:
# line 790 "gram.in"
{ exequals(yypvt[-2], yypvt[-0]); } break;
case 190:
# line 792 "gram.in"
{ exassign(yypvt[-0], yypvt[-2]); } break;
case 193:
# line 796 "gram.in"
{ inioctl = NO; } break;
case 194:
# line 798 "gram.in"
{ exarif(yypvt[-6], yypvt[-4], yypvt[-2], yypvt[-0]);  thiswasbranch = YES; } break;
case 195:
# line 800 "gram.in"
{ excall(yypvt[-0], 0, 0, labarray); } break;
case 196:
# line 802 "gram.in"
{ excall(yypvt[-2], 0, 0, labarray); } break;
case 197:
# line 804 "gram.in"
{ if(nstars < MAXLABLIST)
			excall(yypvt[-3], mklist(yypvt[-1]), nstars, labarray);
		  else
			err("too many alternate returns");
		} break;
case 198:
# line 810 "gram.in"
{ exreturn(yypvt[-0]);  thiswasbranch = YES; } break;
case 199:
# line 812 "gram.in"
{ exstop(yypvt[-2], yypvt[-0]);  thiswasbranch = yypvt[-2]; } break;
case 200:
# line 816 "gram.in"
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(0, CLMAIN);
			}
		} break;
case 201:
# line 825 "gram.in"
{ exgoto(yypvt[-0]);  thiswasbranch = YES; } break;
case 202:
# line 827 "gram.in"
{ exasgoto(yypvt[-0]);  thiswasbranch = YES; } break;
case 203:
# line 829 "gram.in"
{ exasgoto(yypvt[-4]);  thiswasbranch = YES; } break;
case 204:
# line 831 "gram.in"
{ if(nstars < MAXLABLIST)
			putcmgo(fixtype(yypvt[-0]), nstars, labarray);
		  else
			err("computed GOTO list too long");
		} break;
case 207:
# line 843 "gram.in"
{ nstars = 0; yyval = yypvt[-0]; } break;
case 208:
# line 847 "gram.in"
{ yyval = (yypvt[-0] ? mkchain(yypvt[-0],0) : 0); } break;
case 209:
# line 849 "gram.in"
{ if(yypvt[-0])
			if(yypvt[-2]) yyval = hookup(yypvt[-2], mkchain(yypvt[-0],0));
			else yyval = mkchain(yypvt[-0],0);
		} break;
case 211:
# line 857 "gram.in"
{ if(nstars<MAXLABLIST) labarray[nstars++] = yypvt[-0]; yyval = 0; } break;
case 212:
# line 861 "gram.in"
{ yyval = 0; } break;
case 213:
# line 863 "gram.in"
{ yyval = 1; } break;
case 214:
# line 867 "gram.in"
{ yyval = mkchain(yypvt[-0], 0); } break;
case 215:
# line 869 "gram.in"
{ yyval = hookup(yypvt[-2], mkchain(yypvt[-0],0) ); } break;
case 216:
# line 873 "gram.in"
{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(0, CLMAIN);
			}
		  if(parstate < INDATA) enddcl();
		} break;
case 217:
# line 884 "gram.in"
{ endio(); } break;
case 219:
# line 889 "gram.in"
{ ioclause(IOSUNIT, yypvt[-0]); endioctl(); } break;
case 221:
# line 892 "gram.in"
{ doio(NULL); } break;
case 222:
# line 894 "gram.in"
{ doio(NULL); } break;
case 223:
# line 896 "gram.in"
{ doio(yypvt[-0]); } break;
case 224:
# line 898 "gram.in"
{ doio(yypvt[-0]); } break;
case 225:
# line 900 "gram.in"
{ doio(yypvt[-0]); } break;
case 226:
# line 902 "gram.in"
{ doio(NULL); } break;
case 227:
# line 904 "gram.in"
{ doio(yypvt[-0]); } break;
case 228:
# line 906 "gram.in"
{ doio(NULL); } break;
case 229:
# line 908 "gram.in"
{ doio(yypvt[-0]); } break;
case 231:
# line 915 "gram.in"
{ iostmt = IOBACKSPACE; } break;
case 232:
# line 917 "gram.in"
{ iostmt = IOREWIND; } break;
case 233:
# line 919 "gram.in"
{ iostmt = IOENDFILE; } break;
case 235:
# line 926 "gram.in"
{ iostmt = IOINQUIRE; } break;
case 236:
# line 928 "gram.in"
{ iostmt = IOOPEN; } break;
case 237:
# line 930 "gram.in"
{ iostmt = IOCLOSE; } break;
case 238:
# line 934 "gram.in"
{
		ioclause(IOSUNIT, NULL);
		ioclause(IOSFMT, yypvt[-0]);
		endioctl();
		} break;
case 239:
# line 940 "gram.in"
{
		ioclause(IOSUNIT, NULL);
		ioclause(IOSFMT, NULL);
		endioctl();
		} break;
case 240:
# line 948 "gram.in"
{ if(yypvt[-1]->vtype == TYCHAR)
			{
			ioclause(IOSUNIT, NULL);
			ioclause(IOSFMT, yypvt[-1]);
			}
		  else
			ioclause(IOSUNIT, yypvt[-1]);
		  endioctl();
		} break;
case 241:
# line 958 "gram.in"
{ endioctl(); } break;
case 244:
# line 966 "gram.in"
{ ioclause(IOSPOSITIONAL, yypvt[-0]); } break;
case 245:
# line 968 "gram.in"
{ ioclause(IOSPOSITIONAL, NULL); } break;
case 246:
# line 970 "gram.in"
{ ioclause(yypvt[-1], yypvt[-0]); } break;
case 247:
# line 972 "gram.in"
{ ioclause(yypvt[-1], NULL); } break;
case 248:
# line 976 "gram.in"
{ yyval = iocname(); } break;
case 249:
# line 980 "gram.in"
{ iostmt = IOREAD; } break;
case 250:
# line 984 "gram.in"
{ iostmt = IOWRITE; } break;
case 251:
# line 988 "gram.in"
{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, NULL);
		ioclause(IOSFMT, yypvt[-1]);
		endioctl();
		} break;
case 252:
# line 995 "gram.in"
{
		iostmt = IOWRITE;
		ioclause(IOSUNIT, NULL);
		ioclause(IOSFMT, NULL);
		endioctl();
		} break;
case 253:
# line 1004 "gram.in"
{ yyval = mkchain(yypvt[-0],0); } break;
case 254:
# line 1006 "gram.in"
{ yyval = hookup(yypvt[-2], mkchain(yypvt[-0],0)); } break;
case 256:
# line 1011 "gram.in"
{ yyval = mkiodo(yypvt[-1],yypvt[-3]); } break;
case 257:
# line 1015 "gram.in"
{ yyval = mkchain(yypvt[-0], 0); } break;
case 258:
# line 1017 "gram.in"
{ yyval = mkchain(yypvt[-0], 0); } break;
case 260:
# line 1022 "gram.in"
{ yyval = mkchain(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 261:
# line 1024 "gram.in"
{ yyval = mkchain(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 262:
# line 1026 "gram.in"
{ yyval = mkchain(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 263:
# line 1028 "gram.in"
{ yyval = mkchain(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 264:
# line 1030 "gram.in"
{ yyval = hookup(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 265:
# line 1032 "gram.in"
{ yyval = hookup(yypvt[-2], mkchain(yypvt[-0], 0) ); } break;
case 267:
# line 1037 "gram.in"
{ yyval = mkiodo(yypvt[-1], mkchain(yypvt[-3], 0) ); } break;
case 268:
# line 1039 "gram.in"
{ yyval = mkiodo(yypvt[-1], mkchain(yypvt[-3], 0) ); } break;
case 269:
# line 1041 "gram.in"
{ yyval = mkiodo(yypvt[-1], yypvt[-3]); } break;
case 270:
# line 1045 "gram.in"
{ startioctl(); } break;
		}
		goto yystack;  /* stack new state and value */

	}
