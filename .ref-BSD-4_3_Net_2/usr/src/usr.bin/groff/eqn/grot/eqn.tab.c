
/*  A Bison parser, made from eqn.y  */

#define	OVER	258
#define	SMALLOVER	259
#define	SQRT	260
#define	SUB	261
#define	SUP	262
#define	LPILE	263
#define	RPILE	264
#define	CPILE	265
#define	PILE	266
#define	LEFT	267
#define	RIGHT	268
#define	TO	269
#define	FROM	270
#define	SIZE	271
#define	FONT	272
#define	ROMAN	273
#define	BOLD	274
#define	ITALIC	275
#define	FAT	276
#define	ACCENT	277
#define	BAR	278
#define	UNDER	279
#define	ABOVE	280
#define	TEXT	281
#define	QUOTED_TEXT	282
#define	FWD	283
#define	BACK	284
#define	DOWN	285
#define	UP	286
#define	MATRIX	287
#define	COL	288
#define	LCOL	289
#define	RCOL	290
#define	CCOL	291
#define	MARK	292
#define	LINEUP	293
#define	TYPE	294
#define	VCENTER	295
#define	PRIME	296
#define	SPLIT	297
#define	NOSPLIT	298
#define	UACCENT	299
#define	SPACE	300
#define	GFONT	301
#define	GSIZE	302
#define	DEFINE	303
#define	NDEFINE	304
#define	TDEFINE	305
#define	SDEFINE	306
#define	UNDEF	307
#define	IFDEF	308
#define	INCLUDE	309
#define	DELIM	310
#define	CHARTYPE	311
#define	SET	312
#define	GRFONT	313
#define	GBFONT	314

#line 19 "eqn.y"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "box.h"
extern int non_empty_flag;
char *strsave(const char *);
#define YYDEBUG 1
int yylex();
void yyerror(const char *);

#line 32 "eqn.y"
typedef union {
	char *str;
	box *b;
	pile_box *pb;
	matrix_box *mb;
	int n;
	column *col;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		139
#define	YYFLAG		-32768
#define	YYNTBASE	65

#define YYTRANSLATE(x) ((unsigned)(x) <= 314 ? yytranslate[x] : 82)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,    62,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    60,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    63,     2,    64,    61,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59
};

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   122,   124,   128,   131,   141,   144,   146,   150,   153,   155,
   157,   159,   163,   166,   168,   170,   174,   177,   181,   184,
   186,   190,   193,   195,   197,   199,   201,   203,   205,   207,
   209,   211,   213,   215,   217,   219,   221,   223,   225,   227,
   229,   231,   233,   235,   237,   239,   241,   243,   245,   247,
   249,   251,   253,   257,   267,   270,   274,   277,   281,   284,
   288,   291,   295,   298,   302,   305,   307,   309,   313,   315,
   319,   322,   324
};

static const char * const yytname[] = {     0,
"error","$illegal.","OVER","SMALLOVER","SQRT","SUB","SUP","LPILE","RPILE","CPILE",
"PILE","LEFT","RIGHT","TO","FROM","SIZE","FONT","ROMAN","BOLD","ITALIC",
"FAT","ACCENT","BAR","UNDER","ABOVE","TEXT","QUOTED_TEXT","FWD","BACK","DOWN",
"UP","MATRIX","COL","LCOL","RCOL","CCOL","MARK","LINEUP","TYPE","VCENTER",
"PRIME","SPLIT","NOSPLIT","UACCENT","SPACE","GFONT","GSIZE","DEFINE","NDEFINE","TDEFINE",
"SDEFINE","UNDEF","IFDEF","INCLUDE","DELIM","CHARTYPE","SET","GRFONT","GBFONT","'^'",
"'~'","'\\t'","'{'","'}'","top"
};
#endif

static const short yyr1[] = {     0,
    65,    65,    66,    66,    67,    67,    67,    68,    68,    68,
    68,    68,    69,    69,    69,    69,    70,    70,    71,    71,
    71,    72,    72,    72,    72,    72,    72,    72,    72,    72,
    72,    72,    72,    72,    72,    72,    72,    72,    72,    72,
    72,    72,    72,    72,    72,    72,    72,    72,    72,    72,
    72,    72,    72,    73,    74,    74,    75,    75,    76,    76,
    77,    77,    78,    78,    79,    79,    79,    79,    80,    80,
    81,    81,    81
};

static const short yyr2[] = {     0,
     0,     1,     1,     2,     1,     2,     2,     1,     3,     3,
     5,     5,     1,     2,     3,     3,     1,     3,     1,     3,
     5,     1,     1,     2,     2,     1,     1,     1,     3,     2,
     2,     2,     2,     4,     5,     3,     2,     2,     2,     3,
     3,     2,     2,     2,     2,     3,     3,     3,     3,     3,
     3,     3,     2,     1,     1,     3,     3,     4,     1,     2,
     1,     3,     3,     4,     2,     2,     2,     2,     1,     1,
     1,     1,     1
};

static const short yydefact[] = {     1,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    22,    23,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    26,    27,    28,     0,     2,
     3,     5,     8,    13,    17,    19,    14,    69,    70,     0,
     0,    31,    54,    32,    33,    30,    72,    73,    71,     0,
     0,     0,    42,    43,    44,    45,     0,     0,     0,     0,
     0,     6,     7,     0,    53,    24,    25,     0,     4,     0,
     0,     0,     0,     0,     0,     0,    37,    38,    39,     0,
    55,     0,     0,    36,    47,    46,    48,    49,    51,    50,
     0,     0,     0,     0,     0,    59,    52,    29,    15,    16,
     9,    10,    20,    19,    18,    40,    41,     0,    57,     0,
     0,     0,     0,    65,    66,    67,    68,    34,    60,     0,
     0,     0,    56,    58,    35,    61,     0,     0,    11,    12,
    21,     0,    63,     0,    62,    64,     0,     0,     0
};

static const short yydefgoto[] = {   137,
    81,    31,    32,    33,    34,    35,    36,    41,    82,    42,
    95,   127,   114,    96,    43,    50
};

static const short yypact[] = {   259,
   318,     1,     1,     1,     1,     4,    70,    70,   356,   356,
   356,   356,-32768,-32768,    70,    70,    70,    70,   -46,   259,
   259,    70,   356,     9,    22,-32768,-32768,-32768,   259,   259,
-32768,-32768,    20,-32768,-32768,    15,-32768,-32768,-32768,   259,
   -44,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   259,
   356,   356,    58,    58,    58,    58,   356,   356,   356,   356,
    11,-32768,-32768,   356,    58,-32768,-32768,   140,-32768,   318,
   318,   318,   318,   356,   356,   356,-32768,-32768,-32768,   356,
   259,   -12,   259,   200,    58,    58,    58,    58,    58,    58,
     6,     6,     6,     6,     7,-32768,    58,-32768,-32768,-32768,
-32768,    91,-32768,   383,-32768,-32768,-32768,   259,-32768,   -11,
     4,   259,   -38,-32768,-32768,-32768,-32768,-32768,-32768,   318,
   318,   356,   259,-32768,-32768,   259,   -10,   259,-32768,-32768,
-32768,   259,-32768,    -7,   259,-32768,    26,    51,-32768
};

static const short yypgoto[] = {-32768,
     0,   -19,   -60,     2,   -59,   -16,    67,    -8,   -28,    84,
-32768,   -62,    -1,   -25,    -2,   -37
};


#define	YYLAST		427


static const short yytable[] = {    30,
    62,    63,    37,    49,    51,    52,    57,    58,    59,    60,
    69,   101,   108,   108,   132,   105,    61,   132,    83,    64,
    74,    75,    70,    71,   128,   138,    38,    39,    68,    38,
    39,    38,    39,    72,    73,    66,    76,    77,    78,    91,
    92,    93,    94,    91,    92,    93,    94,    67,    69,    84,
   139,   109,   124,   133,   110,    79,   136,   103,    80,   129,
   130,    69,   131,    40,    69,   134,    47,    48,   112,   119,
   118,    99,   100,   125,   102,    53,    54,    55,    56,    76,
    77,    78,   113,   113,   113,   113,    44,    45,    46,    65,
   115,   116,   117,    70,    71,    38,    39,     0,    79,     0,
     0,    80,     0,    69,   120,   121,    69,   123,    49,     0,
     0,   126,     0,     0,     0,    69,     0,    85,    86,     0,
     0,     0,     0,    87,    88,    89,    90,   126,     0,     0,
    97,   135,     0,     0,     0,     0,     0,     0,     0,     0,
   104,     0,   106,     0,     1,     0,   107,     2,     3,     4,
     5,     6,     0,     0,     0,     7,     8,     9,    10,    11,
    12,     0,     0,     0,     0,    13,    14,    15,    16,    17,
    18,    19,     0,     0,     0,     0,    20,    21,    22,    23,
     0,    24,    25,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    26,
    27,    28,    29,    98,     1,     0,     0,     2,     3,     4,
     5,     6,   111,     0,     0,     7,     8,     9,    10,    11,
    12,     0,     0,     0,     0,    13,    14,    15,    16,    17,
    18,    19,     0,     0,     0,     0,    20,    21,    22,    23,
     0,    24,    25,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    26,
    27,    28,    29,     1,     0,     0,     2,     3,     4,     5,
     6,     0,     0,     0,     7,     8,     9,    10,    11,    12,
     0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
    19,     0,     0,     0,     0,    20,    21,    22,    23,     0,
    24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    26,    27,
    28,    29,     1,     0,     0,     2,     3,     4,     5,     6,
     0,     0,     0,     7,     8,     9,    10,    11,    12,     0,
     0,     0,     0,    13,    14,    15,    16,    17,    18,    19,
     0,     0,     0,     0,     0,     0,    22,    23,     0,    24,
    25,     0,     0,     2,     3,     4,     5,     6,     0,     0,
     0,     7,     8,     9,    10,    11,    12,    26,    27,    28,
    29,    13,    14,    15,    16,    17,    18,    19,    74,   122,
     0,     0,     0,     0,    22,    23,     0,    24,    25,     0,
     0,     0,     0,     0,    76,    77,    78,     0,     0,     0,
     0,     0,     0,     0,     0,    26,    27,    28,    29,     0,
     0,     0,     0,    79,     0,     0,    80
};

static const short yycheck[] = {     0,
    20,    21,     1,     6,     7,     8,    15,    16,    17,    18,
    30,    72,    25,    25,    25,    75,    63,    25,    63,    22,
     6,     7,     3,     4,    63,     0,    26,    27,    29,    26,
    27,    26,    27,    14,    15,    27,    22,    23,    24,    33,
    34,    35,    36,    33,    34,    35,    36,    26,    68,    50,
     0,    64,    64,    64,    83,    41,    64,    74,    44,   120,
   121,    81,   122,    63,    84,   128,    63,    64,    63,    95,
    64,    70,    71,   111,    73,     9,    10,    11,    12,    22,
    23,    24,    91,    92,    93,    94,     3,     4,     5,    23,
    92,    93,    94,     3,     4,    26,    27,    -1,    41,    -1,
    -1,    44,    -1,   123,    14,    15,   126,   108,   111,    -1,
    -1,   112,    -1,    -1,    -1,   135,    -1,    51,    52,    -1,
    -1,    -1,    -1,    57,    58,    59,    60,   128,    -1,    -1,
    64,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    74,    -1,    76,    -1,     5,    -1,    80,     8,     9,    10,
    11,    12,    -1,    -1,    -1,    16,    17,    18,    19,    20,
    21,    -1,    -1,    -1,    -1,    26,    27,    28,    29,    30,
    31,    32,    -1,    -1,    -1,    -1,    37,    38,    39,    40,
    -1,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,
    61,    62,    63,    64,     5,    -1,    -1,     8,     9,    10,
    11,    12,    13,    -1,    -1,    16,    17,    18,    19,    20,
    21,    -1,    -1,    -1,    -1,    26,    27,    28,    29,    30,
    31,    32,    -1,    -1,    -1,    -1,    37,    38,    39,    40,
    -1,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,
    61,    62,    63,     5,    -1,    -1,     8,     9,    10,    11,
    12,    -1,    -1,    -1,    16,    17,    18,    19,    20,    21,
    -1,    -1,    -1,    -1,    26,    27,    28,    29,    30,    31,
    32,    -1,    -1,    -1,    -1,    37,    38,    39,    40,    -1,
    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,    61,
    62,    63,     5,    -1,    -1,     8,     9,    10,    11,    12,
    -1,    -1,    -1,    16,    17,    18,    19,    20,    21,    -1,
    -1,    -1,    -1,    26,    27,    28,    29,    30,    31,    32,
    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,    -1,    42,
    43,    -1,    -1,     8,     9,    10,    11,    12,    -1,    -1,
    -1,    16,    17,    18,    19,    20,    21,    60,    61,    62,
    63,    26,    27,    28,    29,    30,    31,    32,     6,     7,
    -1,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    -1,
    -1,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    60,    61,    62,    63,    -1,
    -1,    -1,    -1,    41,    -1,    -1,    44
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifdef __GNUC__
#ifndef alloca
#define alloca __builtin_alloca
#endif /* Not alloca. */
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#include <alloca.h>
#endif /* Sparc.  */
#endif /* Not GNU C.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYIMPURE
#define YYLEX		yylex()
#endif

#ifndef YYPURE
#define YYLEX		yylex(&yylval, &yylloc)
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYIMPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* YYIMPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
#ifdef __cplusplus
__yy_bcopy (char *from, char *to, int count)
#else
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
#endif
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#line 137 "/usr/local/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/

#define YYPOPSTACK   (yyvsp--, yysp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yysp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifndef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Next token is %d (%s)\n", yychar, yytname[yychar1]);
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      if (yylen == 1)
	fprintf (stderr, "Reducing 1 value via rule %d (line %d), ",
		 yyn, yyrline[yyn]);
      else
	fprintf (stderr, "Reducing %d values via rule %d (line %d), ",
		 yylen, yyn, yyrline[yyn]);
    }
#endif


  switch (yyn) {

case 2:
#line 125 "eqn.y"
{ yyvsp[0].b->top_level(); non_empty_flag = 1; ;
    break;}
case 3:
#line 130 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 4:
#line 132 "eqn.y"
{
		  list_box *lb = yyvsp[-1].b->to_list_box();
		  if (!lb)
		    lb = new list_box(yyvsp[-1].b);
		  lb->append(yyvsp[0].b);
		  yyval.b = lb;
		;
    break;}
case 5:
#line 143 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 6:
#line 145 "eqn.y"
{ yyval.b = make_mark_box(yyvsp[0].b); ;
    break;}
case 7:
#line 147 "eqn.y"
{ yyval.b = make_lineup_box(yyvsp[0].b); ;
    break;}
case 8:
#line 152 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 9:
#line 154 "eqn.y"
{ yyval.b = make_limit_box(yyvsp[-2].b, 0, yyvsp[0].b); ;
    break;}
case 10:
#line 156 "eqn.y"
{ yyval.b = make_limit_box(yyvsp[-2].b, yyvsp[0].b, 0); ;
    break;}
case 11:
#line 158 "eqn.y"
{ yyval.b = make_limit_box(yyvsp[-4].b, yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 12:
#line 160 "eqn.y"
{ yyval.b = make_limit_box(yyvsp[-4].b, make_limit_box(yyvsp[-2].b, yyvsp[0].b, 0), 0); ;
    break;}
case 13:
#line 165 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 14:
#line 167 "eqn.y"
{ yyval.b = make_sqrt_box(yyvsp[0].b); ;
    break;}
case 15:
#line 169 "eqn.y"
{ yyval.b = make_over_box(yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 16:
#line 171 "eqn.y"
{ yyval.b = make_small_over_box(yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 17:
#line 176 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 18:
#line 178 "eqn.y"
{ yyval.b = make_script_box(yyvsp[-2].b, 0, yyvsp[0].b); ;
    break;}
case 19:
#line 183 "eqn.y"
{ yyval.b = yyvsp[0].b; ;
    break;}
case 20:
#line 185 "eqn.y"
{ yyval.b = make_script_box(yyvsp[-2].b, yyvsp[0].b, 0); ;
    break;}
case 21:
#line 187 "eqn.y"
{ yyval.b = make_script_box(yyvsp[-4].b, yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 22:
#line 192 "eqn.y"
{ yyval.b = split_text(yyvsp[0].str); ;
    break;}
case 23:
#line 194 "eqn.y"
{ yyval.b = new quoted_text_box(yyvsp[0].str); ;
    break;}
case 24:
#line 196 "eqn.y"
{ yyval.b = split_text(yyvsp[0].str); ;
    break;}
case 25:
#line 198 "eqn.y"
{ yyval.b = new quoted_text_box(yyvsp[0].str); ;
    break;}
case 26:
#line 200 "eqn.y"
{ yyval.b = new half_space_box; ;
    break;}
case 27:
#line 202 "eqn.y"
{ yyval.b = new space_box; ;
    break;}
case 28:
#line 204 "eqn.y"
{ yyval.b = new tab_box; ;
    break;}
case 29:
#line 206 "eqn.y"
{ yyval.b = yyvsp[-1].b; ;
    break;}
case 30:
#line 208 "eqn.y"
{ yyvsp[0].pb->set_alignment(CENTER_ALIGN); yyval.b = yyvsp[0].pb; ;
    break;}
case 31:
#line 210 "eqn.y"
{ yyvsp[0].pb->set_alignment(LEFT_ALIGN); yyval.b = yyvsp[0].pb; ;
    break;}
case 32:
#line 212 "eqn.y"
{ yyvsp[0].pb->set_alignment(RIGHT_ALIGN); yyval.b = yyvsp[0].pb; ;
    break;}
case 33:
#line 214 "eqn.y"
{ yyvsp[0].pb->set_alignment(CENTER_ALIGN); yyval.b = yyvsp[0].pb; ;
    break;}
case 34:
#line 216 "eqn.y"
{ yyval.b = yyvsp[-1].mb; ;
    break;}
case 35:
#line 218 "eqn.y"
{ yyval.b = make_delim_box(yyvsp[-3].str, yyvsp[-2].b, yyvsp[0].str); ;
    break;}
case 36:
#line 220 "eqn.y"
{ yyval.b = make_delim_box(yyvsp[-1].str, yyvsp[0].b, 0); ;
    break;}
case 37:
#line 222 "eqn.y"
{ yyval.b = make_overline_box(yyvsp[-1].b); ;
    break;}
case 38:
#line 224 "eqn.y"
{ yyval.b = make_underline_box(yyvsp[-1].b); ;
    break;}
case 39:
#line 226 "eqn.y"
{ yyval.b = make_prime_box(yyvsp[-1].b); ;
    break;}
case 40:
#line 228 "eqn.y"
{ yyval.b = make_accent_box(yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 41:
#line 230 "eqn.y"
{ yyval.b = make_uaccent_box(yyvsp[-2].b, yyvsp[0].b); ;
    break;}
case 42:
#line 232 "eqn.y"
{ yyval.b = new font_box(strsave(get_grfont()), yyvsp[0].b); ;
    break;}
case 43:
#line 234 "eqn.y"
{ yyval.b = new font_box(strsave(get_gbfont()), yyvsp[0].b); ;
    break;}
case 44:
#line 236 "eqn.y"
{ yyval.b = new font_box(strsave(get_gfont()), yyvsp[0].b); ;
    break;}
case 45:
#line 238 "eqn.y"
{ yyval.b = new fat_box(yyvsp[0].b); ;
    break;}
case 46:
#line 240 "eqn.y"
{ yyval.b = new font_box(yyvsp[-1].str, yyvsp[0].b); ;
    break;}
case 47:
#line 242 "eqn.y"
{ yyval.b = new size_box(yyvsp[-1].str, yyvsp[0].b); ;
    break;}
case 48:
#line 244 "eqn.y"
{ yyval.b = new hmotion_box(yyvsp[-1].n, yyvsp[0].b); ;
    break;}
case 49:
#line 246 "eqn.y"
{ yyval.b = new hmotion_box(-yyvsp[-1].n, yyvsp[0].b); ;
    break;}
case 50:
#line 248 "eqn.y"
{ yyval.b = new vmotion_box(yyvsp[-1].n, yyvsp[0].b); ;
    break;}
case 51:
#line 250 "eqn.y"
{ yyval.b = new vmotion_box(-yyvsp[-1].n, yyvsp[0].b); ;
    break;}
case 52:
#line 252 "eqn.y"
{ yyvsp[0].b->set_spacing_type(yyvsp[-1].str); yyval.b = yyvsp[0].b; ;
    break;}
case 53:
#line 254 "eqn.y"
{ yyval.b = new vcenter_box(yyvsp[0].b); ;
    break;}
case 54:
#line 259 "eqn.y"
{
		  int n;
		  if (sscanf(yyvsp[0].str, "%d", &n) == 1)
		    yyval.n = n;
		  delete yyvsp[0].str;
		;
    break;}
case 55:
#line 269 "eqn.y"
{ yyval.pb = new pile_box(yyvsp[0].b); ;
    break;}
case 56:
#line 271 "eqn.y"
{ yyvsp[-2].pb->append(yyvsp[0].b); yyval.pb = yyvsp[-2].pb; ;
    break;}
case 57:
#line 276 "eqn.y"
{ yyval.pb = yyvsp[-1].pb; ;
    break;}
case 58:
#line 278 "eqn.y"
{ yyvsp[-1].pb->set_space(yyvsp[-3].n); yyval.pb = yyvsp[-1].pb; ;
    break;}
case 59:
#line 283 "eqn.y"
{ yyval.mb = new matrix_box(yyvsp[0].col); ;
    break;}
case 60:
#line 285 "eqn.y"
{ yyvsp[-1].mb->append(yyvsp[0].col); yyval.mb = yyvsp[-1].mb; ;
    break;}
case 61:
#line 290 "eqn.y"
{ yyval.col = new column(yyvsp[0].b); ;
    break;}
case 62:
#line 292 "eqn.y"
{ yyvsp[-2].col->append(yyvsp[0].b); yyval.col = yyvsp[-2].col; ;
    break;}
case 63:
#line 297 "eqn.y"
{ yyval.col = yyvsp[-1].col; ;
    break;}
case 64:
#line 299 "eqn.y"
{ yyvsp[-1].col->set_space(yyvsp[-3].n); yyval.col = yyvsp[-1].col; ;
    break;}
case 65:
#line 304 "eqn.y"
{ yyvsp[0].col->set_alignment(CENTER_ALIGN); yyval.col = yyvsp[0].col; ;
    break;}
case 66:
#line 306 "eqn.y"
{ yyvsp[0].col->set_alignment(LEFT_ALIGN); yyval.col = yyvsp[0].col; ;
    break;}
case 67:
#line 308 "eqn.y"
{ yyvsp[0].col->set_alignment(RIGHT_ALIGN); yyval.col = yyvsp[0].col; ;
    break;}
case 68:
#line 310 "eqn.y"
{ yyvsp[0].col->set_alignment(CENTER_ALIGN); yyval.col = yyvsp[0].col; ;
    break;}
case 69:
#line 314 "eqn.y"
{ yyval.str = yyvsp[0].str; ;
    break;}
case 70:
#line 316 "eqn.y"
{ yyval.str = yyvsp[0].str; ;
    break;}
case 71:
#line 321 "eqn.y"
{ yyval.str = yyvsp[0].str; ;
    break;}
case 72:
#line 323 "eqn.y"
{ yyval.str = strsave("{"); ;
    break;}
case 73:
#line 325 "eqn.y"
{ yyval.str = strsave("}"); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 362 "/usr/local/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) xmalloc(size + 15);
	  strcpy(msg, "parse error");

	  if (count < 5)
	    {
	      count = 0;
	      for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
		if (yycheck[x + yyn] == x)
		  {
		    strcat(msg, count == 0 ? ", expecting `" : " or `");
		    strcat(msg, yytname[x]);
		    strcat(msg, "'");
		    count++;
		  }
	    }
	  yyerror(msg);
	  free(msg);
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 328 "eqn.y"

