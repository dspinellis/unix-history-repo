# define xxif 300
# define xxelse 301
# define xxwhile 302
# define xxrept 303
# define xxdo 304
# define xxrb 305
# define xxpred 306
# define xxident 307
# define xxle 308
# define xxge 309
# define xxne 310
# define xxnum 311
# define xxcom 312
# define xxstring 313
# define xxexplist 314
# define xxidpar 315
# define xxelseif 316
# define xxlb 318
# define xxend 319
# define xxcase 320
# define xxswitch 321
# define xxuntil 322
# define xxdefault 323
# define xxeq 324
# define xxuminus 281

# line 17 "beauty.y"
#include "b.h"
#include <stdio.h>
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

# line 23 "beauty.y"
struct node *t;
# define YYERRCODE 256

# line 228 "beauty.y"

#define ASSERT(X,Y)	if (!(X)) error("struct bug: assertion 'X' invalid in routine Y","","");

yyerror(s)
char *s;
	{
	extern int yychar;
	fprintf(stderr,"\n%s",s);
	fprintf(stderr," in beautifying, output line %d,",xxlineno + 1);
	fprintf(stderr," on input: ");
		switch (yychar) {
			case '\t': fprintf(stderr,"\\t\n"); return;
			case '\n': fprintf(stderr,"\\n\n"); return;
			case '\0': fprintf(stderr,"$end\n"); return;
			default: fprintf(stderr,"%c\n",yychar); return;
			}
	}

yyinit(argc, argv)			/* initialize pushdown store */
int argc;
char *argv[];
	{
	xxindent = 0;
	xxbpertab = 8;
	xxmaxchars = 120;
	}


#include <signal.h>
main()
	{
	int exit();
	if ( signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, exit);
	yyinit();
	yyparse();
	}


putout(type,string)			/* output string with proper indentation */
int type;
char *string;
	{
	static int lasttype;
	if ( (lasttype != 0) && (lasttype != '\n') && (lasttype != ' ') && (lasttype != '\t') && (type == xxcom))
		accum("\t");
	else if (lasttype == xxcom && type != '\n')
		tab(xxindent);
	else
		if (lasttype == xxif	||
			lasttype == xxwhile	||
			lasttype == xxdo	||
			type == '='	||
			lasttype == '='	||
			(lasttype == xxident && (type == xxident || type == xxnum) )	||
			(lasttype == xxnum && type == xxnum) )
			accum(" ");
	accum(string);
	lasttype = type;
	}


accum(token)		/* fill output buffer, generate continuation lines */
char *token;
	{
	static char *buffer;
	static int lstatus,llen,bufind;
	int tstatus,tlen,i;

#define NEW	0
#define MID	1
#define CONT	2

	if (buffer == 0)
		{
		buffer = malloc(xxmaxchars);
		if (buffer == 0) error("malloc out of space","","");
		}
	tlen = slength(token);
	if (tlen == 0) return;
	for (i = 0; i < tlen; ++i)
		ASSERT(token[i] != '\n' || tlen == 1,accum);
	switch(token[tlen-1])
		{
		case '\n':	tstatus = NEW;
				break;
		case '+':
		case '-':
		case '*':
		case ',':
		case '|':
		case '&':
		case '(':	tstatus = CONT;
				break;
		default:	tstatus = MID;
		}
	if (llen + bufind + tlen > xxmaxchars && lstatus == CONT && tstatus != NEW)
		{
		putchar('\n');
		++xxlineno;
		for (i = 0; i < xxindent; ++i)
			putchar('\t');
		putchar(' ');putchar(' ');
		llen = 2 + xxindent * xxbpertab;
		lstatus = NEW;
		}
	if (lstatus == CONT && tstatus == MID)
		{			/* store in buffer in case need \n after last CONT char */
		ASSERT(bufind + tlen < xxmaxchars,accum);
		for (i = 0; i < tlen; ++i)
			buffer[bufind++] = token[i];
		}
	else
		{
		for (i = 0; i < bufind; ++i)
			putchar(buffer[i]);
		llen += bufind;
		bufind = 0;
		for (i = 0; i < tlen; ++i)
			putchar(token[i]);
		if (tstatus == NEW) ++xxlineno;
		llen = (tstatus == NEW) ? 0 : llen + tlen;
		lstatus = tstatus;
		}
	}

tab(n)
int n;
	{
	int i;
	newline();
	for ( i = 0;  i < n; ++i)
		putout('\t',"\t");
	}

newline()
	{
	static int already;
	if (already)
		putout('\n',"\n");
	else
		already = 1;
	}

error(mess1, mess2, mess3)
char *mess1, *mess2, *mess3;
	{
	fprintf(stderr,"\nerror in beautifying, output line %d: %s %s %s \n",
		xxlineno, mess1, mess2, mess3);
	exit(1);
	}







push(type)
int type;
	{
	if (++xxstind > xxtop)
		error("nesting too deep, stack overflow","","");
	xxstack[xxstind] = type;
	}

pop()
	{
	if (xxstind <= 0)
		error("stack exhausted, can't be popped as requested","","");
	--xxstind;
	}


forst()
	{
	while( (xxval = yylex()) != '\n')
		{
		putout(xxval, yylval);
		free(yylval);
		}
	free(yylval);
	}
short yyexca[] ={
-1, 0,
	312, 17,
	-2, 16,
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	0, 18,
	312, 17,
	-2, 16,
-1, 6,
	312, 17,
	-2, 16,
-1, 7,
	312, 17,
	-2, 16,
-1, 29,
	312, 17,
	-2, 16,
-1, 36,
	312, 17,
	-2, 16,
-1, 46,
	322, 16,
	-2, 65,
-1, 93,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 51,
-1, 94,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 52,
-1, 95,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 53,
-1, 96,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 54,
-1, 97,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 55,
-1, 98,
	308, 0,
	309, 0,
	310, 0,
	324, 0,
	60, 0,
	62, 0,
	-2, 56,
-1, 104,
	312, 17,
	-2, 16,
-1, 110,
	312, 17,
	125, 23,
	-2, 16,
-1, 126,
	312, 17,
	-2, 16,
-1, 130,
	312, 17,
	320, 21,
	323, 21,
	125, 21,
	-2, 16,
-1, 132,
	312, 17,
	320, 21,
	323, 21,
	125, 21,
	-2, 16,
-1, 133,
	312, 17,
	320, 21,
	323, 21,
	125, 21,
	-2, 16,
	};
# define YYNPROD 76
# define YYLAST 384
short yyact[]={

  66,  31, 103,  55,  62,  60, 113,  61,  66,  63,
  11,  40,  62,  60, 107,  61, 119,  63,  38, 120,
  34,  44,  68,  39,  67,  11,  11,  31,  57,  31,
  68,  66,  67,  85,  99,  62,  60,  11,  61,  66,
  63, 110,  59,  62,  60, 109,  61, 100,  63,   8,
   2,  64,  82,  68,   6,  67,  64,  29, 131,  30,
  33,  68, 127,  67,  64,  36,  66,  51, 106,  16,
  62,  60,  76,  61,  66,  63,  50,  24,  62,  60,
  62,  61,  41,  63, 102,  63,  65,  64,  68, 118,
  67, 124, 117,   7,  65,  64,  68,   3,  67,   5,
  14,  62,  60,  21,  61,  32,  63,  22,  23,  28,
  48,  52,  27,  83,  80,  15,  20,  65,  19,  68,
  78,  67,  64,  18,  26,  65,  25,  14,  35,  53,
  64,  54,  64,  45,  58, 104,  49,  62,  60,  17,
  61,  56,  63,   9,   4,  13,  79,  46,  47,   1,
   0,   0,  65,  64, 112, 114, 116, 129,   9,   9,
 112,   0,   0, 128, 134, 125, 135, 136,   0,   0,
   0, 112,   0, 121,  77, 123, 101, 132,  22,  23,
  28, 133,   0,  27, 126,  84,   0,  10,   0,  64,
 130,   0,   0,  37,   0,  26,   0,  25,  12,  55,
   0,   0,  10,  10, 111,   0,   0,   0,   0, 115,
 111,   0,   0,  12,  12,  73,  74,  75, 108,   0,
   0, 111,   0,   0,   0,   0,   0,   0,  81,   0,
  14,  14,   0,   0,   0,   0,   0,  86,  87,  88,
  89,  90,  91,  92,  93,  94,  95,  96,  97,  98,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 105,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  70,  71,  72,   0,   0,   0,   0,   0,  70,  71,
  72,   0,   0,   0,   0,  44,  69,   0,   0,  42,
 122,  43,   0,   0,  69,   0,   0,   0,   0,   0,
   0,  70,  71,  72,   0,   0,   0,   0,   0,  70,
  71,  72,   0,   0,   0,   0,   0,  69,   0,   0,
   0,   0,   0,   0,   0,  69,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,  70,  71,  72,   0,
   0,   0,   0,   0,  70,  71,  72,   0,   0,   0,
   0,   0,  69,   0,   0,   0,   0,   0,   0,   0,
  69,   0,   0,   0,   0,   0,   0,  70,  71,  72,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,  69 };
short yypact[]={

 -97,-1000, -97,-1000,  29,-195, -98, -97,-292,-1000,
-1000,-1000,-1000,-1000,-1000,-1000, -22,  29,-1000,-1000,
  29,-1000,-1000,-1000,  23,-1000,-1000,-1000,-286, -97,
-1000,-1000,-1000,-113,-1000,-273, -97,   1, -22, -22,
 -22,-1000,-1000,-1000,  32,-1000,-1000,-1000,-1000,-1000,
-1000, -22,  -9,-124,-1000,-1000,-1000,-1000,-1000,-1000,
 -22, -22, -22, -22, -22, -22, -22, -22, -22, -22,
 -22, -22, -22,  -7, -43,  59, -22,-1000,-1000,-320,
 -86,  28, -22,-1000,-1000,-1000,  38,  38, -43, -43,
 -43,  36,  59,  95,  95,  95,  95,  95,  95,-1000,
  27, -30,  29,-1000,-1000, -38,-1000, -22,-1000,-1000,
-1000,-304,-292, -22,-1000, -96,-1000, -22,-1000,-1000,
   4,-309,  28,-1000,-1000,   0, -97,-1000,-1000,-1000,
 -97,-1000, -97, -97,-1000,-1000,-1000 };
short yypgo[]={

   0, 149,  50, 145,  97, 144, 115, 128, 141,  99,
 139, 123, 120, 118, 116, 110,  65,  54,  45,  59,
  33, 103,  93,  49,  60,  41,  92,  91,  89,  82,
  47, 176,  84,  77,  76 };
short yyr1[]={

   0,   1,   2,   2,   4,   4,   4,   4,   4,   4,
   4,   4,   4,   4,   4,   4,   9,  23,   3,   7,
  16,  20,  18,  18,  25,  25,  25,  25,  26,  28,
  14,  21,  21,  29,  29,  27,  30,  30,  15,  15,
   6,  31,  31,  31,  31,  31,  31,  31,  31,  31,
  31,  31,  31,  31,  31,  31,  31,  31,  31,  31,
   5,   8,  10,  11,  12,  12,  32,  13,  33,  34,
  34,  17,  19,  22,  24,  24 };
short yyr2[]={

   0,   2,   1,   2,   5,   3,   4,   4,   3,   9,
   2,   4,   2,   2,   3,   1,   0,   0,   0,   3,
   0,   0,   2,   1,   6,   5,   5,   3,   1,   2,
   1,   1,   1,   4,   1,   2,   3,   1,   1,   0,
   3,   3,   2,   2,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   1,   1,   1,
   1,   1,   1,   1,   3,   0,   1,   2,   6,   2,
   0,   1,   1,   1,   1,   2 };
short yychk[]={

-1000,  -1,  -2,  -4,  -5,  -9, -17, -22, -23, 256,
 300, 123, 311,  -3,  -4,  -6,  40, -10, -11, -13,
 -14, -21, 302, 303, -33, 321, 319, 307, 304,  -2,
 -19, 125,  -4, -24, 312,  -7, -16, -31,  40,  45,
  33, -29, 311, 313, 307,  -6,  -7,  -7, -15,  -6,
 -34,  44, -29,  -9,  -4, 312,  -8, 301,  -4,  41,
  43,  45,  42,  47,  94, 124,  38,  62,  60, 324,
 308, 309, 310, -31, -31, -31,  40,  -7, -12,  -9,
 -16, -31,  61, -19,  -7, -20, -31, -31, -31, -31,
 -31, -31, -31, -31, -31, -31, -31, -31, -31,  41,
 -30, -31, -32, 322, -17, -31,  41,  44,  -6, -18,
 -25,  -9, -23,  44, -30,  -9, -18, -26, -28, 320,
 323, -24, -31, -19, -27, -30, -16,  58, -25, -20,
 -16,  58,  -2,  -2, -20, -20, -20 };
short yydef[]={

  -2,  -2,  -2,   2,   0,   0,  -2,  -2,   0,  15,
  60,  71,  73,   1,   3,  20,   0,   0,  20,  20,
  39,  10,  62,  63,  70,  30,  31,  32,   0,  -2,
  12,  72,  13,  16,  74,   5,  -2,   0,   0,   0,
   0,  57,  58,  59,  34,  20,  -2,   8,  20,  38,
  67,   0,   0,   0,  14,  75,  20,  61,  21,  40,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,  42,  43,   0,   6,   7,   0,
   0,  69,   0,  11,   4,  19,  44,  45,  46,  47,
  48,  49,  50,  -2,  -2,  -2,  -2,  -2,  -2,  41,
   0,  37,   0,  66,  -2,   0,  33,   0,  64,  16,
  -2,   0,   0,   0,  36,   0,  22,   0,  20,  28,
   0,  16,  68,  21,  20,   0,  -2,  29,  27,   9,
  -2,  35,  -2,  -2,  25,  26,  24 };
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
			
case 16:
# line 49 "beauty.y"
	{
			if (!xxlablast) tab(xxindent);
			xxlablast = 0;
			} break;
case 17:
# line 54 "beauty.y"
	newline(); break;
case 18:
# line 55 "beauty.y"
	putout('\n',"\n"); break;
case 20:
# line 57 "beauty.y"

				{
				if (xxstack[xxstind] != xxlb)
					++xxindent;
				} break;
case 21:
# line 62 "beauty.y"

				{if (xxstack[xxstind] != xxlb && xxstack[xxstind] != xxelseif)
					--xxindent;
				pop();
				} break;
case 28:
# line 77 "beauty.y"
	{putout(xxcase,"case "); free (yypvt[-0]); push(xxcase); } break;
case 29:
# line 80 "beauty.y"
		{
						putout(xxcase,"default");
						free(yypvt[-1]);
						putout(':',":");
						free(yypvt[-0]);
						push(xxcase);
						} break;
case 30:
# line 87 "beauty.y"
	{putout(xxswitch,"switch"); free(yypvt[-0]); push(xxswitch); } break;
case 31:
# line 89 "beauty.y"
	{
				free(yypvt[-0]);
				putout(xxident,"end");
				putout('\n',"\n");
				putout('\n',"\n");
				putout('\n',"\n");
				} break;
case 32:
# line 96 "beauty.y"
	{
				putout(xxident,yypvt[-0]);
				free(yypvt[-0]);
				newflag = 1;
				forst();
				newflag = 0;
				} break;
case 33:
# line 106 "beauty.y"
	{
				xxt = addroot(yypvt[-3],xxident,0,0);
				yyval = addroot("",xxidpar,xxt,yypvt[-1]);
				} break;
case 34:
# line 111 "beauty.y"
	yyval = addroot(yypvt[-0],xxident,0,0); break;
case 35:
# line 114 "beauty.y"
	{
				yield(yypvt[-1],0);
				putout(':',":");
				freetree(yypvt[-1]);
				} break;
case 36:
# line 119 "beauty.y"
	yyval = addroot(yypvt[-1],xxexplist,checkneg(yypvt[-2],0),yypvt[-0]); break;
case 37:
# line 120 "beauty.y"
	yyval = checkneg(yypvt[-0],0); break;
case 40:
# line 128 "beauty.y"
	{ t = checkneg(yypvt[-1],0);
				yield(t,100);  freetree(t);	} break;
case 41:
# line 131 "beauty.y"
	yyval = yypvt[-1]; break;
case 42:
# line 132 "beauty.y"
	yyval = addroot(yypvt[-1],xxuminus,yypvt[-0],0); break;
case 43:
# line 133 "beauty.y"
	yyval = addroot(yypvt[-1],'!',yypvt[-0],0); break;
case 44:
# line 134 "beauty.y"
	yyval = addroot(yypvt[-1],'+',yypvt[-2],yypvt[-0]); break;
case 45:
# line 135 "beauty.y"
	yyval = addroot(yypvt[-1],'-',yypvt[-2],yypvt[-0]); break;
case 46:
# line 136 "beauty.y"
	yyval = addroot(yypvt[-1],'*',yypvt[-2],yypvt[-0]); break;
case 47:
# line 137 "beauty.y"
	yyval = addroot(yypvt[-1],'/',yypvt[-2],yypvt[-0]); break;
case 48:
# line 138 "beauty.y"
	yyval = addroot(yypvt[-1],'^',yypvt[-2],yypvt[-0]); break;
case 49:
# line 139 "beauty.y"
	yyval = addroot(yypvt[-1],'|',yypvt[-2],yypvt[-0]); break;
case 50:
# line 140 "beauty.y"
	yyval = addroot(yypvt[-1],'&',yypvt[-2],yypvt[-0]); break;
case 51:
# line 141 "beauty.y"
	yyval = addroot(yypvt[-1],'>',yypvt[-2],yypvt[-0]); break;
case 52:
# line 142 "beauty.y"
	yyval = addroot(yypvt[-1],'<',yypvt[-2],yypvt[-0]); break;
case 53:
# line 143 "beauty.y"
	yyval = addroot(yypvt[-1],xxeq,yypvt[-2],yypvt[-0]); break;
case 54:
# line 144 "beauty.y"
	yyval = addroot(yypvt[-1],xxle,yypvt[-2],yypvt[-0]); break;
case 55:
# line 145 "beauty.y"
	yyval = addroot(yypvt[-1],xxge,yypvt[-2],yypvt[-0]); break;
case 56:
# line 146 "beauty.y"
	yyval = addroot(yypvt[-1],xxne,yypvt[-2],yypvt[-0]); break;
case 57:
# line 147 "beauty.y"
	yyval = yypvt[-0]; break;
case 58:
# line 148 "beauty.y"
	yyval = addroot(yypvt[-0],xxnum,0,0); break;
case 59:
# line 149 "beauty.y"
	yyval = addroot(yypvt[-0],xxstring,0,0); break;
case 60:
# line 152 "beauty.y"

				{
				if (xxstack[xxstind] == xxelse && !xxlablast)
					{
					--xxindent;
					xxstack[xxstind] = xxelseif;
					putout(' '," ");
					}
				else
					{
					if (!xxlablast)
						tab(xxindent);
					xxlablast = 0;
					}
				putout(xxif,"if");
				free(yypvt[-0]);
				push(xxif);
				} break;
case 61:
# line 170 "beauty.y"

				{
				tab(xxindent);
				putout(xxelse,"else");
				free(yypvt[-0]);
				push(xxelse);
				} break;
case 62:
# line 177 "beauty.y"
	{
				putout(xxwhile,"while");
				free(yypvt[-0]);
				push(xxwhile);
				} break;
case 63:
# line 182 "beauty.y"
			{
					putout(xxrept,"repeat");
					free(yypvt[-0]);
					push(xxrept);
					} break;
case 66:
# line 191 "beauty.y"
 	{
			putout('\t',"\t");
			putout(xxuntil,"until");
			free(yypvt[-0]);
			} break;
case 68:
# line 198 "beauty.y"

					{push(xxdo);
					putout(xxdo,"do");
					free(yypvt[-5]);
					puttree(yypvt[-4]);
					putout('=',"=");
					free(yypvt[-3]);
					puttree(yypvt[-2]);
					putout(',',",");
					free(yypvt[-1]);
					puttree(yypvt[-0]);
					} break;
case 69:
# line 210 "beauty.y"
	{
						putout(',',",");
						puttree(yypvt[-0]);
						} break;
case 71:
# line 215 "beauty.y"
	{
				putout('{'," {");
				push(xxlb);
				} break;
case 72:
# line 219 "beauty.y"
	{ putout('}',"}");  pop();   } break;
case 73:
# line 220 "beauty.y"
	{
				tab(xxindent);
				putout(xxnum,yypvt[-0]);
				putout(' ',"  ");
				xxlablast = 1;
				} break;
case 74:
# line 226 "beauty.y"
	{ putout(xxcom,yypvt[-0]);  free(yypvt[-0]);  xxlablast = 0; } break;
case 75:
# line 227 "beauty.y"
 { putout ('\n',"\n"); putout(xxcom,yypvt[-0]);  free(yypvt[-0]);  xxlablast = 0; } break;
		}
		goto yystack;  /* stack new state and value */

	}
