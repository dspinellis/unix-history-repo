# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin ={stdin}, *yyout ={stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
#ifndef lint
static char sccsid[] = "@(#)scan.l	4.1 (Berkeley) 10/20/82";
#endif

#include "y.tab.h"
#include "htable.h"
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	{
			yylval.number = KW_NET;
			return (KEYWORD);
		}
break;
case 2:
{
			yylval.number = KW_GATEWAY;
			return (KEYWORD);
		}
break;
case 3:
	{
			yylval.number = KW_HOST;
			return (KEYWORD);
		}
break;
case 4:
{
			yylval.namelist = newname(yytext);
			return (NAME);
		}
break;
case 5:
	return (NAME);
break;
case 6:
{
			yylval.number = atoi(yytext);
			return (NUMBER);
		}
break;
case 7:
	return ('.');
break;
case 8:
	return (':');
break;
case 9:
	return (',');
break;
case 10:
	return ('/');
break;
case 11:
	;
break;
case 12:
;
break;
case 13:
;
break;
case 14:
	return (END);
break;
case 15:
	fprintf(stderr, "Illegal char: '%s'\n", yytext);
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

yywrap()
{
	return (1);
}
int yyvstop[] ={
0,

15,
0,

13,
15,
0,

14,
0,

9,
15,
0,

7,
15,
0,

10,
15,
0,

6,
15,
0,

8,
15,
0,

11,
15,
0,

5,
15,
0,

5,
15,
0,

5,
15,
0,

5,
15,
0,

13,
0,

12,
0,

6,
0,

11,
0,

4,
0,

4,
0,

4,
0,

4,
0,

4,
0,

4,
0,

1,
4,
0,

4,
0,

3,
4,
0,

4,
0,

4,
0,

2,
4,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] ={
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	11,19,	
0,0,	0,0,	1,4,	1,5,	
4,16,	5,17,	0,0,	11,19,	
11,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
19,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	4,16,	
5,17,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,6,	1,3,	1,7,	
1,8,	1,9,	2,6,	11,19,	
2,7,	2,8,	11,19,	0,0,	
0,0,	0,0,	0,0,	1,10,	
1,11,	0,0,	0,0,	0,0,	
2,10,	2,11,	1,12,	0,0,	
0,0,	0,0,	0,0,	11,19,	
1,13,	1,14,	0,0,	0,0,	
0,0,	2,13,	2,14,	1,15,	
13,22,	30,31,	0,0,	15,24,	
2,15,	9,18,	9,18,	9,18,	
9,18,	9,18,	9,18,	9,18,	
9,18,	9,18,	9,18,	12,20,	
12,20,	12,20,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
14,23,	22,25,	23,26,	24,27,	
25,28,	26,29,	28,30,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	12,21,	12,21,	12,21,	
12,21,	31,32,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] ={
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-6,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+3,	0,		yyvstop+3,
yycrank+4,	0,		yyvstop+6,
yycrank+0,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+14,
yycrank+37,	0,		yyvstop+17,
yycrank+0,	0,		yyvstop+20,
yycrank+-6,	0,		yyvstop+23,
yycrank+50,	0,		yyvstop+26,
yycrank+15,	yysvec+12,	yyvstop+29,
yycrank+29,	yysvec+12,	yyvstop+32,
yycrank+14,	yysvec+12,	yyvstop+35,
yycrank+0,	yysvec+4,	yyvstop+38,
yycrank+0,	yysvec+5,	yyvstop+40,
yycrank+0,	yysvec+9,	yyvstop+42,
yycrank+-14,	yysvec+11,	yyvstop+44,
yycrank+0,	yysvec+12,	0,	
yycrank+0,	yysvec+12,	yyvstop+46,
yycrank+25,	yysvec+12,	yyvstop+48,
yycrank+27,	yysvec+12,	yyvstop+50,
yycrank+27,	yysvec+12,	yyvstop+52,
yycrank+43,	yysvec+12,	yyvstop+54,
yycrank+29,	yysvec+12,	yyvstop+56,
yycrank+0,	yysvec+12,	yyvstop+58,
yycrank+27,	yysvec+12,	yyvstop+61,
yycrank+0,	yysvec+12,	yyvstop+63,
yycrank+16,	yysvec+12,	yyvstop+66,
yycrank+52,	yysvec+12,	yyvstop+68,
yycrank+0,	yysvec+12,	yyvstop+70,
0,	0,	0};
struct yywork *yytop = yycrank+141;
struct yysvf *yybgin = yysvec+1;
char yymatch[] ={
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,'-' ,'-' ,'-' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] ={
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank){		/* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
