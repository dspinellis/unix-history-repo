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
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	;
break;
case 2:
	;
break;
case 3:
	{ return (ARRAY); }
break;
case 4:
	{ return (_BEGIN); }
break;
case 5:
	{ return (BOOLEAN); }
break;
case 6:
{ return (CARDINAL); }
break;
case 7:
	{ return (CHOICE); }
break;
case 8:
	{ return (DEPENDS); }
break;
case 9:
	{ return (END); }
break;
case 10:
	{ return (ERROR); }
break;
case 11:
	{ return (INTEGER); }
break;
case 12:
	{ return (LONG); }
break;
case 13:
	{ return (OF); }
break;
case 14:
{ return (PROCEDURE); }
break;
case 15:
	{ return (PROGRAM); }
break;
case 16:
	{ return (RECORD); }
break;
case 17:
	{ return (REPORTS); }
break;
case 18:
	{ return (RETURNS); }
break;
case 19:
{ return (SEQUENCE); }
break;
case 20:
	{ return (STRING); }
break;
case 21:
	{ return (TYPE); }
break;
case 22:
{ return (UNSPECIFIED); }
break;
case 23:
	{ return (UPON); }
break;
case 24:
	{ return (VERSION); }
break;
case 25:
		{
				/*
				 * Only decimal numbers are supported.
				 */
				yylval.object = make(O_CONSTANT, atoi(yytext));
				return (number);
			}
break;
case 26:
{
				yylval.object = make(O_SYMBOL, yytext);
				return (identifier);
			}
break;
case 27:
		{
				return ((int) yytext[0]);
			}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] ={
0,

27,
0,

2,
27,
0,

2,
0,

27,
0,

25,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

26,
27,
0,

25,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

13,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

1,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

9,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

12,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

21,
26,
0,

26,
0,

23,
26,
0,

26,
0,

3,
26,
0,

4,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

10,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

7,
26,
0,

26,
0,

26,
0,

26,
0,

26,
0,

16,
26,
0,

26,
0,

26,
0,

26,
0,

20,
26,
0,

26,
0,

26,
0,

5,
26,
0,

26,
0,

8,
26,
0,

11,
26,
0,

26,
0,

15,
26,
0,

17,
26,
0,

18,
26,
0,

26,
0,

26,
0,

24,
26,
0,

6,
26,
0,

26,
0,

19,
26,
0,

26,
0,

14,
26,
0,

26,
0,

26,
0,

22,
26,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] ={
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	6,23,	
0,0,	1,7,	6,24,	6,24,	
6,24,	6,24,	6,24,	6,24,	
6,24,	6,24,	6,24,	6,24,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,8,	1,9,	
1,10,	1,11,	1,12,	1,13,	
1,13,	1,13,	1,14,	1,13,	
1,13,	1,15,	1,13,	1,13,	
1,16,	1,17,	1,13,	1,18,	
1,19,	1,20,	1,21,	1,22,	
1,13,	1,13,	1,13,	1,13,	
13,25,	12,32,	17,37,	25,25,	
1,13,	12,33,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
2,6,	7,24,	7,24,	7,24,	
7,24,	7,24,	7,24,	7,24,	
7,24,	7,24,	7,24,	14,34,	
20,25,	15,35,	26,46,	14,25,	
15,25,	28,48,	29,49,	20,41,	
28,25,	2,9,	2,10,	2,11,	
2,12,	2,13,	2,13,	2,13,	
2,14,	2,13,	2,13,	2,15,	
2,13,	2,13,	2,16,	2,17,	
2,13,	2,18,	2,19,	2,20,	
2,21,	2,22,	2,13,	2,13,	
2,13,	2,13,	31,51,	21,42,	
31,25,	21,43,	2,13,	21,25,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	2,13,	2,13,	
2,13,	2,13,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
30,50,	33,53,	34,25,	30,25,	
34,54,	36,25,	40,61,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,26,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	37,56,	35,55,	44,65,	
37,25,	8,25,	35,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	8,25,	8,25,	8,25,	
8,25,	9,27,	10,29,	11,31,	
27,47,	16,36,	52,25,	18,38,	
19,39,	10,30,	32,52,	9,28,	
22,44,	72,92,	9,25,	27,25,	
11,25,	16,25,	23,23,	10,25,	
18,25,	19,25,	38,57,	19,40,	
32,25,	22,25,	23,23,	23,45,	
39,60,	39,25,	41,62,	46,66,	
41,25,	42,25,	42,63,	38,58,	
47,67,	38,25,	43,64,	38,59,	
48,68,	43,25,	49,69,	50,70,	
51,71,	47,25,	48,25,	53,72,	
46,25,	54,73,	53,25,	56,75,	
50,25,	55,74,	74,25,	56,76,	
49,25,	51,25,	57,77,	62,82,	
58,78,	57,25,	54,25,	58,25,	
55,25,	23,23,	56,25,	59,25,	
60,25,	61,81,	59,79,	60,80,	
62,25,	63,83,	66,25,	63,25,	
64,84,	68,88,	61,25,	69,89,	
64,25,	66,86,	23,23,	65,25,	
65,85,	67,87,	70,90,	71,91,	
69,25,	67,25,	68,25,	71,25,	
73,93,	75,94,	76,95,	77,96,	
78,97,	79,98,	80,99,	82,25,	
81,100,	70,25,	83,101,	73,25,	
81,25,	84,25,	75,25,	85,102,	
86,25,	87,25,	88,103,	80,25,	
90,105,	89,104,	91,106,	83,25,	
85,25,	89,25,	92,25,	93,107,	
94,108,	95,109,	96,110,	105,25,	
97,25,	90,25,	97,111,	88,25,	
91,25,	98,112,	100,114,	99,113,	
93,25,	98,25,	94,25,	99,25,	
96,25,	101,115,	95,25,	102,116,	
103,117,	100,25,	102,25,	104,118,	
103,25,	106,25,	106,119,	107,120,	
108,25,	109,122,	110,25,	108,121,	
101,25,	113,125,	109,25,	111,25,	
111,123,	112,25,	112,124,	114,25,	
104,25,	115,126,	116,127,	117,25,	
119,25,	118,128,	116,25,	120,25,	
113,25,	121,129,	115,25,	118,25,	
122,25,	123,25,	124,25,	125,130,	
127,25,	126,131,	128,25,	129,132,	
130,25,	131,133,	132,25,	133,134,	
134,135,	135,25,	0,0,	0,0,	
125,25,	126,25,	131,25,	0,0,	
129,25,	0,0,	0,0,	0,0,	
133,25,	0,0,	134,25,	0,0,	
0,0};
struct yysvf yysvec[] ={
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-79,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+2,	0,		yyvstop+8,
yycrank+77,	0,		yyvstop+10,
yycrank+154,	0,		yyvstop+13,
yycrank+208,	yysvec+8,	yyvstop+16,
yycrank+213,	yysvec+8,	yyvstop+19,
yycrank+210,	yysvec+8,	yyvstop+22,
yycrank+15,	yysvec+8,	yyvstop+25,
yycrank+10,	yysvec+8,	yyvstop+28,
yycrank+57,	yysvec+8,	yyvstop+31,
yycrank+58,	yysvec+8,	yyvstop+34,
yycrank+211,	yysvec+8,	yyvstop+37,
yycrank+12,	yysvec+8,	yyvstop+40,
yycrank+214,	yysvec+8,	yyvstop+43,
yycrank+215,	yysvec+8,	yyvstop+46,
yycrank+54,	yysvec+8,	yyvstop+49,
yycrank+93,	yysvec+8,	yyvstop+52,
yycrank+219,	yysvec+8,	yyvstop+55,
yycrank+-293,	0,		0,	
yycrank+0,	yysvec+7,	yyvstop+58,
yycrank+13,	yysvec+8,	yyvstop+60,
yycrank+56,	yysvec+8,	yyvstop+62,
yycrank+209,	yysvec+8,	yyvstop+64,
yycrank+62,	yysvec+8,	yyvstop+66,
yycrank+60,	yysvec+8,	yyvstop+68,
yycrank+133,	yysvec+8,	yyvstop+70,
yycrank+90,	yysvec+8,	yyvstop+72,
yycrank+218,	yysvec+8,	yyvstop+74,
yycrank+131,	yysvec+8,	yyvstop+76,
yycrank+132,	yysvec+8,	yyvstop+78,
yycrank+168,	yysvec+8,	yyvstop+80,
yycrank+135,	yysvec+8,	yyvstop+82,
yycrank+166,	yysvec+8,	yyvstop+85,
yycrank+231,	yysvec+8,	yyvstop+87,
yycrank+223,	yysvec+8,	yyvstop+89,
yycrank+136,	yysvec+8,	yyvstop+91,
yycrank+226,	yysvec+8,	yyvstop+93,
yycrank+227,	yysvec+8,	yyvstop+95,
yycrank+235,	yysvec+8,	yyvstop+97,
yycrank+165,	yysvec+8,	yyvstop+99,
yycrank+0,	0,		yyvstop+101,
yycrank+242,	yysvec+8,	yyvstop+103,
yycrank+239,	yysvec+8,	yyvstop+105,
yycrank+240,	yysvec+8,	yyvstop+107,
yycrank+250,	yysvec+8,	yyvstop+109,
yycrank+246,	yysvec+8,	yyvstop+111,
yycrank+251,	yysvec+8,	yyvstop+113,
yycrank+200,	yysvec+8,	yyvstop+115,
yycrank+244,	yysvec+8,	yyvstop+118,
yycrank+256,	yysvec+8,	yyvstop+120,
yycrank+258,	yysvec+8,	yyvstop+122,
yycrank+260,	yysvec+8,	yyvstop+124,
yycrank+255,	yysvec+8,	yyvstop+126,
yycrank+257,	yysvec+8,	yyvstop+128,
yycrank+261,	yysvec+8,	yyvstop+130,
yycrank+262,	yysvec+8,	yyvstop+132,
yycrank+272,	yysvec+8,	yyvstop+134,
yycrank+266,	yysvec+8,	yyvstop+136,
yycrank+269,	yysvec+8,	yyvstop+138,
yycrank+274,	yysvec+8,	yyvstop+140,
yycrank+277,	yysvec+8,	yyvstop+142,
yycrank+268,	yysvec+8,	yyvstop+144,
yycrank+283,	yysvec+8,	yyvstop+146,
yycrank+284,	yysvec+8,	yyvstop+148,
yycrank+282,	yysvec+8,	yyvstop+150,
yycrank+295,	yysvec+8,	yyvstop+152,
yycrank+285,	yysvec+8,	yyvstop+154,
yycrank+207,	yysvec+8,	yyvstop+156,
yycrank+297,	yysvec+8,	yyvstop+158,
yycrank+248,	yysvec+8,	yyvstop+160,
yycrank+300,	yysvec+8,	yyvstop+163,
yycrank+288,	yysvec+8,	yyvstop+165,
yycrank+289,	yysvec+8,	yyvstop+167,
yycrank+290,	yysvec+8,	yyvstop+169,
yycrank+291,	yysvec+8,	yyvstop+171,
yycrank+305,	yysvec+8,	yyvstop+173,
yycrank+298,	yysvec+8,	yyvstop+175,
yycrank+293,	yysvec+8,	yyvstop+177,
yycrank+309,	yysvec+8,	yyvstop+180,
yycrank+299,	yysvec+8,	yyvstop+182,
yycrank+310,	yysvec+8,	yyvstop+185,
yycrank+302,	yysvec+8,	yyvstop+187,
yycrank+303,	yysvec+8,	yyvstop+190,
yycrank+321,	yysvec+8,	yyvstop+193,
yycrank+311,	yysvec+8,	yyvstop+195,
yycrank+319,	yysvec+8,	yyvstop+197,
yycrank+322,	yysvec+8,	yyvstop+199,
yycrank+312,	yysvec+8,	yyvstop+201,
yycrank+326,	yysvec+8,	yyvstop+204,
yycrank+328,	yysvec+8,	yyvstop+206,
yycrank+332,	yysvec+8,	yyvstop+208,
yycrank+330,	yysvec+8,	yyvstop+210,
yycrank+318,	yysvec+8,	yyvstop+212,
yycrank+327,	yysvec+8,	yyvstop+214,
yycrank+329,	yysvec+8,	yyvstop+216,
yycrank+335,	yysvec+8,	yyvstop+218,
yycrank+346,	yysvec+8,	yyvstop+220,
yycrank+336,	yysvec+8,	yyvstop+222,
yycrank+338,	yysvec+8,	yyvstop+224,
yycrank+354,	yysvec+8,	yyvstop+226,
yycrank+317,	yysvec+8,	yyvstop+228,
yycrank+339,	yysvec+8,	yyvstop+231,
yycrank+341,	yysvec+8,	yyvstop+233,
yycrank+342,	yysvec+8,	yyvstop+235,
yycrank+348,	yysvec+8,	yyvstop+237,
yycrank+344,	yysvec+8,	yyvstop+239,
yycrank+349,	yysvec+8,	yyvstop+242,
yycrank+351,	yysvec+8,	yyvstop+244,
yycrank+362,	yysvec+8,	yyvstop+246,
yycrank+353,	yysvec+8,	yyvstop+248,
yycrank+364,	yysvec+8,	yyvstop+251,
yycrank+360,	yysvec+8,	yyvstop+253,
yycrank+357,	yysvec+8,	yyvstop+255,
yycrank+365,	yysvec+8,	yyvstop+258,
yycrank+358,	yysvec+8,	yyvstop+260,
yycrank+361,	yysvec+8,	yyvstop+263,
yycrank+363,	yysvec+8,	yyvstop+266,
yycrank+366,	yysvec+8,	yyvstop+268,
yycrank+367,	yysvec+8,	yyvstop+271,
yycrank+368,	yysvec+8,	yyvstop+274,
yycrank+382,	yysvec+8,	yyvstop+277,
yycrank+383,	yysvec+8,	yyvstop+279,
yycrank+370,	yysvec+8,	yyvstop+281,
yycrank+372,	yysvec+8,	yyvstop+284,
yycrank+386,	yysvec+8,	yyvstop+287,
yycrank+374,	yysvec+8,	yyvstop+289,
yycrank+384,	yysvec+8,	yyvstop+292,
yycrank+376,	yysvec+8,	yyvstop+294,
yycrank+390,	yysvec+8,	yyvstop+297,
yycrank+392,	yysvec+8,	yyvstop+299,
yycrank+379,	yysvec+8,	yyvstop+301,
0,	0,	0};
struct yywork *yytop = yycrank+474;
struct yysvf *yybgin = yysvec+1;
char yymatch[] ={
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,'A' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] ={
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
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
