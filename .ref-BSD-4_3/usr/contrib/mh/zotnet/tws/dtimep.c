# include "stdio.h"
static int start_cond = 0;
#define BEGIN start_cond =
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
# define Z 2
#include "tws.h"
#include <ctype.h>

/*
 * Table to convert month names to numeric month.  We use the
 * fact that the low order 5 bits of the sum of the 2nd & 3rd
 * characters of the name is a hash with no collisions for the 12
 * valid month names.  (The mask to 5 bits maps any combination of
 * upper and lower case into the same hash value).
 */
static	int month_map[] = {
	0,
	6,	/* 1 - Jul */
	3,	/* 2 - Apr */
	5,	/* 3 - Jun */
	0,
	10,	/* 5 - Nov */
	0,
	1,	/* 7 - Feb */
	11,	/* 8 - Dec */
	0,
	0,
	0,
	0,
	0,
	0,
	0,	/*15 - Jan */
	0,
	0,
	0,
	2,	/*19 - Mar */
	0,
	8,	/*21 - Sep */
	0,
	9,	/*23 - Oct */
	0,
	0,
	4,	/*26 - May */
	0,
	7	/*28 - Aug */
};
/*
 * Same trick for day-of-week using the hash function
 *  (c1 & 7) + (c2 & 4)
 */
static	int day_map[] = {
	0,
	0,
	0,
	6,	/* 3 - Sat */
	4,	/* 4 - Thu */
	0,
	5,	/* 6 - Fri */
	0,	/* 7 - Sun */
	2,	/* 8 - Tue */
	1	/* 9 - Mon */,
	0,
	3	/*11 - Wed */
};
#define SETDAY	tw.tw_wday= day_map[(cp[0] & 7) + (cp[1] & 4)];\
		tw.tw_flags |= TW_SEXP;\
		cp += 2;
#define SETMONTH tw.tw_mon = month_map[(cp[0] + cp[1]) & 0x1f]; gotdate++;\
		 cp += 2;\
		 SKIPD;
#define CVT2	(i=(*cp++ - '0'),isdigit(*cp)? i*10 + (*cp++ - '0') : i)
#define SKIPD	while ( !isdigit(*cp++) ) ;  --cp;
#define ZONE(x)	tw.tw_zone=(x);
#define ZONED(x)  tw.tw_zone=(x); tw.tw_flags |= TW_DST;
#define LC(c)	(isupper(c) ? tolower(c) : (c))
struct tws *dparsetime (str)
	char *str;
{
	register int i;
	static struct tws tw;
	register char *cp;
	register int gotdate = 0;

	start_cond = 0;
	bzero( (char *) &tw, sizeof tw);
	while (isspace(*str))
		str++;
	while ( 1 )
		switch (cp = str, *cp? lex_string( &str, start_cond) : 0) {

		case -1:
			if (!gotdate || tw.tw_year == 0)
				return 0;
			/* fall through */
		case 0:
			return &tw;

case 1:
			SETDAY;
break;
case 2:
		cp++, SETDAY;
break;
case 3:
	{
					tw.tw_mday = CVT2; cp++;
					tw.tw_mon  = CVT2 - 1; cp += 3;
					tw.tw_year = CVT2;
					gotdate++;
					}
break;
case 4:
	{
					tw.tw_mday = CVT2; cp++;
					tw.tw_mon  = CVT2 - 1; cp++;
					tw.tw_year = CVT2;
					gotdate++;
					}
break;
case 5:
{
					tw.tw_mday = CVT2;
					while ( !isalpha(*cp++) )
						;
					SETMONTH;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i % 100;
					}
break;
case 6:
	{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT2;
					SKIPD;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i % 100;
					}
break;
case 7:
		{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT2;
					}
break;
case 8:
			{
					tw.tw_hour = CVT2; cp++;
					tw.tw_min  = CVT2; cp++;
					tw.tw_sec  = CVT2;
					BEGIN Z;
					}
break;
case 9:
			case 10:
			{
					tw.tw_hour = CVT2; cp++;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
break;
case 11:
			{
					tw.tw_hour = CVT2 + 12; cp++;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
break;
case 12:
		{
					tw.tw_hour = CVT2;
					tw.tw_min  = CVT2;
					tw.tw_sec  = CVT2;
					BEGIN Z;
					}
break;
case 13:
		{
					tw.tw_hour = CVT2;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
break;
case 14:
			ZONE(0 * 60);
break;
case 15:
			ZONE(0 * 60);
break;
case 16:
			ZONE(2 * 60);
break;
case 17:
			ZONED(2 * 60);
break;
case 18:
			ZONE(-5 * 60);
break;
case 19:
			ZONED(-5 * 60);
break;
case 20:
			ZONE(-6 * 60);
break;
case 21:
			ZONED(-6 * 60);
break;
case 22:
			ZONE(-7 * 60);
break;
case 23:
			ZONED(-7 * 60);
break;
case 24:
			ZONE(-8 * 60);
break;
case 25:
			ZONED(-8 * 60);
break;
case 26:
			ZONE(-(3 * 60 + 30));
break;
case 27:
			ZONE(-4 * 60);
break;
case 28:
			ZONED(-4 * 60);
break;
case 29:
			ZONE(-9 * 60);
break;
case 30:
			ZONED(-9 * 60);
break;
case 31:
			ZONE(-10 * 60);
break;
case 32:
			ZONED(-10 * 60);
break;
case 33:
			ZONED(-1 * 60);
break;
case 34:
			tw.tw_zone = 60 * (('a'-1) - LC(*cp));
break;
case 35:
			tw.tw_zone = 60 * ('a' - LC(*cp));
break;
case 36:
			tw.tw_zone = 60 * (LC(*cp) - 'm');
break;
case 37:
		{
					cp++;
					tw.tw_zone = ((cp[0] * 10 + cp[1])
						     -('0' * 10   + '0'))*60
						    +((cp[2] * 10 + cp[3])
						     -('0' * 10   + '0'));
					cp += 4;
					}
break;
case 38:
		{
					cp++;
					tw.tw_zone = (('0' * 10   + '0')
						     -(cp[0] * 10 + cp[1]))*60
						    +(('0' * 10   + '0')
						     -(cp[2] * 10 + cp[3]));
					cp += 4;
					}
break;
case 39:
		{
					while( !isdigit(*cp++) )
						;
					cp++;
					tw.tw_year = CVT2;
					}
break;
case 40:
case 41:
;
break;
default: return(0);
} return(0); }
/* end of yylex */
int yyvstop[] ={
0,

41,
0,

40,
0,

41,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

34,
0,

35,
0,

35,
0,

36,
0,

36,
0,

36,
0,

36,
0,

36,
0,

36,
0,

36,
0,

36,
0,

36,
0,

14,
0,

9,
0,

1,
0,

1,
0,

1,
0,

1,
0,

1,
0,

1,
0,

1,
0,

28,
0,

27,
0,

33,
0,

21,
0,

20,
0,

19,
0,

18,
0,

15,
0,

32,
0,

31,
0,

17,
0,

16,
0,

23,
0,

22,
0,

26,
0,

25,
0,

24,
0,

30,
0,

29,
0,

13,
0,

9,
0,

9,
0,

1,
0,

2,
0,

13,
0,

8,
0,

10,
0,

11,
0,

5,
0,

5,
0,

7,
0,

39,
0,

37,
0,

38,
0,

2,
0,

4,
0,

4,
0,

12,
0,

8,
0,

5,
0,

5,
0,

7,
0,

7,
0,

1,
0,

3,
0,

5,
0,

6,
0,

6,
0,

6,
0,

6,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] ={
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,5,	1,6,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,5,	0,0,	0,0,	
3,20,	3,6,	5,5,	0,0,	
0,0,	1,7,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,8,	1,8,	1,8,	
1,9,	1,9,	1,9,	1,9,	
1,9,	1,9,	1,9,	3,20,	
0,0,	5,5,	0,0,	0,0,	
0,0,	0,0,	0,0,	3,7,	
0,0,	0,0,	3,21,	0,0,	
3,22,	20,20,	0,0,	3,8,	
3,8,	3,8,	3,9,	3,9,	
3,9,	3,9,	3,9,	3,9,	
3,9,	9,61,	9,61,	9,61,	
9,61,	9,61,	9,61,	9,61,	
9,61,	9,61,	9,61,	0,0,	
20,20,	0,0,	1,10,	21,80,	
21,80,	1,11,	0,0,	1,12,	
0,0,	0,0,	0,0,	1,13,	
0,0,	0,0,	1,14,	1,15,	
1,16,	20,79,	0,0,	0,0,	
1,17,	1,18,	0,0,	0,0,	
1,19,	0,0,	0,0,	0,0,	
3,23,	3,24,	3,25,	3,26,	
3,27,	3,28,	3,29,	3,30,	
3,31,	3,32,	3,33,	3,33,	
3,34,	3,35,	3,36,	3,37,	
3,38,	3,38,	3,39,	3,40,	
3,41,	3,38,	3,42,	3,38,	
3,43,	7,44,	10,62,	11,64,	
8,49,	15,71,	16,72,	10,63,	
7,45,	19,78,	24,96,	29,101,	
13,67,	41,111,	7,46,	7,47,	
44,114,	8,49,	7,48,	8,50,	
8,51,	8,51,	8,51,	8,51,	
8,51,	8,51,	8,51,	8,51,	
8,51,	8,51,	8,52,	12,65,	
13,68,	14,69,	17,73,	18,76,	
25,97,	27,99,	17,74,	22,81,	
22,81,	23,94,	30,102,	32,67,	
12,66,	35,71,	32,104,	14,70,	
18,77,	35,108,	37,109,	25,98,	
27,100,	23,62,	17,75,	43,112,	
23,95,	30,103,	23,63,	34,69,	
45,115,	32,105,	34,106,	32,68,	
46,116,	37,110,	47,118,	48,120,	
54,126,	8,53,	43,113,	53,124,	
8,54,	34,70,	8,55,	55,127,	
53,125,	34,107,	8,56,	47,119,	
57,130,	8,57,	8,58,	8,59,	
46,117,	58,131,	59,132,	8,60,	
22,82,	22,83,	22,84,	60,133,	
22,85,	49,53,	22,86,	22,87,	
49,54,	22,88,	49,55,	62,134,	
22,89,	22,90,	49,56,	22,91,	
63,135,	49,57,	49,58,	49,59,	
22,92,	64,136,	65,137,	49,60,	
22,93,	50,121,	50,121,	50,121,	
50,121,	50,121,	50,121,	50,121,	
50,121,	50,121,	50,121,	51,122,	
51,122,	51,122,	51,122,	51,122,	
51,122,	51,122,	51,122,	51,122,	
51,122,	52,123,	52,123,	52,123,	
52,123,	52,123,	52,123,	52,123,	
52,123,	52,123,	52,123,	56,128,	
61,49,	66,138,	67,139,	68,140,	
69,142,	68,141,	70,144,	71,145,	
72,146,	73,147,	74,148,	69,143,	
75,149,	61,49,	76,150,	61,50,	
77,151,	78,152,	79,153,	56,129,	
90,108,	94,156,	95,157,	96,158,	
97,159,	98,160,	61,52,	80,154,	
80,154,	80,154,	80,154,	80,154,	
80,154,	80,154,	80,154,	80,154,	
80,154,	81,155,	81,155,	81,155,	
81,155,	81,155,	81,155,	81,155,	
81,155,	81,155,	81,155,	82,94,	
88,104,	89,106,	99,161,	100,162,	
101,163,	102,164,	103,165,	104,166,	
105,167,	106,168,	107,169,	108,170,	
109,171,	110,172,	82,95,	88,105,	
89,107,	112,173,	113,174,	114,175,	
115,176,	116,177,	117,178,	118,179,	
119,180,	120,181,	121,182,	121,183,	
121,183,	121,183,	121,183,	121,183,	
121,183,	121,183,	121,183,	121,183,	
121,183,	122,184,	122,184,	122,184,	
122,184,	122,184,	122,184,	122,184,	
122,184,	122,184,	122,184,	123,185,	
124,190,	125,191,	126,192,	127,193,	
128,194,	129,195,	130,197,	129,196,	
131,199,	132,200,	133,201,	134,202,	
135,202,	130,198,	136,205,	141,143,	
144,211,	145,212,	146,213,	148,215,	
149,216,	176,225,	123,185,	175,223,	
177,223,	178,227,	137,202,	138,207,	
139,202,	140,202,	142,202,	143,202,	
147,207,	150,207,	134,202,	135,202,	
151,207,	181,223,	123,186,	123,186,	
123,186,	123,186,	123,186,	123,186,	
123,186,	123,186,	123,186,	123,186,	
123,187,	137,202,	138,207,	139,202,	
140,202,	142,202,	143,202,	147,207,	
150,207,	152,207,	180,223,	151,207,	
179,223,	183,182,	138,207,	153,220,	
153,220,	153,220,	153,220,	147,207,	
150,207,	186,187,	188,236,	151,207,	
189,237,	192,243,	196,198,	199,247,	
200,248,	201,249,	203,143,	204,251,	
152,207,	205,252,	175,224,	206,253,	
208,254,	209,255,	210,143,	123,188,	
211,256,	212,257,	213,258,	214,259,	
152,207,	215,260,	216,261,	217,262,	
185,185,	218,263,	219,264,	223,268,	
177,226,	224,269,	123,189,	225,270,	
135,204,	226,271,	181,230,	134,203,	
154,221,	154,221,	154,221,	154,221,	
154,221,	154,221,	154,221,	154,221,	
154,221,	154,221,	138,208,	185,185,	
142,210,	155,222,	155,222,	155,222,	
155,222,	155,222,	155,222,	155,222,	
155,222,	155,222,	155,222,	137,206,	
180,229,	179,228,	184,233,	227,272,	
139,209,	228,273,	150,217,	229,274,	
147,214,	140,143,	151,218,	182,231,	
182,232,	182,231,	182,231,	182,231,	
182,231,	182,231,	182,231,	182,231,	
182,231,	230,275,	232,277,	240,282,	
241,198,	184,233,	152,219,	187,235,	
187,235,	187,235,	187,235,	187,235,	
187,235,	187,235,	187,235,	187,235,	
187,235,	242,283,	190,238,	243,284,	
244,285,	184,234,	184,234,	184,234,	
184,234,	184,234,	184,234,	184,234,	
184,234,	184,234,	184,234,	190,238,	
185,188,	191,238,	190,239,	190,240,	
190,239,	190,239,	190,239,	190,239,	
190,239,	190,239,	190,239,	190,239,	
245,286,	246,198,	191,238,	185,189,	
193,238,	191,239,	191,240,	191,239,	
191,239,	191,239,	191,239,	191,239,	
191,239,	191,239,	191,239,	247,287,	
248,288,	193,238,	194,238,	249,289,	
193,239,	193,240,	193,239,	193,239,	
193,239,	193,239,	193,239,	193,239,	
193,239,	193,239,	251,143,	194,238,	
195,238,	252,293,	194,239,	194,240,	
194,239,	194,239,	194,239,	194,239,	
194,239,	194,239,	194,239,	194,239,	
253,294,	195,238,	197,238,	190,241,	
195,239,	195,240,	195,239,	195,239,	
195,239,	195,239,	195,239,	195,239,	
195,239,	195,239,	191,242,	197,238,	
198,238,	254,295,	197,239,	197,240,	
197,239,	197,239,	197,239,	197,239,	
197,239,	197,239,	197,239,	197,239,	
207,207,	198,238,	233,233,	236,236,	
198,239,	198,240,	198,239,	198,239,	
198,239,	198,239,	198,239,	198,239,	
198,239,	198,239,	193,244,	202,202,	
237,237,	255,296,	257,297,	258,298,	
259,299,	260,300,	262,301,	207,207,	
263,302,	233,233,	236,236,	264,303,	
269,304,	271,305,	273,306,	194,245,	
274,307,	275,308,	281,310,	283,198,	
284,311,	285,312,	202,202,	237,237,	
286,313,	197,246,	287,314,	288,315,	
289,316,	292,291,	293,318,	294,319,	
295,207,	195,198,	296,143,	297,320,	
298,143,	299,321,	202,250,	202,250,	
202,250,	202,250,	202,250,	202,250,	
202,250,	202,250,	202,250,	202,250,	
220,265,	220,265,	220,265,	220,265,	
220,265,	220,265,	220,265,	220,265,	
220,265,	220,265,	221,266,	221,266,	
221,266,	221,266,	221,266,	221,266,	
221,266,	221,266,	221,266,	221,266,	
222,267,	222,267,	222,267,	222,267,	
222,267,	222,267,	222,267,	222,267,	
222,267,	222,267,	231,276,	231,276,	
231,276,	231,276,	231,276,	231,276,	
231,276,	231,276,	231,276,	231,276,	
234,278,	234,278,	234,278,	234,278,	
234,278,	234,278,	234,278,	234,278,	
234,278,	234,278,	235,279,	238,239,	
238,240,	238,239,	238,239,	238,239,	
238,239,	238,239,	238,239,	238,239,	
238,239,	239,280,	276,276,	278,278,	
300,322,	279,279,	301,323,	303,324,	
304,223,	305,325,	306,326,	308,327,	
311,330,	235,279,	312,331,	313,198,	
250,290,	314,332,	315,198,	316,333,	
322,336,	324,337,	327,338,	333,339,	
239,280,	276,276,	278,278,	277,276,	
279,279,	235,279,	235,279,	235,279,	
235,279,	235,279,	235,279,	235,279,	
235,279,	235,279,	235,279,	250,290,	
239,280,	239,280,	239,280,	239,280,	
239,280,	239,280,	239,280,	239,280,	
239,280,	239,280,	277,276,	250,291,	
335,341,	280,280,	282,280,	250,292,	
250,292,	250,292,	250,292,	250,292,	
250,292,	250,292,	250,292,	250,292,	
250,292,	290,290,	277,309,	277,309,	
277,309,	277,309,	277,309,	277,309,	
277,309,	277,309,	277,309,	277,309,	
280,280,	282,280,	291,317,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
290,290,	239,281,	310,329,	317,317,	
328,328,	340,340,	282,239,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	291,317,	309,328,	309,328,	
309,328,	309,328,	309,328,	309,328,	
309,328,	309,328,	309,328,	309,328,	
0,0,	310,329,	317,317,	328,328,	
340,340,	334,340,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	341,340,	0,0,	
0,0,	0,0,	317,334,	317,335,	
317,334,	317,334,	317,334,	317,334,	
317,334,	317,334,	317,334,	317,334,	
334,340,	280,281,	282,281,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	341,340,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
334,340,	334,340,	334,340,	334,340,	
334,340,	334,340,	334,340,	334,340,	
334,340,	334,340,	341,334,	0,0,	
0,0};
struct yysvf yysvec[] ={
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+0,	yysvec+1,	0,	
yycrank+27,	0,		0,	
yycrank+0,	yysvec+3,	0,	
yycrank+29,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+47,	0,		0,	
yycrank+120,	0,		0,	
yycrank+37,	yysvec+8,	0,	
yycrank+38,	0,		0,	
yycrank+50,	0,		0,	
yycrank+78,	0,		0,	
yycrank+63,	0,		0,	
yycrank+84,	0,		0,	
yycrank+42,	0,		0,	
yycrank+55,	0,		0,	
yycrank+85,	0,		0,	
yycrank+79,	0,		0,	
yycrank+56,	0,		0,	
yycrank+64,	0,		yyvstop+5,
yycrank+51,	0,		0,	
yycrank+139,	0,		0,	
yycrank+89,	0,		yyvstop+7,
yycrank+43,	0,		yyvstop+9,
yycrank+84,	0,		yyvstop+11,
yycrank+0,	yysvec+11,	yyvstop+13,
yycrank+85,	0,		yyvstop+15,
yycrank+0,	yysvec+12,	yyvstop+17,
yycrank+50,	0,		yyvstop+19,
yycrank+90,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+94,	0,		0,	
yycrank+0,	0,		yyvstop+25,
yycrank+110,	0,		yyvstop+27,
yycrank+82,	0,		yyvstop+29,
yycrank+0,	yysvec+16,	yyvstop+31,
yycrank+98,	0,		yyvstop+33,
yycrank+0,	0,		yyvstop+35,
yycrank+0,	yysvec+17,	yyvstop+37,
yycrank+0,	yysvec+18,	yyvstop+39,
yycrank+45,	0,		yyvstop+41,
yycrank+0,	yysvec+19,	yyvstop+43,
yycrank+103,	0,		yyvstop+45,
yycrank+50,	0,		0,	
yycrank+97,	0,		0,	
yycrank+115,	0,		0,	
yycrank+110,	0,		0,	
yycrank+114,	0,		0,	
yycrank+144,	0,		0,	
yycrank+213,	0,		0,	
yycrank+223,	yysvec+8,	0,	
yycrank+233,	0,		0,	
yycrank+107,	0,		0,	
yycrank+115,	0,		0,	
yycrank+122,	0,		0,	
yycrank+194,	0,		0,	
yycrank+131,	0,		0,	
yycrank+122,	0,		0,	
yycrank+135,	0,		0,	
yycrank+138,	0,		0,	
yycrank+260,	yysvec+49,	0,	
yycrank+133,	0,		0,	
yycrank+149,	0,		0,	
yycrank+158,	0,		0,	
yycrank+160,	0,		0,	
yycrank+188,	0,		0,	
yycrank+184,	0,		0,	
yycrank+187,	0,		0,	
yycrank+182,	0,		0,	
yycrank+188,	0,		0,	
yycrank+181,	0,		0,	
yycrank+184,	0,		0,	
yycrank+185,	0,		0,	
yycrank+190,	0,		0,	
yycrank+194,	0,		0,	
yycrank+189,	0,		0,	
yycrank+207,	0,		0,	
yycrank+209,	0,		0,	
yycrank+253,	0,		0,	
yycrank+271,	0,		0,	
yycrank+281,	0,		0,	
yycrank+239,	0,		0,	
yycrank+0,	yysvec+24,	0,	
yycrank+0,	yysvec+25,	0,	
yycrank+0,	yysvec+27,	0,	
yycrank+0,	yysvec+29,	0,	
yycrank+0,	yysvec+30,	0,	
yycrank+240,	0,		0,	
yycrank+241,	0,		0,	
yycrank+197,	0,		0,	
yycrank+0,	yysvec+37,	0,	
yycrank+0,	yysvec+41,	0,	
yycrank+0,	yysvec+43,	0,	
yycrank+197,	0,		0,	
yycrank+198,	0,		0,	
yycrank+199,	0,		0,	
yycrank+200,	0,		0,	
yycrank+201,	0,		0,	
yycrank+226,	0,		0,	
yycrank+227,	0,		0,	
yycrank+228,	0,		0,	
yycrank+229,	0,		0,	
yycrank+230,	0,		0,	
yycrank+231,	0,		0,	
yycrank+232,	0,		0,	
yycrank+233,	0,		0,	
yycrank+234,	0,		0,	
yycrank+235,	0,		0,	
yycrank+236,	0,		0,	
yycrank+237,	0,		0,	
yycrank+0,	0,		yyvstop+47,
yycrank+241,	0,		0,	
yycrank+242,	0,		0,	
yycrank+254,	0,		0,	
yycrank+250,	0,		0,	
yycrank+245,	0,		0,	
yycrank+252,	0,		0,	
yycrank+246,	0,		0,	
yycrank+263,	0,		0,	
yycrank+265,	0,		0,	
yycrank+319,	0,		0,	
yycrank+329,	0,		0,	
yycrank+378,	0,		yyvstop+49,
yycrank+274,	0,		0,	
yycrank+286,	0,		0,	
yycrank+291,	0,		0,	
yycrank+293,	0,		0,	
yycrank+282,	0,		0,	
yycrank+285,	0,		0,	
yycrank+280,	0,		0,	
yycrank+278,	0,		0,	
yycrank+281,	0,		0,	
yycrank+286,	0,		0,	
yycrank+390,	0,		0,	
yycrank+391,	0,		0,	
yycrank+301,	yysvec+135,	0,	
yycrank+405,	0,		0,	
yycrank+406,	0,		yyvstop+51,
yycrank+407,	0,		0,	
yycrank+408,	0,		0,	
yycrank+302,	yysvec+135,	0,	
yycrank+409,	0,		0,	
yycrank+410,	0,		0,	
yycrank+304,	yysvec+138,	yyvstop+53,
yycrank+304,	yysvec+135,	0,	
yycrank+295,	yysvec+143,	0,	
yycrank+411,	0,		yyvstop+55,
yycrank+291,	yysvec+143,	0,	
yycrank+308,	yysvec+138,	yyvstop+57,
yycrank+412,	0,		yyvstop+59,
yycrank+415,	0,		yyvstop+61,
yycrank+436,	0,		yyvstop+63,
yycrank+397,	0,		0,	
yycrank+448,	0,		0,	
yycrank+461,	0,		0,	
yycrank+0,	0,		yyvstop+65,
yycrank+0,	0,		yyvstop+67,
yycrank+0,	0,		yyvstop+69,
yycrank+0,	0,		yyvstop+71,
yycrank+0,	0,		yyvstop+73,
yycrank+0,	0,		yyvstop+75,
yycrank+0,	0,		yyvstop+77,
yycrank+0,	0,		yyvstop+79,
yycrank+0,	0,		yyvstop+81,
yycrank+0,	0,		yyvstop+83,
yycrank+0,	0,		yyvstop+85,
yycrank+0,	0,		yyvstop+87,
yycrank+0,	0,		yyvstop+89,
yycrank+0,	0,		yyvstop+91,
yycrank+0,	0,		yyvstop+93,
yycrank+0,	0,		yyvstop+95,
yycrank+0,	0,		yyvstop+97,
yycrank+0,	0,		yyvstop+99,
yycrank+0,	0,		yyvstop+101,
yycrank+370,	0,		0,	
yycrank+309,	yysvec+175,	0,	
yycrank+371,	0,		0,	
yycrank+313,	yysvec+175,	0,	
yycrank+407,	0,		0,	
yycrank+405,	0,		0,	
yycrank+384,	0,		0,	
yycrank+483,	0,		0,	
yycrank+402,	0,		0,	
yycrank+513,	0,		yyvstop+103,
yycrank+475,	0,		yyvstop+105,
yycrank+399,	yysvec+185,	yyvstop+107,
yycrank+499,	0,		0,	
yycrank+349,	0,		0,	
yycrank+351,	0,		0,	
yycrank+526,	0,		0,	
yycrank+541,	0,		0,	
yycrank+360,	yysvec+191,	0,	
yycrank+556,	0,		0,	
yycrank+570,	0,		0,	
yycrank+584,	0,		0,	
yycrank+361,	yysvec+191,	0,	
yycrank+598,	0,		0,	
yycrank+612,	0,		0,	
yycrank+362,	yysvec+191,	0,	
yycrank+353,	yysvec+198,	0,	
yycrank+349,	yysvec+198,	0,	
yycrank+662,	0,		0,	
yycrank+358,	0,		0,	
yycrank+352,	0,		0,	
yycrank+360,	0,		0,	
yycrank+354,	0,		0,	
yycrank+647,	0,		yyvstop+109,
yycrank+375,	0,		0,	
yycrank+376,	0,		0,	
yycrank+370,	0,		0,	
yycrank+379,	0,		0,	
yycrank+368,	0,		0,	
yycrank+380,	0,		0,	
yycrank+365,	0,		0,	
yycrank+380,	0,		0,	
yycrank+385,	0,		0,	
yycrank+368,	0,		0,	
yycrank+385,	0,		0,	
yycrank+385,	0,		0,	
yycrank+672,	0,		0,	
yycrank+682,	0,		0,	
yycrank+692,	0,		0,	
yycrank+443,	0,		yyvstop+111,
yycrank+392,	0,		0,	
yycrank+394,	0,		0,	
yycrank+379,	0,		0,	
yycrank+426,	0,		0,	
yycrank+410,	0,		0,	
yycrank+427,	0,		0,	
yycrank+440,	0,		0,	
yycrank+702,	0,		0,	
yycrank+485,	yysvec+231,	0,	
yycrank+649,	0,		yyvstop+113,
yycrank+712,	0,		0,	
yycrank+761,	0,		yyvstop+115,
yycrank+650,	0,		yyvstop+117,
yycrank+663,	0,		yyvstop+119,
yycrank+723,	0,		0,	
yycrank+772,	0,		yyvstop+121,
yycrank+486,	yysvec+239,	yyvstop+123,
yycrank+436,	0,		0,	
yycrank+442,	0,		0,	
yycrank+450,	0,		0,	
yycrank+443,	0,		0,	
yycrank+487,	0,		0,	
yycrank+481,	0,		0,	
yycrank+490,	0,		0,	
yycrank+502,	0,		0,	
yycrank+502,	0,		0,	
yycrank+787,	0,		yyvstop+125,
yycrank+498,	0,		0,	
yycrank+519,	0,		0,	
yycrank+531,	0,		0,	
yycrank+524,	0,		0,	
yycrank+559,	0,		0,	
yycrank+0,	yysvec+254,	0,	
yycrank+576,	0,		0,	
yycrank+574,	0,		0,	
yycrank+576,	0,		0,	
yycrank+568,	0,		0,	
yycrank+0,	yysvec+254,	0,	
yycrank+578,	0,		0,	
yycrank+583,	0,		0,	
yycrank+568,	0,		0,	
yycrank+0,	0,		yyvstop+127,
yycrank+0,	0,		yyvstop+129,
yycrank+0,	0,		yyvstop+131,
yycrank+0,	0,		yyvstop+133,
yycrank+563,	0,		0,	
yycrank+0,	yysvec+269,	0,	
yycrank+585,	0,		0,	
yycrank+0,	yysvec+269,	0,	
yycrank+586,	0,		0,	
yycrank+591,	0,		0,	
yycrank+574,	0,		0,	
yycrank+773,	0,		yyvstop+135,
yycrank+798,	0,		yyvstop+137,
yycrank+774,	0,		yyvstop+139,
yycrank+776,	0,		yyvstop+141,
yycrank+824,	0,		yyvstop+143,
yycrank+574,	0,		0,	
yycrank+825,	yysvec+238,	yyvstop+145,
yycrank+575,	0,		0,	
yycrank+594,	0,		0,	
yycrank+596,	0,		0,	
yycrank+582,	0,		0,	
yycrank+600,	0,		0,	
yycrank+598,	0,		0,	
yycrank+591,	0,		0,	
yycrank+836,	0,		yyvstop+147,
yycrank+849,	0,		0,	
yycrank+657,	yysvec+290,	yyvstop+149,
yycrank+601,	0,		0,	
yycrank+589,	0,		0,	
yycrank+660,	yysvec+207,	yyvstop+151,
yycrank+585,	0,		0,	
yycrank+606,	0,		0,	
yycrank+594,	0,		0,	
yycrank+612,	0,		0,	
yycrank+686,	0,		0,	
yycrank+689,	0,		0,	
yycrank+0,	yysvec+254,	0,	
yycrank+687,	0,		0,	
yycrank+747,	0,		0,	
yycrank+692,	0,		0,	
yycrank+693,	0,		0,	
yycrank+0,	yysvec+269,	0,	
yycrank+691,	0,		0,	
yycrank+834,	0,		0,	
yycrank+861,	0,		0,	
yycrank+691,	0,		0,	
yycrank+680,	0,		0,	
yycrank+674,	0,		0,	
yycrank+696,	0,		0,	
yycrank+684,	0,		0,	
yycrank+701,	0,		0,	
yycrank+862,	0,		0,	
yycrank+0,	yysvec+298,	0,	
yycrank+0,	yysvec+296,	0,	
yycrank+0,	yysvec+298,	0,	
yycrank+0,	yysvec+254,	0,	
yycrank+699,	0,		0,	
yycrank+0,	yysvec+254,	0,	
yycrank+704,	0,		0,	
yycrank+0,	yysvec+269,	0,	
yycrank+0,	yysvec+269,	0,	
yycrank+705,	0,		0,	
yycrank+863,	0,		yyvstop+153,
yycrank+0,	yysvec+310,	yyvstop+155,
yycrank+0,	yysvec+315,	0,	
yycrank+0,	yysvec+313,	0,	
yycrank+0,	yysvec+315,	0,	
yycrank+702,	0,		0,	
yycrank+888,	0,		yyvstop+157,
yycrank+775,	yysvec+334,	yyvstop+159,
yycrank+0,	yysvec+298,	0,	
yycrank+0,	yysvec+254,	0,	
yycrank+0,	yysvec+269,	0,	
yycrank+0,	yysvec+315,	0,	
yycrank+864,	0,		yyvstop+161,
yycrank+897,	yysvec+317,	yyvstop+163,
0,	0,	0};
struct yywork *yytop = yycrank+946;
struct yysvf *yybgin = yysvec+1;
char yymatch[] ={
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,'-' ,01  ,01  ,
'0' ,'0' ,'2' ,'3' ,'3' ,'3' ,'6' ,'6' ,
'6' ,'6' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,
'a' ,'a' ,01  ,'k' ,'k' ,'k' ,'n' ,'n' ,
'n' ,'n' ,'n' ,'n' ,'n' ,'n' ,'n' ,'n' ,
'n' ,'n' ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] ={
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	ncform	4.1	83/08/11	*/

