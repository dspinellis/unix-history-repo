%e 2000
%p 5000
%n 1000
%a 4000
%START	Z
sun	(sun(day)?)
mon	(mon(day)?)
tue	(tue(sday)?)
wed	(wed(nesday)?)
thu	(thu(rsday)?)
fri	(fri(day)?)
sat	(sat(urday)?)

DAY	({sun}|{mon}|{tue}|{wed}|{thu}|{fri}|{sat})

jan	(jan(uary)?)
feb	(feb(ruary)?)
mar	(mar(ch)?)
apr	(apr(il)?)
may	(may)
jun	(jun(e)?)
jul	(jul(y)?)
aug	(aug(est)?)
sep	(sep(tember)?)
oct	(oct(ober)?)
nov	(nov(ember)?)
dec	(dec(ember)?)

MONTH	({jan}|{feb}|{mar}|{apr}|{may}|{jun}|{jul}|{aug}|{sep}|{oct}|{nov}|{dec})

w	([ \t]*)
W	([ \t]+)
D	([0-9]?[0-9])
d	[0-9]
%{
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
%}
%%
%{
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

%}
{DAY}","?{w}				SETDAY;
"("{DAY}")"(","?)			cp++, SETDAY;

{D}"/"{D}"/"19[0-9][0-9]{w}		{
					tw.tw_mday = CVT2; cp++;
					tw.tw_mon  = CVT2 - 1; cp += 3;
					tw.tw_year = CVT2;
					gotdate++;
					}
{D}"/"{D}"/"[0-9][0-9]{w}		{
					tw.tw_mday = CVT2; cp++;
					tw.tw_mon  = CVT2 - 1; cp++;
					tw.tw_year = CVT2;
					gotdate++;
					}
{D}[- ]?{MONTH}[- ]?(19)?{D}{w}(at{W})?	{
					tw.tw_mday = CVT2;
					while ( !isalpha(*cp++) )
						;
					SETMONTH;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i % 100;
					}
{MONTH}{W}{D}","{W}(19)?{D}{w}		{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT2;
					SKIPD;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i % 100;
					}

{MONTH}{W}{D}{w}			{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT2;
					}

{D}:{D}:{D}{w}				{
					tw.tw_hour = CVT2; cp++;
					tw.tw_min  = CVT2; cp++;
					tw.tw_sec  = CVT2;
					BEGIN Z;
					}
{D}:{D}{w}				|
{D}:{D}{w}am{w}				{
					tw.tw_hour = CVT2; cp++;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
{D}:{D}{w}pm{w}				{
					tw.tw_hour = CVT2 + 12; cp++;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
[0-2]{d}{d}{d}{d}{d}{w}			{
					tw.tw_hour = CVT2;
					tw.tw_min  = CVT2;
					tw.tw_sec  = CVT2;
					BEGIN Z;
					}
[0-2]{d}{d}{d}{w}			{
					tw.tw_hour = CVT2;
					tw.tw_min  = CVT2;
					BEGIN Z;
					}
<Z>"-"?ut				ZONE(0 * 60);
<Z>"-"?gmt				ZONE(0 * 60);
<Z>"-"?jst				ZONE(2 * 60);
<Z>"-"?jdt				ZONED(2 * 60);
<Z>"-"?est				ZONE(-5 * 60);
<Z>"-"?edt				ZONED(-5 * 60);
<Z>"-"?cst				ZONE(-6 * 60);
<Z>"-"?cdt				ZONED(-6 * 60);
<Z>"-"?mst				ZONE(-7 * 60);
<Z>"-"?mdt				ZONED(-7 * 60);
<Z>"-"?pst				ZONE(-8 * 60);
<Z>"-"?pdt				ZONED(-8 * 60);
<Z>"-"?nst				ZONE(-(3 * 60 + 30));
<Z>"-"?ast				ZONE(-4 * 60);
<Z>"-"?adt				ZONED(-4 * 60);
<Z>"-"?yst				ZONE(-9 * 60);
<Z>"-"?ydt				ZONED(-9 * 60);
<Z>"-"?hst				ZONE(-10 * 60);
<Z>"-"?hdt				ZONED(-10 * 60);
<Z>"-"?bst				ZONED(-1 * 60);
<Z>[a-i]				tw.tw_zone = 60 * (('a'-1) - LC(*cp));
<Z>[k-m]				tw.tw_zone = 60 * ('a' - LC(*cp));
<Z>[n-y]				tw.tw_zone = 60 * (LC(*cp) - 'm');
<Z>"+"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = ((cp[0] * 10 + cp[1])
						     -('0' * 10   + '0'))*60
						    +((cp[2] * 10 + cp[3])
						     -('0' * 10   + '0'));
					cp += 4;
					}
<Z>"-"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = (('0' * 10   + '0')
						     -(cp[0] * 10 + cp[1]))*60
						    +(('0' * 10   + '0')
						     -(cp[2] * 10 + cp[3]));
					cp += 4;
					}
<Z>{W}19[6-9]{d}			{
					while( !isdigit(*cp++) )
						;
					cp++;
					tw.tw_year = CVT2;
					}

\n	|
{W}	;
%%
