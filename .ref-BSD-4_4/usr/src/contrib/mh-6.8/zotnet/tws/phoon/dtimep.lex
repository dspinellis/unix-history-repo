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
aug	(aug(ust)?)
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
/* dtimep.lex - routines to do ``ARPA-style'' time parsing

ver  date   who remarks
--- ------- --- -------------------------------------------------------------
01B 15nov86 JP  Thouroughly hacked by Jef Poskanzer.
01A ??????? MTR Original version from the MH 6.5 distribution, courtesy
	          of Marshall Rose.

*/

#include "tws.h"
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef SYS5
#include <string.h>
#else SYS5
#include <strings.h>
#include <sys/timeb.h>
#endif SYS5

#ifdef SYS5
extern int  daylight;
extern long timezone;
extern char *tzname[];
#endif SYS5

/*
 * Table to convert month names to numeric month.  We use the
 * fact that the low order 5 bits of the sum of the 2nd & 3rd
 * characters of the name is a hash with no collisions for the 12
 * valid month names.  (The mask to 5 bits maps any combination of
 * upper and lower case into the same hash value).
 */
static int month_map[] = {
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
	7 };	/*28 - Aug */
/*
 * Same trick for day-of-week using the hash function
 *  (c1 & 7) + (c2 & 4)
 */
static int day_map[] = {
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
	3 };	/*11 - Wed */
#define SETDAY tw.tw_wday= day_map[(cp[0] & 7) + (cp[1] & 4)];\
		tw.tw_flags |= TW_SEXP;\
		cp += 2;
#define SETMONTH tw.tw_mon = month_map[(cp[0] + cp[1]) & 0x1f]; gotdate++;\
		 cp += 2;\
		 SKIPD;
#define CVT1OR2 (i=(*cp++ - '0'), isdigit(*cp)? i*10 + (*cp++ - '0') : i)
#define CVT2 ( (*cp++ - '0')*10 + (*cp++ - '0') )
#define CVT3 ( ( (*cp++ - '0')*10 + (*cp++ - '0') )*10 + (*cp++ - '0') )
#define CVT4 ( ( ( (*cp++ - '0')*10 + (*cp++ - '0') )*10 + (*cp++ - '0') )*10 + (*cp++ - '0') )
#define SKIPD while ( ! isdigit( *cp++ ) ) ; --cp;
#define ZONE(x) tw.tw_zone=(x);
#define ZONED(x) tw.tw_zone=(x); tw.tw_flags |= TW_DST;
#define LC(c) (isupper( c ) ? tolower( c ) : ( c ))
%}
%%
%{
struct tws *
dparsetime( str )
char *str;
    {
    register int i;
    static struct tws tw;
    register char *cp;
    register int gotdate = 0;
#ifndef SYS5
    struct timeb	tb;
#endif not SYS5
    long clock;

    start_cond = 0;

    /* Zero out the struct. */
    bzero( (char *) &tw, sizeof tw );

    /* Set default time zone. */
#ifndef SYS5
    ftime( &tb );
    tw.tw_zone = -tb.timezone;
#else SYS5
    tzset( );
    tw.tw_zone = -(timezone / 60);
#endif SYS5

    for ( ; ; )
	switch ( cp = str, lex_string( &str, start_cond ) )
	    {
	    case -1:
		if ( ! gotdate )
			return ( NULL );
		tw.tw_flags |= TW_JUNK;
		/* fall through */
	    case 0:
		if ( tw.tw_year == 0 )
		    {
		    /* Set default year. */
		    time( &clock );
		    tw.tw_year = localtime( &clock ) -> tm_year;
		    }
		return ( &tw );

%}
{DAY}","?{w}				SETDAY;
"("{DAY}")"(","?)			cp++, SETDAY;

{D}"/"{D}"/"{D}?{d}{d}{w}		{
#ifdef EUROPE
					tw.tw_mday = CVT1OR2; cp++;
					tw.tw_mon  = CVT1OR2 - 1; cp++;
#else EUROPE
					tw.tw_mon = CVT1OR2 - 1; cp++;
					tw.tw_mday  = CVT1OR2; cp++;
#endif EUROPE
					for ( i = 0; isdigit( *cp ); )
						i = i * 10 + (*cp++ - '0');
					tw.tw_year = i;
					gotdate++;
					}
{D}"/"{D}{w}				{
#ifdef EUROPE
					tw.tw_mday = CVT1OR2; cp++;
					tw.tw_mon  = CVT1OR2 - 1;
#else EUROPE
					tw.tw_mon = CVT1OR2 - 1; cp++;
					tw.tw_mday  = CVT1OR2;
#endif EUROPE
					gotdate++;
					}
{D}"-"?{MONTH}"-"?{D}?{d}{d}({W}at)?{w}	|
{D}" "{MONTH}" "{D}?{d}{d}({W}at)?{w}	{
					tw.tw_mday = CVT1OR2;
					while ( ! isalpha( *cp++ ) )
						;
					SETMONTH;
					for ( i = 0; isdigit( *cp ); )
						i = i * 10 + (*cp++ - '0');
					tw.tw_year = i;
					gotdate++;
					}
{D}"-"?{MONTH}({W}at)?{w}		{
					tw.tw_mday = CVT1OR2;
					while ( ! isalpha( *cp++ ) )
						;
					SETMONTH;
					gotdate++;
					}
{MONTH}{W}{D}","{W}{D}?{d}{d}{w}	{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT1OR2;
					SKIPD;
					for ( i = 0; isdigit( *cp ); )
						i = i * 10 + (*cp++ - '0');
					tw.tw_year = i;
					gotdate++;
					}
{MONTH}{W}{D}{w}			{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT1OR2;
					gotdate++;
					}

{D}:{D}:{D}({w}am)?{w}			{
					tw.tw_hour = CVT1OR2; cp++;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}:{D}{w}pm{w}			{
					tw.tw_hour = CVT1OR2 + 12; cp++;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}({w}am)?{w}			{
					tw.tw_hour = CVT1OR2; cp++;
					tw.tw_min  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}{w}pm{w}				{
					tw.tw_hour = CVT1OR2 + 12; cp++;
					tw.tw_min  = CVT1OR2;
					BEGIN Z;
					}
[0-2]{d}{d}{d}{d}{d}{w}			{
					tw.tw_hour = CVT1OR2;
					tw.tw_min  = CVT1OR2;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
[0-2]{d}{d}{d}{w}			{
					tw.tw_hour = CVT1OR2;
					tw.tw_min  = CVT1OR2;
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
<Z>[a-i]				tw.tw_zone = 60 * (('a'-1) - LC (*cp));
<Z>[k-m]				tw.tw_zone = 60 * ('a' - LC (*cp));
<Z>[n-y]				tw.tw_zone = 60 * (LC (*cp) - 'm');
<Z>"+"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = ((cp[0] * 10 + cp[1])
						     -('0' * 10   + '0'))*60
						    +((cp[2] * 10 + cp[3])
						     -('0' * 10   + '0'));
#ifdef DSTXXX
					zonehack (&tw);
#endif DSTXXX
					cp += 4;
					}
<Z>"-"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = (('0' * 10   + '0')
						     -(cp[0] * 10 + cp[1]))*60
						    +(('0' * 10   + '0')
						     -(cp[2] * 10 + cp[3]));
#ifdef DSTXXX
					zonehack (&tw);
#endif DSTXXX
					cp += 4;
					}

<Z>{W}{d}{d}{d}{d}			{
					SKIPD;
					tw.tw_year = CVT4;
					}
\n	|
{W}	;
%%

#ifdef DSTXXX
static
zonehack( tw )
register struct tws *tw;
    {
    register struct tm *tm;

    if ( twclock( tw ) == -1L )
	return;

    tm = localtime( &tw -> tw_clock );
    if ( tm -> tm_isdst )
	{
	tw -> tw_flags |= TW_DST;
	tw -> tw_zone -= 60;
	}
    }
#endif DSTXXX
