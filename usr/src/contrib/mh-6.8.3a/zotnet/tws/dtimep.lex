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
#ifndef	lint
static char ident[] = "@(#)$Id: dtimep.lex,v 2.15 1993/02/26 22:07:40 jromine Exp $";
#endif
#include "tws.h"
#include "../h/strings.h"
#include <ctype.h>
#include <sys/types.h>
#if !defined(SYS5) && !defined(ZONEINFO)
#include <sys/timeb.h>
#endif /* !defined(SYS5) && !defined(ZONEINFO) */

#ifdef SYS5
extern int  daylight;
extern long timezone;
extern char *tzname[];
#endif /* SYS5 */

/*
 * Patchable flag that says how to interpret NN/NN/NN dates. When
 * true, we do it European style: DD/MM/YY. When false, we do it
 * American style: MM/DD/YY.  Of course, these are all non-RFC822
 * compliant.
 */
int europeandate = 0;

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
#define SETDAY	{ tw.tw_wday= day_map[(cp[0] & 7) + (cp[1] & 4)];\
		tw.tw_flags &= ~TW_SDAY; tw.tw_flags |= TW_SEXP;\
		cp += 2; }
#define SETMONTH { tw.tw_mon = month_map[(cp[0] + cp[1]) & 0x1f]; gotdate++;\
		 cp += 2;\
		 SKIPD;}
#define	CVT1OR2	(i=(*cp++ - '0'), isdigit(*cp)? i*10 + (*cp++ - '0') : i)
#define	CVT2	  ((cp[0] - '0')*10 + (cp[1] - '0'))
#define	CVT4	((((cp[0] - '0')*10 + (cp[1] - '0'))*10 + \
				      (cp[2] - '0'))*10 + (cp[3] - '0'))
#define SKIPD	{ while ( !isdigit(*cp++) ) ;  --cp; }
#define EXPZONE	{ tw.tw_flags &= ~TW_SZONE; tw.tw_flags |= TW_SZEXP; }
#define ZONE(x)	{ tw.tw_zone=(x); EXPZONE; }
#define ZONED(x) { ZONE(x); tw.tw_flags |= TW_DST; }
#define	LC(c)	(isupper (c) ? tolower (c) : (c))

#ifdef	DSTXXX
#ifdef	_AIX
#include <sys/time.h>
#include <time.h>
#else
#ifndef	BSD42
#include <time.h>
#else	/* BSD42 */
#include <sys/time.h>
#endif	/* BSD42 */
#endif

static	zonehack (tw)
register struct tws *tw;
{
    register struct tm *tm;

    if (twclock (tw) == -1L)
	return;

    tm = localtime (&tw -> tw_clock);
    if (tm -> tm_isdst) {
	tw -> tw_flags |= TW_DST;
	tw -> tw_zone -= 60;
    }
}
#endif	/* DSTXXX */
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
#ifndef SYS5
#ifdef ZONEINFO
	struct tm      *tm;
	time_t		clock;
#else
	struct timeb	tb;
#endif	/* ZONEINFO */
#endif	/* not SYS5 */
	long tclock;

	start_cond = 0;

	/* Zero out the struct. */
	bzero( (char *) &tw, sizeof tw);

	/* Set default time zone. */
#ifdef SYS5
	tzset( );
	tw.tw_zone = -(timezone / 60);
#else
#ifdef ZONEINFO
	time (&clock);
	tm = localtime(&clock);
	tw.tw_zone = tm->tm_gmtoff / 60;
	if (tm -> tm_isdst)			/* if DST is in effect */
		tw.tw_zone -= 60;		/* reset to normal offset */
#else
	ftime( &tb );
	tw.tw_zone = -tb.timezone;
#endif	/* ZONEINFO */
#endif	/* SYS5 */

	while (isspace(*str))
		str++;
	while ( 1 )
		switch (cp = str, *cp ? lex_string( &str, start_cond) : 0) {

		case -1:
			if (!gotdate || tw.tw_year == 0)
				return (struct tws *)0;
			/* fall through */
		case 0:
			if ( tw.tw_year == 0 ) {
				/* Set default year. */
				time (&tclock);
				tw.tw_year = localtime(&tclock)->tm_year + 1900;
			}
			else if (tw.tw_year < 100) {
				/* assume no 2-digit years > 1999 */
				tw.tw_year += 1900;
			}
			return &tw;

%}
{DAY}","?{w}				SETDAY;
"("{DAY}")"(","?)			{
					cp++;
					SETDAY;
					}
{D}(("-"{D}"-")|("/"{D}"/")){D}?{d}{d}{w}	{
					if (europeandate) {
						/* European: DD/MM/YY */
						tw.tw_mday = CVT1OR2;
						cp++;
						tw.tw_mon  = CVT1OR2 - 1;
					} else {
						/* American: MM/DD/YY */
						tw.tw_mon  = CVT1OR2 - 1;
						cp++;
						tw.tw_mday = CVT1OR2;
					}
					cp++;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i;
					gotdate++;	/* XXX */
					}
{D}("/"|"-"){D}{w}			{
					if (europeandate) {
						tw.tw_mday = CVT1OR2; cp++;
						tw.tw_mon  = CVT1OR2 - 1;
					} else {
						tw.tw_mon = CVT1OR2 - 1; cp++;
						tw.tw_mday  = CVT1OR2;
					}
					gotdate++;
					}
{D}{w}(-)?{w}{MONTH}{w}(-)?{w}{D}?{d}{d}({W}at)?{w}	{
					tw.tw_mday = CVT1OR2;
					while ( !isalpha(*cp++) )
						;
					SETMONTH;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i;
					}
{D}"-"?{MONTH}({W}at)?{w}               {
                                        tw.tw_mday = CVT1OR2;
                                        while ( ! isalpha( *cp++ ) )
                                                ;
                                        SETMONTH;
                                        }
{MONTH}{W}{D}","{W}{D}?{d}{d}{w}	{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT1OR2;
					SKIPD;
					for (i = 0; isdigit(*cp); )
						i = i*10 + (*cp++ - '0');
					tw.tw_year = i;
					}
{MONTH}{W}{D}{w}			{
					cp++;
					SETMONTH;
					tw.tw_mday = CVT1OR2;
					}

{D}:{D}:{D}{W}19[6-9]{d}		{	/* hack: ctime w/o TZ */
					tw.tw_hour = CVT1OR2; cp++;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					SKIPD;
					tw.tw_year = CVT4; cp+=4;
					}
{D}:{D}:{D}{w}				{
					tw.tw_hour = CVT1OR2; cp++;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}{w}				{
					tw.tw_hour = CVT1OR2; cp++;
					tw.tw_min = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}{w}am{w}				{
					tw.tw_hour = CVT1OR2; cp++;
					if (tw.tw_hour == 12)
						tw.tw_hour = 0;
					tw.tw_min  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}:{D}{w}am{w}			{
					tw.tw_hour = CVT1OR2; cp++;
					if (tw.tw_hour == 12)
						tw.tw_hour = 0;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}{w}pm{w}				{
					tw.tw_hour = CVT1OR2; cp++;
					if (tw.tw_hour != 12)
						tw.tw_hour += 12;
					tw.tw_min  = CVT1OR2;
					BEGIN Z;
					}
{D}:{D}:{D}{w}pm{w}			{
					tw.tw_hour = CVT1OR2; cp++;
					if (tw.tw_hour != 12)
						tw.tw_hour += 12;
					tw.tw_min  = CVT1OR2; cp++;
					tw.tw_sec  = CVT1OR2;
					BEGIN Z;
					}
[0-2]{d}{d}{d}{d}{d}{w}			{
					tw.tw_hour = CVT2; cp+=2;
					tw.tw_min  = CVT2; cp+=2;
					tw.tw_sec  = CVT2; cp+=2;
					BEGIN Z;
					}
19[6-9]{d}{w}				{
					/*
					 * Luckly, 4 digit times in the range
					 * 1960-1999 aren't legal as hour
					 * and minutes.
					 */
					tw.tw_year = CVT4; cp+=4;
					}
[0-2]{d}{d}{d}{w}			{
					if (tw.tw_hour || tw.tw_min 
							    || tw.tw_sec) {
					    tw.tw_year = CVT4; cp+=4;
					    tw.tw_zone = 0;
					} else {
					    tw.tw_hour = CVT2; cp+=2;
					    tw.tw_min  = CVT2; cp+=2;
					    BEGIN Z;
					}
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
<Z>[a-i]				{
					tw.tw_zone = 60 * (('a'-1) - LC(*cp));
					EXPZONE; 
					}
<Z>[k-m]				{
					tw.tw_zone = 60 * ('a' - LC(*cp));
					EXPZONE; 
					}
<Z>[n-y]				{
					tw.tw_zone = 60 * (LC(*cp) - 'm');
					EXPZONE; 
					}
<Z>"+"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = ((cp[0] * 10 + cp[1])
						     -('0' * 10   + '0'))*60
						    +((cp[2] * 10 + cp[3])
						     -('0' * 10   + '0'));
					EXPZONE;
#ifdef	DSTXXX
					zonehack (&tw);
#endif	/* DSTXXX */
					cp += 4;
					}
<Z>"-"[0-1]{d}{d}{d}			{
					cp++;
					tw.tw_zone = (('0' * 10   + '0')
						     -(cp[0] * 10 + cp[1]))*60
						    +(('0' * 10   + '0')
						     -(cp[2] * 10 + cp[3]));
					EXPZONE;
#ifdef	DSTXXX
					zonehack (&tw);
#endif	/* DSTXXX */
					cp += 4;
					}
<Z>{W}{d}{d}{d}{d}			{
					SKIPD;
					tw.tw_year = CVT4; cp+=4;
					}
\n	|
{W}	;
%%
