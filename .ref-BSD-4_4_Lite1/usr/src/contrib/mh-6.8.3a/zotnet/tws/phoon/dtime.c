/* dtime.c - routines to do ``ARPA-style'' time structures

ver  date   who remarks
--- ------- --- -------------------------------------------------------------
01B 15nov86 JP  Thouroughly hacked by Jef Poskanzer.
01A ??????? MTR Original version from the MH 6.5 distribution, courtesy
	          of Marshall Rose.

*/


#include "tws.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#ifdef  SYS5
#include <string.h>
#else SYS5
#include <strings.h>
#include <sys/timeb.h>
#endif SYS5

#ifdef	SYS5
extern int  daylight;
extern long timezone;
extern char *tzname[];
#endif	SYS5

/*  */

#define	abs(a) ( a >= 0 ? a : -a )

char *tw_moty[] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NULL };

char *tw_dotw[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", NULL };

char *tw_ldotw[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday", NULL };

/*  */

static struct zone
    {
    char *std;
    char *dst;
    int shift;
    }
    zones[] = {
	"GMT", "BST", 0,
	"EST", "EDT", -5,
	"CST", "CDT", -6,
	"MST", NULL, -7,
	"PST", "PDT", -8,
	"A", NULL, -1,
	"B", NULL, -2,
	"C", NULL, -3,
	"D", NULL, -4,
	"E", NULL, -5,
	"F", NULL, -6,
	"G", NULL, -7,
	"H", NULL, -8,
	"I", NULL, -9,
	"K", NULL, -10,
	"L", NULL, -11,
	"M", NULL, -12,
	"N", NULL, 1,
#ifndef	HUJI
	"O", NULL, 2,
#else	HUJI
	"JST", "JDT", 2,
#endif	HUJI
	"P", NULL, 3,
	"Q", NULL, 4,
	"R", NULL, 5,
	"S", NULL, 6,
	"T", NULL, 7,
	"U", NULL, 8,
	"V", NULL, 9,
	"W", NULL, 10,
	"X", NULL, 11,
	"Y", NULL, 12,
	NULL };

#define CENTURY 19

long time( );
struct tm *localtime( );

/*  */

char *dtimenow( )
    {
    long clock;

    (void) time( &clock );
    return ( dtime( &clock ) );
    }


char *
dctime( tw )
struct tws *tw;
    {
    static char buffer[25];

    if ( tw == NULL )
	return ( NULL );

    (void) sprintf( buffer, "%.3s %.3s %02d %02d:%02d:%02d %.4d\n",
	    tw_dotw[tw -> tw_wday], tw_moty[tw -> tw_mon], tw -> tw_mday,
	    tw -> tw_hour, tw -> tw_min, tw -> tw_sec,
	    tw -> tw_year >= 100 ? tw -> tw_year : 1900 + tw -> tw_year );

    return ( buffer );
    }

/*  */

struct tws *
dtwstime( )
    {
    long clock;

    (void) time( &clock );
    return ( dlocaltime( &clock ) );
    }


struct tws *
dlocaltime( clock )
long *clock;
    {
    register struct tm *tm;
#ifndef SYS5
    struct timeb tb;
#endif not SYS5
    static struct tws tw;

    if ( clock == NULL )
	return ( NULL );
    tw.tw_flags = TW_NULL;

    tm = localtime( clock );
    tw.tw_sec = tm -> tm_sec;
    tw.tw_min = tm -> tm_min;
    tw.tw_hour = tm -> tm_hour;
    tw.tw_mday = tm -> tm_mday;
    tw.tw_mon = tm -> tm_mon;
    tw.tw_year = tm -> tm_year;
    tw.tw_wday = tm -> tm_wday;
    tw.tw_yday = tm -> tm_yday;
    if ( tm -> tm_isdst )
	tw.tw_flags |= TW_DST;
#ifndef  SYS5
    ftime( &tb );
    tw.tw_zone = -tb.timezone;
#else   SYS5
    tzset( );
    tw.tw_zone = -(timezone / 60);
#endif  SYS5
    tw.tw_flags &= ~TW_SDAY;
    tw.tw_flags |= TW_SEXP;
    tw.tw_clock = *clock;

    return ( &tw );
    }


struct tws *
dgmtime( clock )
long *clock;
    {
    register struct tm *tm;
    static struct tws tw;

    if ( clock == NULL )
	return ( NULL );
    tw.tw_flags = TW_NULL;

    tm = gmtime( clock );
    tw.tw_sec = tm -> tm_sec;
    tw.tw_min = tm -> tm_min;
    tw.tw_hour = tm -> tm_hour;
    tw.tw_mday = tm -> tm_mday;
    tw.tw_mon = tm -> tm_mon;
    tw.tw_year = tm -> tm_year;
    tw.tw_wday = tm -> tm_wday;
    tw.tw_yday = tm -> tm_yday;
    if ( tm -> tm_isdst )
	tw.tw_flags |= TW_DST;
    tw.tw_zone = 0;
    tw.tw_flags &= ~TW_SDAY;
    tw.tw_flags |= TW_SEXP;
    tw.tw_clock = *clock;

    return( &tw );
    }

/*  */

char *
dasctime( tw, flags )
struct tws *tw;
int flags;
    {
    static char buffer[80], result[80];

    if ( tw == NULL )
	return ( NULL );

    (void) sprintf( buffer, "%02d %s %02d %02d:%02d:%02d %s",
	    tw -> tw_mday, tw_moty[tw -> tw_mon], tw -> tw_year,
	    tw -> tw_hour, tw -> tw_min, tw -> tw_sec,
	    dtimezone( tw -> tw_zone, tw -> tw_flags | flags ) );

    if ( (tw -> tw_flags & TW_SDAY) == TW_SEXP )
	(void) sprintf( result, "%s, %s", tw_dotw[tw -> tw_wday], buffer );
    else
	if ( (tw -> tw_flags & TW_SDAY) == TW_SNIL )
	    (void) strcpy( result, buffer );
	else
	    (void) sprintf( result, "%s (%s)", buffer, tw_dotw[tw -> tw_wday] );

    return ( result );
    }

/*  */

char *
dtimezone( offset, flags )
int offset, flags;
    {
    register int hours, mins;
    register struct zone *z;
    static char buffer[10];

    if ( offset < 0 )
	{
	mins = -((-offset) % 60);
	hours = -((-offset) / 60);
	}
    else
	{
	mins = offset % 60;
	hours = offset / 60;
	}

    if ( !(flags & TW_ZONE) && mins == 0 )
	for ( z = zones; z -> std; z++ )
	    if ( z -> shift == hours )
		return ( z -> dst && (flags & TW_DST) ? z -> dst : z -> std );

#ifdef	DSTXXX
    if ( flags & TW_DST )
	hours += 1;
#endif	DSTXXX
    (void) sprintf( buffer, "%s%02d%02d",
	    offset < 0 ? "-" : "+", abs( hours ), abs( mins ) );
    return ( buffer );
    }

/*  */

void
twscopy( tb, tw )
struct tws *tb, *tw;
    {
#ifdef	notdef
    tb -> tw_sec = tw -> tw_sec;
    tb -> tw_min = tw -> tw_min;
    tb -> tw_hour = tw -> tw_hour;
    tb -> tw_mday = tw -> tw_mday;
    tb -> tw_mon = tw -> tw_mon;
    tb -> tw_year = tw -> tw_year;
    tb -> tw_wday = tw -> tw_wday;
    tb -> tw_yday = tw -> tw_yday;
    tb -> tw_zone = tw -> tw_zone;
    tb -> tw_clock = tw -> tw_clock;
    tb -> tw_flags = tw -> tw_flags;
#else	not notdef
    *tb = *tw;
#endif	not notdef
    }


int
twsort( tw1, tw2 )
struct tws *tw1, *tw2;
    {
    register long c1, c2;

    (void) twclock( tw1 );
    (void) twclock( tw2 );

    return ( (c1 = tw1 -> tw_clock) > (c2 = tw2 -> tw_clock) ? 1
	    : c1 == c2 ? 0 : -1 );
    }

/*  */


/* Julian day number of the Unix clock's origin, 01 Jan 1970. */
#define JD1970 2440587L


long
twjuliandate( tw )
struct tws *tw;
    {
    register int mday, mon, year;
    register long a, b;
    double jd;

    if ( (mday = tw -> tw_mday) < 1 || mday > 31 ||
	    (mon = tw -> tw_mon + 1) < 1 || mon > 12 ||
	    (year = tw -> tw_year) < 1 || year > 10000 )
	return ( -1L );
    if ( year < 100 )
	year += CENTURY * 100;

    if ( mon == 1 || mon == 2 )
	{
	--year;
	mon += 12;
	}
    if ( year < 1583 )
	return ( -1L );
    a = year / 100;
    b = 2 - a + a / 4;
    b += (long) ( (double) year * 365.25 );
    b += (long) ( 30.6001 * ( (double) mon + 1.0 ) );
    jd = mday + b + 1720994.5;
    return ( (long) jd );
    }


long
twsubdayclock( tw )
struct tws *tw;
    {
    register int i, sec, min, hour;
    register long result;

    if ( (sec = tw -> tw_sec) < 0 || sec > 59 ||
	    (min = tw -> tw_min) < 0 || min > 59 ||
	    (hour = tw -> tw_hour) < 0 || hour > 23 )
	return ( -1L );

    result = ( hour * 60 + min ) * 60 + sec;
    result -= 60 * tw -> tw_zone;
    if ( tw -> tw_flags & TW_DST )
	result -= 60 * 60;

    return ( result );
    }


long
twclock( tw )
struct tws *tw;
    {
    register long jd, sdc, result;

    if ( tw -> tw_clock != 0L )
	return ( tw -> tw_clock );

    if ( ( jd = twjuliandate( tw ) ) == -1L )
	return ( tw -> tw_clock = -1L );
    if ( ( sdc = twsubdayclock( tw ) ) == -1L )
	return ( tw -> tw_clock = -1L );

    result = ( jd - JD1970 ) * 24 * 60 * 60 + sdc;

    return ( tw -> tw_clock = result );
    }

/*  */

/*** twsubtract - subtract tw2 from tw1, returning result in seconds

The point of this routine is that using twclock( tw1 ) - twclock( tw2 )
would limit you to dates after the Unix Epoch ( 01 January 1970 ).  This
routine avoids that limit.  However, because the result is represented
by 32 bits, it is still limited to a span of two billion seconds, which is
about 66 years.

*/

long
twsubtract( tw1, tw2 )
struct tws *tw1, *tw2;
    {
    register long jd1, jd2, sdc1, sdc2, result;

    if ( ( jd1 = twjuliandate( tw1 ) ) == -1L )
	return ( 0L );
    if ( ( sdc1 = twsubdayclock( tw1 ) ) == -1L )
	return ( 0L );

    if ( ( jd2 = twjuliandate( tw2 ) ) == -1L )
	return ( 0L );
    if ( ( sdc2 = twsubdayclock( tw2 ) ) == -1L )
	return ( 0L );
    
    result = ( jd1 - jd2 ) * 24 * 60 * 60 + ( sdc1 - sdc2 );

    return ( result );
    }

/*  */

/*
 *    Simple calculation of day of the week.  Algorithm used is Zeller's
 *    congruence.  Currently, we assume if tw -> tw_year < 100
 *    then the century is CENTURY.
 */

set_dotw( tw )
struct tws *tw;
    {
    register int month, day, year, century;

    month = tw -> tw_mon - 1;
    day = tw -> tw_mday;
    year = tw -> tw_year % 100;
    century = tw -> tw_year >= 100 ? tw -> tw_year / 100 : CENTURY;

    if ( month <= 0 )
	{
	month += 12;
	if ( --year < 0 )
	    {
	    year += 100;
	    century--;
	    }
	}

    tw -> tw_wday =
	((26 * month - 2) / 10 + day + year + year / 4
	    - 3 * century / 4 + 1) % 7;

    tw -> tw_flags &= ~TW_SDAY;
    tw -> tw_flags |= TW_SIMP;
    }
