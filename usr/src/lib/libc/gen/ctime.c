/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ctime.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * This routine converts time as follows.
 * The epoch is 0000 Jan 1 1970 GMT.
 * The argument time is in seconds since then.
 * The localtime(t) entry returns a pointer to an array
 * containing
 *  seconds (0-59)
 *  minutes (0-59)
 *  hours (0-23)
 *  day of month (1-31)
 *  month (0-11)
 *  year-1970
 *  weekday (0-6, Sun is 0)
 *  day of the year
 *  daylight savings flag
 *
 * The routine calls the system to determine the local
 * timezone and whether Daylight Saving Time is permitted locally.
 * (DST is then determined by the current local rules)
 *
 * The routine does not work
 * in Saudi Arabia which runs on Solar time.
 *
 * asctime(tvec))
 * where tvec is produced by localtime
 * returns a ptr to a character string
 * that has the ascii time in the form
 *	Thu Jan 01 00:00:00 1970\n\0
 *	0123456789012345678901234 5
 *	0	  1	    2
 *
 * ctime(t) just calls localtime, then asctime.
 */

#include <sys/time.h>
#include <sys/types.h>
#include <sys/timeb.h>

static	char	cbuf[26];
static	int	dmsize[12] =
{
	31,
	28,
	31,
	30,
	31,
	30,
	31,
	31,
	30,
	31,
	30,
	31
};

/*
 * The following table is used for 1974 and 1975 and
 * gives the day number of the first day after the Sunday of the
 * change.
 */
struct dstab {
	int	dayyr;
	int	daylb;
	int	dayle;
};

static struct dstab usdaytab[] = {
	1974,	5,	333,	/* 1974: Jan 6 - last Sun. in Nov */
	1975,	58,	303,	/* 1975: Last Sun. in Feb - last Sun in Oct */
	0,	119,	303,	/* all other years: end Apr - end Oct */
};
static struct dstab ausdaytab[] = {
	1970,	400,	0,	/* 1970: no daylight saving at all */
	1971,	303,	0,	/* 1971: daylight saving from Oct 31 */
	1972,	303,	58,	/* 1972: Jan 1 -> Feb 27 & Oct 31 -> dec 31 */
	0,	303,	65,	/* others: -> Mar 7, Oct 31 -> */
};

/*
 * The European tables ... based on hearsay
 * Believed correct for:
 *	WE:	Great Britain, Portugal?
 *	ME:	Belgium, Luxembourg, Netherlands, Denmark, Norway,
 *		Austria, Poland, Czechoslovakia, Sweden, Switzerland,
 *		DDR, DBR, France, Spain, Hungary, Italy, Jugoslavia
 *		Finland (EE timezone, but ME dst rules)
 * Eastern European dst is unknown, we'll make it ME until someone speaks up.
 *	EE:	Bulgaria, Greece, Rumania, Turkey, Western Russia
 *
 * Ireland is unpredictable.  (Years when Easter Sunday just happens ...)
 * Years before 1983 are suspect.
 */
static struct dstab wedaytab[] = {
	1983,	89,	296,	/* 1983: end March - end Oct */
	0,	89,	303,	/* others: end March - end Oct */
};

static struct dstab medaytab[] = {
	1983,	89,	296,	/* 1983: end March - end Oct */
	0,	89,	272,	/* others: end March - end Sep */
};

/*
 * Canada, same as the US, except no early 70's fluctuations.
 * Can this really be right ??
 */
static struct dstab candaytab[] = {
	0,	119,	303,	/* all years: end Apr - end Oct */
};

static struct dayrules {
	int		dst_type;	/* number obtained from system */
	int		dst_hrs;	/* hours to add when dst on */
	struct	dstab *	dst_rules;	/* one of the above */
	enum {STH,NTH}	dst_hemi;	/* southern, northern hemisphere */
} dayrules [] = {
	DST_USA,	1,	usdaytab,	NTH,
	DST_AUST,	1,	ausdaytab,	STH,
	DST_WET,	1,	wedaytab,	NTH,
	DST_MET,	1,	medaytab,	NTH,
	DST_EET,	1,	medaytab,	NTH,	/* XXX */
	DST_CAN,	1,	candaytab,	NTH,
	-1,
};

struct tm	*gmtime();
char		*ct_numb();
struct tm	*localtime();
char	*ctime();
char	*ct_num();
char	*asctime();

char *
ctime(t)
time_t *t;
{
	return(asctime(localtime(t)));
}

struct tm *
localtime(tim)
time_t *tim;
{
	register int dayno;
	register struct tm *ct;
	register dalybeg, daylend;
	register struct dayrules *dr;
	register struct dstab *ds;
	int year;
	time_t copyt;
	struct timeval curtime;
	struct timezone zone;

	gettimeofday(&curtime, &zone);
	copyt = *tim - (time_t)zone.tz_minuteswest*60;
	ct = gmtime(&copyt);
	dayno = ct->tm_yday;
	for (dr = dayrules; dr->dst_type >= 0; dr++)
		if (dr->dst_type == zone.tz_dsttime)
			break;
	if (dr->dst_type >= 0) {
		year = ct->tm_year + 1900;
		for (ds = dr->dst_rules; ds->dayyr; ds++)
			if (ds->dayyr == year)
				break;
		dalybeg = ds->daylb;	/* first Sun after dst starts */
		daylend = ds->dayle;	/* first Sun after dst ends */
		dalybeg = sunday(ct, dalybeg);
		daylend = sunday(ct, daylend);
		switch (dr->dst_hemi) {
		case NTH:
		    if (!(
		       (dayno>dalybeg || (dayno==dalybeg && ct->tm_hour>=2)) &&
		       (dayno<daylend || (dayno==daylend && ct->tm_hour<1))
		    ))
			    return(ct);
		    break;
		case STH:
		    if (!(
		       (dayno>dalybeg || (dayno==dalybeg && ct->tm_hour>=2)) ||
		       (dayno<daylend || (dayno==daylend && ct->tm_hour<2))
		    ))
			    return(ct);
		    break;
		default:
		    return(ct);
		}
	        copyt += dr->dst_hrs*60*60;
		ct = gmtime(&copyt);
		ct->tm_isdst++;
	}
	return(ct);
}

/*
 * The argument is a 0-origin day number.
 * The value is the day number of the first
 * Sunday on or before the day.
 */
static
sunday(t, d)
register struct tm *t;
register int d;
{
	if (d >= 58)
		d += dysize(t->tm_year) - 365;
	return(d - (d - t->tm_yday + t->tm_wday + 700) % 7);
}

struct tm *
gmtime(tim)
time_t *tim;
{
	register int d0, d1;
	long hms, day;
	register int *tp;
	static struct tm xtime;

	/*
	 * break initial number into days
	 */
	hms = *tim % 86400;
	day = *tim / 86400;
	if (hms<0) {
		hms += 86400;
		day -= 1;
	}
	tp = (int *)&xtime;

	/*
	 * generate hours:minutes:seconds
	 */
	*tp++ = hms%60;
	d1 = hms/60;
	*tp++ = d1%60;
	d1 /= 60;
	*tp++ = d1;

	/*
	 * day is the day number.
	 * generate day of the week.
	 * The addend is 4 mod 7 (1/1/1970 was Thursday)
	 */

	xtime.tm_wday = (day+7340036)%7;

	/*
	 * year number
	 */
	if (day>=0) for(d1=70; day >= dysize(d1); d1++)
		day -= dysize(d1);
	else for (d1=70; day<0; d1--)
		day += dysize(d1-1);
	xtime.tm_year = d1;
	xtime.tm_yday = d0 = day;

	/*
	 * generate month
	 */

	if (dysize(d1)==366)
		dmsize[1] = 29;
	for(d1=0; d0 >= dmsize[d1]; d1++)
		d0 -= dmsize[d1];
	dmsize[1] = 28;
	*tp++ = d0+1;
	*tp++ = d1;
	xtime.tm_isdst = 0;
	return(&xtime);
}

char *
asctime(t)
struct tm *t;
{
	register char *cp, *ncp;
	register int *tp;

	cp = cbuf;
	for (ncp = "Day Mon 00 00:00:00 1900\n"; *cp++ = *ncp++;);
	ncp = &"SunMonTueWedThuFriSat"[3*t->tm_wday];
	cp = cbuf;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	cp++;
	tp = &t->tm_mon;
	ncp = &"JanFebMarAprMayJunJulAugSepOctNovDec"[(*tp)*3];
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	cp = ct_numb(cp, *--tp);
	cp = ct_numb(cp, *--tp+100);
	cp = ct_numb(cp, *--tp+100);
	cp = ct_numb(cp, *--tp+100);
	if (t->tm_year>=100) {
		cp[1] = '2';
		cp[2] = '0' + (t->tm_year-100) / 100;
	}
	cp += 2;
	cp = ct_numb(cp, t->tm_year+100);
	return(cbuf);
}

dysize(y)
{
	if((y%4) == 0)
		return(366);
	return(365);
}

static char *
ct_numb(cp, n)
register char *cp;
{
	cp++;
	if (n>=10)
		*cp++ = (n/10)%10 + '0';
	else
		*cp++ = ' ';
	*cp++ = n%10 + '0';
	return(cp);
}
