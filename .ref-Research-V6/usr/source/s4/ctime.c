#
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
 * The routine corrects for daylight saving
 * time and will work in any time zone provided
 * "timezone" is adjusted to the difference between
 * Greenwich and local standard time (measured in seconds).
 * In places like Michigan "daylight" must
 * be initialized to 0 to prevent the conversion
 * to daylight time.
 * There is a table which accounts for the peculiarities
 * undergone by daylight time in 1974-1975.
 *
 * The routine does not work
 * in Saudi Arabia which runs on Solar time.
 *
 * asctime(tvec))
 * where tvec is produced by localtime
 * returns a ptr to a character string
 * that has the ascii time in the form
 *	Thu Jan 01 00:00:00 1970n0\\
 *	01234567890123456789012345
 *	0	  1	    2
 *
 * ctime(t) just calls localtime, then asctime.
 */
char	cbuf[26];
int	dmsize[12]
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

int timezone	5*60*60;
int tzname[]
{
	"EST",
	"EDT",
};
int	daylight 1;	/* Allow daylight conversion */
/*
 * The following table is used for 1974 and 1975 and
 * gives the day number of the first day after the Sunday of the
 * change.
 */
struct {
	int	daylb;
	int	dayle;
} daytab[] {
	5,	333,	/* 1974: Jan 6 - last Sun. in Nov */
	58,	303,	/* 1975: Last Sun. in Feb - last Sun in Oct */
};

#define	SEC	0
#define	MIN	1
#define	HOUR	2
#define	MDAY	3
#define	MON	4
#define	YEAR	5
#define	WDAY	6
#define	YDAY	7
#define	ISDAY	8

ctime(at)
int *at;
{
	return(asctime(localtime(at)));
}

localtime(tim)
int tim[];
{
	register int *t, *ct, dayno;
	int daylbegin, daylend;
	int copyt[2];

	t = copyt;
	t[0] = tim[0];
	t[1] = tim[1];
	dpadd(t, -timezone);
	ct = gmtime(t);
	dayno = ct[YDAY];
	daylbegin = 119;	/* last Sun in Apr */
	daylend = 303;		/* Last Sun in Oct */
	if (ct[YEAR]==74 || ct[YEAR]==75) {
		daylbegin = daytab[ct[YEAR]-74].daylb;
		daylend = daytab[ct[YEAR]-74].dayle;
	}
	daylbegin = sunday(ct, daylbegin);
	daylend = sunday(ct, daylend);
	if (daylight &&
	    (dayno>daylbegin || (dayno==daylbegin && ct[HOUR]>=2)) &&
	    (dayno<daylend || (dayno==daylend && ct[HOUR]<1))) {
		dpadd(t, 1*60*60);
		ct = gmtime(t);
		ct[ISDAY]++;
	}
	return(ct);
}

/*
 * The argument is a 0-origin day number.
 * The value is the day number of the first
 * Sunday on or after the day.
 */
sunday(at, ad)
int *at;
{
	register int *t, d;

	t = at;
	d = ad;
	if (d >= 58)
		d =+ dysize(t[YEAR]) - 365;
	return(d - (d - t[YDAY] + t[WDAY] + 700) % 7);
}

gmtime(tim)
int tim[];
{
	register int d0, d1;
	register *tp;
	static xtime[9];
	extern int ldivr;

	/*
	 * break initial number into
	 * multiples of 8 hours.
	 * (28800 = 60*60*8)
	 */

	d0 = ldiv(tim[0], tim[1], 28800);
	d1 = ldivr;
	tp = &xtime[0];

	/*
	 * generate hours:minutes:seconds
	 */

	*tp++ = d1%60;
	d1 =/ 60;
	*tp++ = d1%60;
	d1 =/ 60;
	d1 =+ (d0%3)*8;
	d0 =/ 3;
	*tp++ = d1;

	/*
	 * d0 is the day number.
	 * generate day of the week.
	 */

	xtime[WDAY] = (d0+4)%7;

	/*
	 * year number
	 */
	for(d1=70; d0 >= dysize(d1); d1++)
		d0 =- dysize(d1);
	xtime[YEAR] = d1;
	xtime[YDAY] = d0;

	/*
	 * generate month
	 */

	if (dysize(d1)==366)
		dmsize[1] = 29;
	for(d1=0; d0 >= dmsize[d1]; d1++)
		d0 =- dmsize[d1];
	dmsize[1] = 28;
	*tp++ = d0+1;
	*tp++ = d1;
	xtime[ISDAY] = 0;
	return(xtime);
}

asctime(t)
int *t;
{
	register char *cp, *ncp;
	register int *tp;

	cp = cbuf;
	for (ncp = "Day Mon 00 00:00:00 1900\n"; *cp++ = *ncp++;);
	ncp = &"SunMonTueWedThuFriSat"[3*t[6]];
	cp = cbuf;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	cp++;
	tp = &t[4];
	ncp = &"JanFebMarAprMayJunJulAugSepOctNovDec"[(*tp)*3];
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	*cp++ = *ncp++;
	cp = ct_numb(cp, *--tp);
	cp = ct_numb(cp, *--tp+100);
	cp = ct_numb(cp, *--tp+100);
	cp = ct_numb(cp, *--tp+100);
	cp =+ 2;
	cp = ct_numb(cp, t[YEAR]);
	return(cbuf);
}

dysize(y)
{
	if((y%4) == 0)
		return(366);
	return(365);
}

ct_numb(acp, n)
{
	register char *cp;

	cp = acp;
	cp++;
	if (n>=10)
		*cp++ = (n/10)%10 + '0';
	else
		*cp++ = ' ';
	*cp++ = n%10 + '0';
	return(cp);
}
