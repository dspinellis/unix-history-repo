/*	@(#)asctotime.c	4.2	(Melbourne)	82/02/15	*/
/*
 *	Asctotime	by Mark Davoren (plus emitl and dcmp from unctime.c)
 *
 *	Routines to take a date string and return the number of seconds
 *	since the epoch to 0:00 am that day (local time)
 *	There are 10 allowable formats
 *	1	d
 *	2	d m
 *	3	d m y
 *	4	d M y
 *	5	d M
 *	6	D
 *	7	M d
 *	8	M d y
 *	9	y m d
 *	10	y M d
 *	where d, m and y represent day, month and year respectively.
 *	small letters mean a number, capitals mean a name.
 *	With the year either 19xx or xx is allowable.
 *	Times beyond the end of the 20th century are not handled.
 *
 *	Any of the formats may include a time specification in the
 *	form	hh:mm[:ss]
 *
 *	With format 6;
 *		 D : today <= D < today next week
 *	An optional '-' may directly precede the day name
 *		-D : today last week <= D < today
 *	This may also be suffixed by one of "week" or "fortnight"
 *
 *	Most sensible abbreviations are allowable for day and month names,
 *	names can be of either upper, lower or mixed case.
 *	Formats 1-5 may have an optional 'st', 'nd', 'rd', 'th' after the day
 *	if the day is the first, second, third or later day of the month
 *	respectively.
 *	asctotime knows about leap years
 */
#include	<time.h>
#include	<sys/types.h>
#include	<ctype.h>
#include	<stdio.h>
#define		bufmax	20

#define		error(s)	{ a2terr = s; return(-1); }
char		*a2terr;

struct tm *localtime();
time_t time();
time_t emitl();
char *index();
/*
 *	Structures for days and months.
 */
struct daynum	{
	char	*name;
	int	num;
};

static struct	daynum	months[] = {
	"jan_uary", 31,
	"feb_ruary", 29,
	"mar_ch", 31,
	"apr_il", 30,
	"may", 31,
	"jun_e", 30,
	"jul_y", 31,
	"aug_ust", 31,
	"sep_t_ember", 30,
	"oct_ober", 31,
	"nov_ember", 30,
	"dec_ember", 31,
	(char *)0
}, days[] = {
	"su_n_day", 0,
	"mo_n_day", 1,
	"tu_e_s_day", 2,
	"we_d_nesday", 3,
	"th_u_r_s_day", 4,
	"fr_i_day", 5,
	"sa_t_urday", 6,
	"today", 7,
	"tomorrow", 8,
	"yesterday", 9,
	(char *)0
};


/*
 *	Getday is the main routine, which decodes the date string.
 *	It checks the values of each field and fills in default fields
 */
time_t
asctotime(p, ww)
	char *p;
	struct	 tm	*ww;
{
	register struct	tm	*w, *t;
	register int	i, mflag = 0, flag678 = 0;
		 int	x;
	struct tm 	tm;
	char	buff[bufmax];
	char	copy[256];
	register char	*q;
	char		*s;

	strncpy(copy, p, sizeof copy - 1);
	copy[sizeof copy - 1] = 0;
	for (q = copy; *q; q++)
		if (isupper(*q))
			*q = tolower(*q);
	p = copy;
	a2terr = (char *)0;
	if (ww)
		w = ww;
	else
		w = &tm;
	x = time((time_t *)0);
	t = localtime(&x);
	scan(&p);
	if (*p == '\0')
		return(emitl(t));

	w->tm_sec = w->tm_min = w->tm_hour = 0;
	if (q = index(p, ':')) {	/* a time is specified */
		while (isspace(*--q) && q > p)
			;
		while (isdigit(*q) && q >= p)
			q--;
		while (q >= p && isspace(*q))
			q--;
		q++;
		s = q;
		w->tm_hour = getnum(&s);
		s = index(s, ':')+1;		/* ':' must exist */
		w->tm_min = getnum(&s);
		if (index(s, ':')) {		/* have seconds */
			s = index(s, ':')+1;
			w->tm_sec = getnum(&s);
		}
		scan(&s);
		if (s[1] == 'm') {
			if (s[0] == 'p')
				w->tm_hour += 12;
			else if (s[0] != 'a')
				s-= 2;
			else if (w->tm_hour == 12)
				w->tm_hour = 0;
			s += 2;
		}
		if (w->tm_hour == 24) {
			if (s[-2] == 'p')
				w->tm_hour = 12;
			else
				w->tm_hour = 0;
		}
		if (w->tm_hour > 23 || w->tm_min > 59 || w->tm_sec > 59)
			error("bad time");
		/* now remove the time from the buffer */
		*q++ = ' ';
		while (*q++ = *s++)
			;
		scan(&p);
		if (*p == '\0') {
			strcpy(copy, "today");
			p = copy;
		}
	}
	if (strncmp(p, "last", 4) == 0) {
		mflag++;
		p += 4;
		scan(&p);
	} else if (strncmp(p, "next", 4) == 0) {
		mflag--;
		p += 4;
		scan(&p);
	} else if (*(p-1) == '-')
		mflag++;

	/* if there is a word here put it into buff */
	if (isalpha(*p)) {
		for (q = buff; isalpha(*p) && q < &buff[bufmax-1]; )
			*q++ = *p++;
		*q = '\0';
		scan(&p);
		flag678++;
	}

	if (isdigit(*p)) {		/* Formats 1-5, 7, 8 */
		w->tm_mday = getnum(&p);
		getends(w->tm_mday, &p);
		if (w->tm_mday > 31) {		/* must be a year instead */
			w->tm_year = w->tm_mday;
			w->tm_mday = 0;
		}

		if (flag678) {		/* Formats 7 or 8 */
			if ((w->tm_mon = getname(months, buff) + 1) == 0)
				error("Bad month name");

		} else if (isalpha(*p)) { /* Formats 4, 5 */
			for (q = buff; isalpha(*p) && q < &buff[bufmax-1]; )
				*q++ =  *p++;
			*q = '\0';
			if ((w->tm_mon = getname(months, buff) + 1) == 0)
				error("Bad month name");

		} else {		/* Formats 1-3 */
			w->tm_mon = getnum(&p);
			getends(w->tm_mon, &p);
		}

		/* check for format 1 */
		if (w->tm_mon == 0)
			w->tm_mon = t->tm_mon;
		else
			w->tm_mon--;


		/* get year */
		scan(&p);
		if (w->tm_mday == 0)
			getends(w->tm_mday = getnum(&p), &p);
		else
			getends(w->tm_year = getnum(&p), &p);

		/* validate month */
		if (w->tm_mon > 11)
			error("month field too large");

		/* validate day */
		if (w->tm_mday == 0 || w->tm_mday > months[w->tm_mon].num)
			error("day field too large");

		/* check for formats 1,2 and 5 */
		if (w->tm_year == 0) {
			w->tm_year = t->tm_year;
			if (w->tm_mon + 8 < t->tm_mon && mflag <= 0)
				w->tm_year++;
		}

		/* check for 29th of feb in a non leap year */
		if (  w->tm_mon == 1
		   && w->tm_mday == 29
		   && !(   w->tm_year % 4 == 0
			&& w->tm_year % 100 != 0
			|| w->tm_year % 400 == 0))
				error("not a leap year");
		if (w->tm_year >= 1970)
			w->tm_year -= 1900;
		else if (w->tm_year > 100 || w->tm_year < 70)
			error("year field too small");

	} else { 		/* format 6 */
		i = getname(days, buff);

		if (i == -1)
			error("error in day name");

		/* check for today & tomorrow & yesterday */
		if (i == 7) {
			i = t->tm_wday;
			mflag = 0;
		} else if (i == 8) {
			i = t->tm_wday+1 % 7;
			mflag = -1;
		} else if (i == 9) {
			i = t->tm_wday+6 % 7;
			mflag = 1;
		}

		w->tm_year = t->tm_year;
		w->tm_mon = t->tm_mon;
		w->tm_mday  = t->tm_mday - (t->tm_wday - i);

		if (mflag > 0 && t->tm_wday <= i)
			w->tm_mday -= 7;
		if (mflag == 0 && t->tm_wday > i)
			w->tm_mday += 7;
		if (mflag < 0 && t->tm_wday == i)
			w->tm_mday += 7;
		scan(&p);
		if (strcmp(p, "week") == 0)
			i = 7;
		else if (strcmp(p, "fortnight") == 0)
			i = 14;
		else
			i = 0;
		if (mflag > 0)
			w->tm_mday -= i;
		else
			w->tm_mday += i;

		/* allow for wrap around */
		if (w->tm_mday < 1) {
			/* last month */
			w->tm_mon--;
			w->tm_mday += months[w->tm_mon].num;
			if (w->tm_mon < 0) {
				/* last year */
				w->tm_year--;
				w->tm_mon = 11;
			}
		} else if (w->tm_mday > months [w->tm_mon].num) {
			/* next month */
			w->tm_mday -= months[w->tm_mon].num;
			w->tm_mon++;
			if (w->tm_mon > 11) {
				/* next year */
				w->tm_year++;
				w->tm_mon = 0;
			}
		}
	}
	return(emitl(w));
}

static
scan(pp)
	register char	**pp;
{
	while(**pp && !isalpha(**pp) && !isdigit(**pp))
		++*pp;
}

static
getends(i, pp)
	register int	i;
		 char	**pp;
{
	register char	*p;
	register c0, c1;

	p = *pp;
	c0 = p[0];
	if (c0 == '\0')
		return;
	c1 = p[1];
	switch (i % 10) {
	case 1:
		if (c0 == 's' && c1 == 't')
			p += 2;
		break;
	case 2:
		if (c0 == 'n' && c1 == 'd')
			p += 2;
		break;
	case 3:
		if (c0 == 'r' && c1 == 'd')
			p += 2;
		break;
	default:
		if (c0 == 't' && c1 == 'h')
			p += 2;
		break;
	}
	*pp = p;
	scan(pp);
}

static
getnum(q)
	char	**q;
{
	register int	sum;
	register char	*p;

	p = *q;
	while (*p && !isdigit(*p))
		p++;
	sum = 0;
	while (isdigit(*p))
		sum = sum * 10 + *p++ - '0';
	*q = p;
	return(sum);
}

/*
 *	Getname searches arr an array of daynum structures for a name matching
 *	the name passed as an argument. The entries in the array consist
 *	of words with '_' delimiting abbreviations;
 *	eg	'mon_day' will match 'mon' or 'monday'
 *	Getname returns the index into arr which matches name or -1 if no match
 */
static
getname(arr, name)
	struct	 daynum	arr[];
	register char	*name;
{
	register struct	daynum	*i;
	register char *p, *q;

	for(i = arr; i->name; i++) {
		for (p = name, q = i->name; *p; p++, q++) {
			if (*p == *q)
				continue;
			if (*q != '_')
				break;
			p--;
		}
		if (*p == '\0' && (*q == '\0' || *q == '_'))
			return(i - arr);
	}
	return(-1);
}

/*
 * Routine to convert a localtime(3) format date back into
 * a system format date.
 *
 *	Use a binary search.
 */

time_t
emitl(dp)
	struct tm *dp;
{
	time_t conv;
	register int i, bit;
	struct tm dcopy;

	dcopy = *dp;
	dp = &dcopy;
	conv = 0;
	for (i = 30; i >= 0; i--) {
		bit = 1 << i;
		conv |= bit;
		if (dcmp(localtime(&conv), dp) > 0)
			conv &= ~bit;
	}
	return(conv);
};

/*
 * Compare two localtime dates, return result.
 */

#define DECIDE(a) \
	if (dp->a > dp2->a) \
		return(1); \
	if (dp->a < dp2->a) \
		return(-1)

static
dcmp(dp, dp2)
	register struct tm *dp, *dp2;
{
	DECIDE(tm_year);
	DECIDE(tm_mon);
	DECIDE(tm_mday);
	DECIDE(tm_hour);
	DECIDE(tm_min);
	DECIDE(tm_sec);
	return(0);
}
