#ifndef lint
static char sccsid[] = "@(#)zic.c	1.3 (Berkeley) 10/22/87";
#endif

#include "stdio.h"
#include "ctype.h"
#include "sys/types.h"
#include "sys/stat.h"
#include "sys/file.h"
#include "strings.h"
#include "time.h"
#include "tzfile.h"

#ifndef BUFSIZ
#define BUFSIZ	1024
#endif

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

extern char *	icpyalloc();
extern char *	imalloc();
extern char *	irealloc();
extern char *	optarg;
extern int	optind;
extern char *	scheck();

static		addtt();
static		addtype();
static		associate();
static int	charcnt;
static		ciequal();
static long	eitol();
static int	errors;
static char *	filename;
static char **	getfields();
static long	gethms();
static		infile();
static		inlink();
static		inrule();
static		inzcont();
static		inzone();
static		inzsub();
static int	linenum;
static		lowerit();
static time_t	max_time;
static int	max_year;
static time_t	min_time;
static int	min_year;
static		mkdirs();
static		newabbr();
static int	noise;
static		nondunlink();
static long	oadd();
static		outzone();
static char *	progname;
static char *	rfilename;
static int	rlinenum;
static time_t	rpytime();
static		rulesub();
static		setboundaries();
static time_t	tadd();
static int	timecnt;
static int	tt_signed;
static int	typecnt;
static		yearistype();

/*
** Line codes.
*/

#define LC_RULE		0
#define LC_ZONE		1
#define LC_LINK		2

/*
** Which fields are which on a Zone line.
*/

#define ZF_NAME		1
#define ZF_GMTOFF	2
#define ZF_RULE		3
#define ZF_FORMAT	4
#define ZF_UNTILYEAR	5
#define ZF_UNTILMONTH	6
#define ZF_UNTILDAY	7
#define ZF_UNTILTIME	8
#define ZONE_MINFIELDS	5
#define ZONE_MAXFIELDS	9

/*
** Which fields are which on a Zone continuation line.
*/

#define ZFC_GMTOFF	0
#define ZFC_RULE	1
#define ZFC_FORMAT	2
#define ZFC_UNTILYEAR	3
#define ZFC_UNTILMONTH	4
#define ZFC_UNTILDAY	5
#define ZFC_UNTILTIME	6
#define ZONEC_MINFIELDS	3
#define ZONEC_MAXFIELDS	7

/*
** Which files are which on a Rule line.
*/

#define RF_NAME		1
#define RF_LOYEAR	2
#define RF_HIYEAR	3
#define RF_COMMAND	4
#define RF_MONTH	5
#define RF_DAY		6
#define RF_TOD		7
#define RF_STDOFF	8
#define RF_ABBRVAR	9
#define RULE_FIELDS	10

/*
** Which fields are which on a Link line.
*/

#define LF_FROM		1
#define LF_TO		2
#define LINK_FIELDS	3

struct rule {
	char *	r_filename;
	int	r_linenum;
	char *	r_name;

	int	r_loyear;	/* for example, 1986 */
	int	r_hiyear;	/* for example, 1986 */
	char *	r_yrtype;

	int	r_month;	/* 0..11 */

	int	r_dycode;	/* see below */
	int	r_dayofmonth;
	int	r_wday;

	long	r_tod;		/* time from midnight */
	int	r_todisstd;	/* above is standard time if TRUE */
				/* above is wall clock time if FALSE */
	long	r_stdoff;	/* offset from standard time */
	char *	r_abbrvar;	/* variable part of time zone abbreviation */

	int	r_todo;		/* a rule to do (used in outzone) */
	time_t	r_temp;		/* used in outzone */
};

/*
**	r_dycode		r_dayofmonth	r_wday
*/
#define DC_DOM		0	/* 1..31 */	/* unused */
#define DC_DOWGEQ	1	/* 1..31 */	/* 0..6 (Sun..Sat) */
#define DC_DOWLEQ	2	/* 1..31 */	/* 0..6 (Sun..Sat) */

/*
** Year synonyms.
*/

#define YR_MINIMUM	0
#define YR_MAXIMUM	1
#define YR_ONLY		2

static struct rule *	rules;
static int		nrules;	/* number of rules */

struct zone {
	char *		z_filename;
	int		z_linenum;

	char *		z_name;
	long		z_gmtoff;
	char *		z_rule;
	char *		z_format;

	long		z_stdoff;

	struct rule *	z_rules;
	int		z_nrules;

	struct rule	z_untilrule;
	time_t		z_untiltime;
};

static struct zone *	zones;
static int		nzones;	/* number of zones */

struct link {
	char *		l_filename;
	int		l_linenum;
	char *		l_from;
	char *		l_to;
};

static struct link *	links;
static int		nlinks;

struct lookup {
	char *		l_word;
	int		l_value;
};

static struct lookup *	byword();

static struct lookup	line_codes[] = {
	"Rule",		LC_RULE,
	"Zone",		LC_ZONE,
	"Link",		LC_LINK,
	NULL,		0
};

static struct lookup	mon_names[] = {
	"January",	TM_JANUARY,
	"February",	TM_FEBRUARY,
	"March",	TM_MARCH,
	"April",	TM_APRIL,
	"May",		TM_MAY,
	"June",		TM_JUNE,
	"July",		TM_JULY,
	"August",	TM_AUGUST,
	"September",	TM_SEPTEMBER,
	"October",	TM_OCTOBER,
	"November",	TM_NOVEMBER,
	"December",	TM_DECEMBER,
	NULL,		0
};

static struct lookup	wday_names[] = {
	"Sunday",	TM_SUNDAY,
	"Monday",	TM_MONDAY,
	"Tuesday",	TM_TUESDAY,
	"Wednesday",	TM_WEDNESDAY,
	"Thursday",	TM_THURSDAY,
	"Friday",	TM_FRIDAY,
	"Saturday",	TM_SATURDAY,
	NULL,		0
};

static struct lookup	lasts[] = {
	"last-Sunday",		TM_SUNDAY,
	"last-Monday",		TM_MONDAY,
	"last-Tuesday",		TM_TUESDAY,
	"last-Wednesday",	TM_WEDNESDAY,
	"last-Thursday",	TM_THURSDAY,
	"last-Friday",		TM_FRIDAY,
	"last-Saturday",	TM_SATURDAY,
	NULL,			0
};

static struct lookup	begin_years[] = {
	"minimum",		YR_MINIMUM,
	"maximum",		YR_MAXIMUM,
	NULL,			0
};

static struct lookup	end_years[] = {
	"minimum",		YR_MINIMUM,
	"maximum",		YR_MAXIMUM,
	"only",			YR_ONLY,
	NULL,			0
};

static int	len_months[2][MONS_PER_YEAR] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
	31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static int	len_years[2] = {
	DAYS_PER_NYEAR, DAYS_PER_LYEAR
};

static time_t		ats[TZ_MAX_TIMES];
static unsigned char	types[TZ_MAX_TIMES];
static long		gmtoffs[TZ_MAX_TYPES];
static char		isdsts[TZ_MAX_TYPES];
static char		abbrinds[TZ_MAX_TYPES];
static char		chars[TZ_MAX_CHARS];

/*
** Memory allocation.
*/

static char *
memcheck(ptr)
char *	ptr;
{
	if (ptr == NULL) {
		perror(progname);
		exit(1);
	}
	return ptr;
}

#define emalloc(size)		memcheck(imalloc(size))
#define erealloc(ptr, size)	memcheck(irealloc(ptr, size))
#define ecpyalloc(ptr)		memcheck(icpyalloc(ptr))

/*
** Error handling.
*/

static
eats(name, num, rname, rnum)
char *	name;
char *	rname;
{
	filename = name;
	linenum = num;
	rfilename = rname;
	rlinenum = rnum;
}

static
eat(name, num)
char *	name;
{
	eats(name, num, (char *) NULL, -1);
}

static
error(string)
char *	string;
{
	/*
	** Match the format of "cc" to allow sh users to
	** 	zic ... 2>&1 | error -t "*" -v
	** on BSD systems.
	*/
	(void) fprintf(stderr, "\"%s\", line %d: %s",
		filename, linenum, string);
	if (rfilename != NULL)
		(void) fprintf(stderr, " (rule from \"%s\", line %d)",
			rfilename, rlinenum);
	(void) fprintf(stderr, "\n");
	++errors;
}

static
usage()
{
	(void) fprintf(stderr,
"%s: usage is %s [ -v ] [ -l localtime ] [ -d directory ] [ filename ... ]\n",
		progname, progname);
	exit(1);
}

static char *	lcltime = NULL;
static char *	directory = NULL;

main(argc, argv)
int	argc;
char *	argv[];
{
	register int	i, j;
	register int	c;

#ifdef unix
	umask(umask(022) | 022);
#endif
	progname = argv[0];
	while ((c = getopt(argc, argv, "d:l:v")) != EOF)
		switch (c) {
			default:
				usage();
			case 'd':
				if (directory == NULL)
					directory = optarg;
				else {
					(void) fprintf(stderr,
"%s: More than one -d option specified\n",
						progname);
					exit(1);
				}
				break;
			case 'l':
				if (lcltime == NULL)
					lcltime = optarg;
				else {
					(void) fprintf(stderr,
"%s: More than one -l option specified\n",
						progname);
					exit(1);
				}
				break;
			case 'v':
				noise = TRUE;
				break;
		}
	if (optind == argc - 1 && strcmp(argv[optind], "=") == 0)
		usage();	/* usage message by request */
	if (directory == NULL)
		directory = TZDIR;

	setboundaries();

	zones = (struct zone *) emalloc(0);
	rules = (struct rule *) emalloc(0);
	links = (struct link *) emalloc(0);
	for (i = optind; i < argc; ++i)
		infile(argv[i]);
	if (errors)
		exit(1);
	associate();
	for (i = 0; i < nzones; i = j) {
		/*
		 * Find the next non-continuation zone entry.
		 */
		for (j = i + 1; j < nzones && zones[j].z_name == NULL; ++j)
			;
		outzone(&zones[i], j - i);
	}
	/*
	** We'll take the easy way out on this last part.
	*/
	if (chdir(directory) != 0) {
		(void) fprintf(stderr, "%s: Can't chdir to ", progname);
		perror(directory);
		exit(1);
	}
	for (i = 0; i < nlinks; ++i) {
		nondunlink(links[i].l_to);
		if (link(links[i].l_from, links[i].l_to) != 0) {
			(void) fprintf(stderr, "%s: Can't link %s to ",
				progname, links[i].l_from);
			perror(links[i].l_to);
			exit(1);
		}
	}
	if (lcltime != NULL) {
		nondunlink(TZDEFAULT);
		if (link(lcltime, TZDEFAULT) != 0) {
			(void) fprintf(stderr, "%s: Can't link %s to ",
				progname, lcltime);
			perror(TZDEFAULT);
			exit(1);
		}
	}
	exit((errors == 0) ? 0 : 1);
}

static
setboundaries()
{
	register time_t 	bit;

	for (bit = 1; bit > 0; bit <<= 1)
		;
	if (bit == 0) {		/* time_t is an unsigned type */
		tt_signed = FALSE;
		min_time = 0;
		max_time = ~(time_t) 0;
	} else {
		tt_signed = TRUE;
		min_time = bit;
		max_time = bit;
		++max_time;
		max_time = -max_time;
	}
	min_year = TM_YEAR_BASE + gmtime(&min_time)->tm_year;
	max_year = TM_YEAR_BASE + gmtime(&max_time)->tm_year;
}

/*
** We get to be careful here since there's a fair chance of root running us.
*/

static
nondunlink(name)
char *	name;
{
	struct stat	s;

	if (stat(name, &s) != 0)
		return;
	if ((s.st_mode & S_IFMT) == S_IFDIR)
		return;
	(void) unlink(name);
}

/*
** Associate sets of rules with zones.
*/

/*
** Sort by rule name.
*/

static
rcomp(cp1, cp2)
char *	cp1;
char *	cp2;
{
	return strcmp(((struct rule *) cp1)->r_name,
		((struct rule *) cp2)->r_name);
}

static
associate()
{
	register struct zone *	zp;
	register struct rule *	rp;
	register int		base, out;
	register int		i;

	if (nrules != 0)
		(void) qsort((char *) rules, nrules, sizeof *rules, rcomp);
	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
		zp->z_rules = NULL;
		zp->z_nrules = 0;
	}
	for (base = 0; base < nrules; base = out) {
		rp = &rules[base];
		for (out = base + 1; out < nrules; ++out)
			if (strcmp(rp->r_name, rules[out].r_name) != 0)
				break;
		for (i = 0; i < nzones; ++i) {
			zp = &zones[i];
			if (strcmp(zp->z_rule, rp->r_name) != 0)
				continue;
			zp->z_rules = rp;
			zp->z_nrules = out - base;
		}
	}
	for (i = 0; i < nzones; ++i) {
		zp = &zones[i];
		if (zp->z_nrules == 0) {
			/*
			** Maybe we have a local standard time offset.
			*/
			eat(zp->z_filename, zp->z_linenum);
			zp->z_stdoff = gethms(zp->z_rule, "unruly zone", TRUE);
			/*
			** Note, though, that if there's no rule,
			** a '%s' in the format is a bad thing.
			*/
			if (index(zp->z_format, '%') != 0)
				error("%s in ruleless zone");
		}
	}
	if (errors)
		exit(1);
}

static
infile(name)
char *	name;
{
	register FILE *			fp;
	register char **		fields;
	register char *			cp;
	register struct lookup *	lp;
	register int			nfields;
	register int			wantcont;
	register int			num;
	char				buf[BUFSIZ];

	if (strcmp(name, "-") == 0) {
		name = "standard input";
		fp = stdin;
	} else if ((fp = fopen(name, "r")) == NULL) {
		(void) fprintf(stderr, "%s: Can't open ", progname);
		perror(name);
		exit(1);
	}
	wantcont = FALSE;
	for (num = 1; ; ++num) {
		eat(name, num);
		if (fgets(buf, sizeof buf, fp) != buf)
			break;
		cp = index(buf, '\n');
		if (cp == NULL) {
			error("line too long");
			exit(1);
		}
		*cp = '\0';
		fields = getfields(buf);
		nfields = 0;
		while (fields[nfields] != NULL) {
			if (ciequal(fields[nfields], "-"))
				fields[nfields] = "";
			++nfields;
		}
		if (nfields == 0) {
			/* nothing to do */
		} else if (wantcont) {
			wantcont = inzcont(fields, nfields);
		} else {
			lp = byword(fields[0], line_codes);
			if (lp == NULL)
				error("input line of unknown type");
			else switch ((int) (lp->l_value)) {
				case LC_RULE:
					inrule(fields, nfields);
					wantcont = FALSE;
					break;
				case LC_ZONE:
					wantcont = inzone(fields, nfields);
					break;
				case LC_LINK:
					inlink(fields, nfields);
					wantcont = FALSE;
					break;
				default:	/* "cannot happen" */
					(void) fprintf(stderr,
"%s: panic: Invalid l_value %d\n",
						progname, lp->l_value);
					exit(1);
			}
		}
		free((char *) fields);
	}
	if (ferror(fp)) {
		(void) fprintf(stderr, "%s: Error reading ", progname);
		perror(filename);
		exit(1);
	}
	if (fp != stdin && fclose(fp)) {
		(void) fprintf(stderr, "%s: Error closing ", progname);
		perror(filename);
		exit(1);
	}
	if (wantcont)
		error("expected continuation line not found");
}

/*
** Convert a string of one of the forms
**	h	-h 	hh:mm	-hh:mm	hh:mm:ss	-hh:mm:ss
** into a number of seconds.
** A null string maps to zero.
** Call error with errstring and return zero on errors.
*/

static long
gethms(string, errstring, signable)
char *	string;
char *	errstring;
{
	int	hh, mm, ss, sign;

	if (string == NULL || *string == '\0')
		return 0;
	if (!signable)
		sign = 1;
	else if (*string == '-') {
		sign = -1;
		++string;
	} else	sign = 1;
	if (sscanf(string, scheck(string, "%d"), &hh) == 1)
		mm = ss = 0;
	else if (sscanf(string, scheck(string, "%d:%d"), &hh, &mm) == 2)
		ss = 0;
	else if (sscanf(string, scheck(string, "%d:%d:%d"),
		&hh, &mm, &ss) != 3) {
			error(errstring);
			return 0;
	}
	if (hh < 0 || hh >= HOURS_PER_DAY ||
		mm < 0 || mm >= MINS_PER_HOUR ||
		ss < 0 || ss >= SECS_PER_MIN) {
			error(errstring);
			return 0;
	}
	return eitol(sign) *
		(eitol(hh * MINS_PER_HOUR + mm) *
		eitol(SECS_PER_MIN) + eitol(ss));
}

static
inrule(fields, nfields)
register char **	fields;
{
	static struct rule	r;

	if (nfields != RULE_FIELDS) {
		error("wrong number of fields on Rule line");
		return;
	}
	if (*fields[RF_NAME] == '\0') {
		error("nameless rule");
		return;
	}
	r.r_filename = filename;
	r.r_linenum = linenum;
	r.r_stdoff = gethms(fields[RF_STDOFF], "invalid saved time", TRUE);
	rulesub(&r, fields[RF_LOYEAR], fields[RF_HIYEAR], fields[RF_COMMAND],
		fields[RF_MONTH], fields[RF_DAY], fields[RF_TOD]);
	r.r_name = ecpyalloc(fields[RF_NAME]);
	r.r_abbrvar = ecpyalloc(fields[RF_ABBRVAR]);
	rules = (struct rule *) erealloc((char *) rules,
		(nrules + 1) * sizeof *rules);
	rules[nrules++] = r;
}

static
inzone(fields, nfields)
register char **	fields;
{
	register int	i;
	char		buf[132];

	if (nfields < ZONE_MINFIELDS || nfields > ZONE_MAXFIELDS) {
		error("wrong number of fields on Zone line");
		return FALSE;
	}
	if (strcmp(fields[ZF_NAME], TZDEFAULT) == 0 && lcltime != NULL) {
		(void) sprintf(buf,
			"\"Zone %s\" line and -l option are mutually exclusive",
			TZDEFAULT);
		error(buf);
		return FALSE;
	}
	for (i = 0; i < nzones; ++i)
		if (zones[i].z_name != NULL &&
			strcmp(zones[i].z_name, fields[ZF_NAME]) == 0) {
				(void) sprintf(buf,
"duplicate zone name %s (file \"%s\", line %d)",
					fields[ZF_NAME],
					zones[i].z_filename,
					zones[i].z_linenum);
				error(buf);
				return FALSE;
		}
	return inzsub(fields, nfields, FALSE);
}

static
inzcont(fields, nfields)
register char **	fields;
{
	if (nfields < ZONEC_MINFIELDS || nfields > ZONEC_MAXFIELDS) {
		error("wrong number of fields on Zone continuation line");
		return FALSE;
	}
	return inzsub(fields, nfields, TRUE);
}

static
inzsub(fields, nfields, iscont)
register char **	fields;
{
	register char *	cp;
	static struct zone	z;
	register int	i_gmtoff, i_rule, i_format;
	register int	i_untilyear, i_untilmonth;
	register int	i_untilday, i_untiltime;
	register int	hasuntil;

	if (iscont) {
		i_gmtoff = ZFC_GMTOFF;
		i_rule = ZFC_RULE;
		i_format = ZFC_FORMAT;
		i_untilyear = ZFC_UNTILYEAR;
		i_untilmonth = ZFC_UNTILMONTH;
		i_untilday = ZFC_UNTILDAY;
		i_untiltime = ZFC_UNTILTIME;
		z.z_name = NULL;
	} else {
		i_gmtoff = ZF_GMTOFF;
		i_rule = ZF_RULE;
		i_format = ZF_FORMAT;
		i_untilyear = ZF_UNTILYEAR;
		i_untilmonth = ZF_UNTILMONTH;
		i_untilday = ZF_UNTILDAY;
		i_untiltime = ZF_UNTILTIME;
		z.z_name = ecpyalloc(fields[ZF_NAME]);
	}
	z.z_filename = filename;
	z.z_linenum = linenum;
	z.z_gmtoff = gethms(fields[i_gmtoff], "invalid GMT offset", TRUE);
	if ((cp = index(fields[i_format], '%')) != 0) {
		if (*++cp != 's' || index(cp, '%') != 0) {
			error("invalid abbreviation format");
			return FALSE;
		}
	}
	z.z_rule = ecpyalloc(fields[i_rule]);
	z.z_format = ecpyalloc(fields[i_format]);
	hasuntil = nfields > i_untilyear;
	if (hasuntil) {
		z.z_untilrule.r_filename = filename;
		z.z_untilrule.r_linenum = linenum;
		rulesub(&z.z_untilrule,
			fields[i_untilyear],
			"only",
			"",
			(nfields > i_untilmonth) ? fields[i_untilmonth] : "Jan",
			(nfields > i_untilday) ? fields[i_untilday] : "1",
			(nfields > i_untiltime) ? fields[i_untiltime] : "0");
		z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);
		if (iscont && nzones > 0 && z.z_untiltime < max_time &&
			z.z_untiltime > min_time &&
			zones[nzones - 1].z_untiltime >= z.z_untiltime) {
error("Zone continuation line end time is not after end time of previous line");
			return FALSE;
		}
	}
	zones = (struct zone *) erealloc((char *) zones,
		(nzones + 1) * sizeof *zones);
	zones[nzones++] = z;
	/*
	** If there was an UNTIL field on this line,
	** there's more information about the zone on the next line.
	*/
	return hasuntil;
}

static
inlink(fields, nfields)
register char **	fields;
{
	struct link	l;

	if (nfields != LINK_FIELDS) {
		error("wrong number of fields on Link line");
		return;
	}
	if (*fields[LF_FROM] == '\0') {
		error("blank FROM field on Link line");
		return;
	}
	if (*fields[LF_TO] == '\0') {
		error("blank TO field on Link line");
		return;
	}
	l.l_filename = filename;
	l.l_linenum = linenum;
	l.l_from = ecpyalloc(fields[LF_FROM]);
	l.l_to = ecpyalloc(fields[LF_TO]);
	links = (struct link *) erealloc((char *) links,
		(nlinks + 1) * sizeof *links);
	links[nlinks++] = l;
}

static
rulesub(rp, loyearp, hiyearp, typep, monthp, dayp, timep)
register struct rule *	rp;
char *			loyearp;
char *			hiyearp;
char *			typep;
char *			monthp;
char *			dayp;
char *			timep;
{
	register struct lookup *	lp;
	register char *			cp;

	if ((lp = byword(monthp, mon_names)) == NULL) {
		error("invalid month name");
		return;
	}
	rp->r_month = lp->l_value;
	rp->r_todisstd = FALSE;
	cp = timep;
	if (*cp != '\0') {
		cp += strlen(cp) - 1;
		switch (lowerit(*cp)) {
			case 's':
				rp->r_todisstd = TRUE;
				*cp = '\0';
				break;
			case 'w':
				rp->r_todisstd = FALSE;
				*cp = '\0';
				break;
		}
	}
	rp->r_tod = gethms(timep, "invalid time of day", FALSE);
	/*
	** Year work.
	*/
	cp = loyearp;
	if ((lp = byword(cp, begin_years)) != NULL) switch ((int) lp->l_value) {
		case YR_MINIMUM:
			rp->r_loyear = min_year;
			break;
		case YR_MAXIMUM:
			rp->r_loyear = max_year;
			break;
		default:	/* "cannot happen" */
			(void) fprintf(stderr,
				"%s: panic: Invalid l_value %d\n",
				progname, lp->l_value);
			exit(1);
	} else if (sscanf(cp, scheck(cp, "%d"), &rp->r_loyear) != 1 ||
		rp->r_loyear < min_year || rp->r_loyear > max_year) {
			if (noise)
				error("invalid starting year");
			if (rp->r_loyear > max_year)
				return;
	}
	cp = hiyearp;
	if ((lp = byword(cp, end_years)) != NULL) switch ((int) lp->l_value) {
		case YR_MINIMUM:
			rp->r_hiyear = min_year;
			break;
		case YR_MAXIMUM:
			rp->r_hiyear = max_year;
			break;
		case YR_ONLY:
			rp->r_hiyear = rp->r_loyear;
			break;
		default:	/* "cannot happen" */
			(void) fprintf(stderr,
				"%s: panic: Invalid l_value %d\n",
				progname, lp->l_value);
			exit(1);
	} else if (sscanf(cp, scheck(cp, "%d"), &rp->r_hiyear) != 1 ||
		rp->r_hiyear < min_year || rp->r_hiyear > max_year) {
			if (noise)
				error("invalid ending year");
			if (rp->r_hiyear < min_year)
				return;
	}
	if (rp->r_hiyear < min_year)
 		return;
 	if (rp->r_loyear < min_year)
 		rp->r_loyear = min_year;
 	if (rp->r_hiyear > max_year)
 		rp->r_hiyear = max_year;
	if (rp->r_loyear > rp->r_hiyear) {
		error("starting year greater than ending year");
		return;
	}
	if (*typep == '\0')
		rp->r_yrtype = NULL;
	else {
		if (rp->r_loyear == rp->r_hiyear) {
			error("typed single year");
			return;
		}
		rp->r_yrtype = ecpyalloc(typep);
	}
	/*
	** Day work.
	** Accept things such as:
	**	1
	**	last-Sunday
	**	Sun<=20
	**	Sun>=7
	*/
	if ((lp = byword(dayp, lasts)) != NULL) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = len_months[1][rp->r_month];
	} else {
		if ((cp = index(dayp, '<')) != 0)
			rp->r_dycode = DC_DOWLEQ;
		else if ((cp = index(dayp, '>')) != 0)
			rp->r_dycode = DC_DOWGEQ;
		else {
			cp = dayp;
			rp->r_dycode = DC_DOM;
		}
		if (rp->r_dycode != DC_DOM) {
			*cp++ = 0;
			if (*cp++ != '=') {
				error("invalid day of month");
				return;
			}
			if ((lp = byword(dayp, wday_names)) == NULL) {
				error("invalid weekday name");
				return;
			}
			rp->r_wday = lp->l_value;
		}
		if (sscanf(cp, scheck(cp, "%d"), &rp->r_dayofmonth) != 1 ||
			rp->r_dayofmonth <= 0 ||
			(rp->r_dayofmonth > len_months[1][rp->r_month])) {
				error("invalid day of month");
				return;
		}
	}
}

static
puttzcode(val, fp)
long	val;
FILE *	fp;
{
	register int	c;
	register int	shift;

	for (shift = 24; shift >= 0; shift -= 8) {
		c = val >> shift;
		(void) putc(c, fp);
	}
}

static
writezone(name)
char *	name;
{
	register FILE *		fp;
	register int		i;
	char			fullname[BUFSIZ];

	if (strlen(directory) + 1 + strlen(name) >= sizeof fullname) {
		(void) fprintf(stderr,
			"%s: File name %s/%s too long\n", progname,
			directory, name);
		exit(1);
	}
	(void) sprintf(fullname, "%s/%s", directory, name);
	if ((fp = fopen(fullname, "w")) == NULL) {
		if (mkdirs(fullname) != 0)
			exit(1);
		if ((fp = fopen(fullname, "w")) == NULL) {
			(void) fprintf(stderr, "%s: Can't create ", progname);
			perror(fullname);
			exit(1);
		}
	}
	(void) fseek(fp, (long) sizeof ((struct tzhead *) 0)->tzh_reserved, 0);
	puttzcode(eitol(timecnt), fp);
	puttzcode(eitol(typecnt), fp);
	puttzcode(eitol(charcnt), fp);
	for (i = 0; i < timecnt; ++i)
		puttzcode((long) ats[i], fp);
	if (timecnt > 0)
		(void) fwrite((char *) types, sizeof types[0],
			(int) timecnt, fp);
	for (i = 0; i < typecnt; ++i) {
		puttzcode((long) gmtoffs[i], fp);
		(void) putc(isdsts[i], fp);
		(void) putc(abbrinds[i], fp);
	}
	if (charcnt != 0)
		(void) fwrite(chars, sizeof chars[0], (int) charcnt, fp);
	if (ferror(fp) || fclose(fp)) {
		(void) fprintf(stderr, "%s: Write error on ", progname);
		perror(fullname);
		exit(1);
	}
}

static
outzone(zpfirst, zonecount)
struct zone *	zpfirst;
{
	register struct zone *		zp;
	register struct rule *		rp;
	register int			i, j;
	register int			usestart, useuntil;
	register time_t			starttime, untiltime;
	register long			gmtoff;
	register long			stdoff;
	register int			year;
	register long			startoff;
	register int			startisdst;
	register int			type;
	char				startbuf[BUFSIZ];

	/*
	** Now. . .finally. . .generate some useful data!
	*/
	timecnt = 0;
	typecnt = 0;
	charcnt = 0;
	/*
	** Two guesses. . .the second may well be corrected later.
	*/
	gmtoff = zpfirst->z_gmtoff;
	stdoff = 0;
	for (i = 0; i < zonecount; ++i) {
		usestart = i > 0;
		useuntil = i < (zonecount - 1);
		zp = &zpfirst[i];
		eat(zp->z_filename, zp->z_linenum);
		startisdst = -1;
		if (zp->z_nrules == 0) {
			type = addtype(oadd(zp->z_gmtoff, zp->z_stdoff),
				zp->z_format, zp->z_stdoff != 0);
			if (usestart)
				addtt(starttime, type);
			gmtoff = zp->z_gmtoff;
			stdoff = zp->z_stdoff;
		} else for (year = min_year; year <= max_year; ++year) {
			if (useuntil && year > zp->z_untilrule.r_hiyear)
				break;
			/*
			** Mark which rules to do in the current year.
			** For those to do, calculate rpytime(rp, year);
			*/
			for (j = 0; j < zp->z_nrules; ++j) {
				rp = &zp->z_rules[j];
				eats(zp->z_filename, zp->z_linenum,
					rp->r_filename, rp->r_linenum);
				rp->r_todo = year >= rp->r_loyear &&
						year <= rp->r_hiyear &&
						yearistype(year, rp->r_yrtype);
				if (rp->r_todo)
					rp->r_temp = rpytime(rp, year);
			}
			for ( ; ; ) {
				register int	k;
				register time_t	jtime, ktime;
				register long	offset;
				char		buf[BUFSIZ];

				if (useuntil) {
					/*
					** Turn untiltime into GMT
					** assuming the current gmtoff and
					** stdoff values.
					*/
					offset = gmtoff;
					if (!zp->z_untilrule.r_todisstd)
						offset = oadd(offset, stdoff);
					untiltime = tadd(zp->z_untiltime,
						-offset);
				}
				/*
				** Find the rule (of those to do, if any)
				** that takes effect earliest in the year.
				*/
				k = -1;
				for (j = 0; j < zp->z_nrules; ++j) {
					rp = &zp->z_rules[j];
					if (!rp->r_todo)
						continue;
					eats(zp->z_filename, zp->z_linenum,
						rp->r_filename, rp->r_linenum);
					offset = gmtoff;
					if (!rp->r_todisstd)
						offset = oadd(offset, stdoff);
					jtime = rp->r_temp;
					if (jtime == min_time ||
						jtime == max_time)
							continue;
					jtime = tadd(jtime, -offset);
					if (k < 0 || jtime < ktime) {
						k = j;
						ktime = jtime;
					}
				}
				if (k < 0)
					break;	/* go on to next year */
				rp = &zp->z_rules[k];
				rp->r_todo = FALSE;
				if (useuntil && ktime >= untiltime)
					break;
				if (usestart) {
					if (ktime < starttime) {
						stdoff = rp->r_stdoff;
						startoff = oadd(zp->z_gmtoff,
							rp->r_stdoff);
						(void) sprintf(startbuf,
							zp->z_format,
							rp->r_abbrvar);
						startisdst =
							rp->r_stdoff != 0;
						continue;
					}
					if (ktime != starttime &&
						startisdst >= 0)
addtt(starttime, addtype(startoff, startbuf, startisdst));
					usestart = FALSE;
				}
				eats(zp->z_filename, zp->z_linenum,
					rp->r_filename, rp->r_linenum);
				(void) sprintf(buf, zp->z_format,
					rp->r_abbrvar);
				offset = oadd(zp->z_gmtoff, rp->r_stdoff);
				type = addtype(offset, buf, rp->r_stdoff != 0);
				if (timecnt != 0 || rp->r_stdoff != 0)
					addtt(ktime, type);
				gmtoff = zp->z_gmtoff;
				stdoff = rp->r_stdoff;
			}
		}
		/*
		** Now we may get to set starttime for the next zone line.
		*/
		if (useuntil)
			starttime = tadd(zp->z_untiltime,
				-gmtoffs[types[timecnt - 1]]);
	}
	writezone(zpfirst->z_name);
}

static
addtt(starttime, type)
time_t	starttime;
{
	if (timecnt != 0 && type == types[timecnt - 1])
		return;	/* easy enough! */
	if (timecnt >= TZ_MAX_TIMES) {
		error("too many transitions?!");
		exit(1);
	}
	ats[timecnt] = starttime;
	types[timecnt] = type;
	++timecnt;
}

static
addtype(gmtoff, abbr, isdst)
long	gmtoff;
char *	abbr;
{
	register int	i, j;

	/*
	** See if there's already an entry for this zone type.
	** If so, just return its index.
	*/
	for (i = 0; i < typecnt; ++i) {
		if (gmtoff == gmtoffs[i] && isdst == isdsts[i] &&
			strcmp(abbr, &chars[abbrinds[i]]) == 0)
				return i;
	}
	/*
	** There isn't one; add a new one, unless there are already too
	** many.
	*/
	if (typecnt >= TZ_MAX_TYPES) {
		error("too many local time types");
		exit(1);
	}
	gmtoffs[i] = gmtoff;
	isdsts[i] = isdst;

	for (j = 0; j < charcnt; ++j)
		if (strcmp(&chars[j], abbr) == 0)
			break;
	if (j == charcnt)
		newabbr(abbr);
	abbrinds[i] = j;
	++typecnt;
	return i;
}

static
yearistype(year, type)
char *	type;
{
	char	buf[BUFSIZ];
	int	result;

	if (type == NULL || *type == '\0')
		return TRUE;
	if (strcmp(type, "uspres") == 0)
		return (year % 4) == 0;
	if (strcmp(type, "nonpres") == 0)
		return (year % 4) != 0;
	(void) sprintf(buf, "yearistype %d %s", year, type);
	result = system(buf);
	if (result == 0)
		return TRUE;
	if (result == 1 << 8)
		return FALSE;
	error("Wild result from command execution");
	(void) fprintf(stderr, "%s: command was '%s', result was %d\n",
		progname, buf, result);
	for ( ; ; )
		exit(1);
}

static
lowerit(a)
{
	return (isascii(a) && isupper(a)) ? tolower(a) : a;
}

static
ciequal(ap, bp)		/* case-insensitive equality */
register char *	ap;
register char *	bp;
{
	while (lowerit(*ap) == lowerit(*bp++))
		if (*ap++ == '\0')
			return TRUE;
	return FALSE;
}

static
isabbr(abbr, word)
register char *	abbr;
register char *	word;
{
	if (lowerit(*abbr) != lowerit(*word))
		return FALSE;
	++word;
	while (*++abbr != '\0')
		do if (*word == '\0')
			return FALSE;
				while (lowerit(*word++) != lowerit(*abbr));
	return TRUE;
}

static struct lookup *
byword(word, table)
register char *			word;
register struct lookup *	table;
{
	register struct lookup *	foundlp;
	register struct lookup *	lp;

	if (word == NULL || table == NULL)
		return NULL;
	/*
	** Look for exact match.
	*/
	for (lp = table; lp->l_word != NULL; ++lp)
		if (ciequal(word, lp->l_word))
			return lp;
	/*
	** Look for inexact match.
	*/
	foundlp = NULL;
	for (lp = table; lp->l_word != NULL; ++lp)
		if (isabbr(word, lp->l_word))
			if (foundlp == NULL)
				foundlp = lp;
			else	return NULL;	/* multiple inexact matches */
	return foundlp;
}

static char **
getfields(cp)
register char *	cp;
{
	register char *		dp;
	register char **	array;
	register int		nsubs;

	if (cp == NULL)
		return NULL;
	array = (char **) emalloc((strlen(cp) + 1) * sizeof *array);
	nsubs = 0;
	for ( ; ; ) {
		while (isascii(*cp) && isspace(*cp))
			++cp;
		if (*cp == '\0' || *cp == '#')
			break;
		array[nsubs++] = dp = cp;
		do {
			if ((*dp = *cp++) != '"')
				++dp;
			else while ((*dp = *cp++) != '"')
				if (*dp != '\0')
					++dp;
				else	error("Odd number of quotation marks");
		} while (*cp != '\0' && *cp != '#' &&
			(!isascii(*cp) || !isspace(*cp)));
		if (isascii(*cp) && isspace(*cp))
			++cp;
		*dp = '\0';
	}
	array[nsubs] = NULL;
	return array;
}

static long
oadd(t1, t2)
long	t1;
long	t2;
{
	register long	t;

	t = t1 + t2;
	if (t2 > 0 && t <= t1 || t2 < 0 && t >= t1) {
		error("time overflow");
		exit(1);
	}
	return t;
}

static time_t
tadd(t1, t2)
time_t	t1;
long	t2;
{
	register time_t	t;

	if (t1 == max_time && t2 > 0)
		return max_time;
	if (t1 == min_time && t2 < 0)
		return min_time;
	t = t1 + t2;
	if (t2 > 0 && t <= t1 || t2 < 0 && t >= t1) {
		error("time overflow");
		exit(1);
	}
	return t;
}

/*
** Given a rule, and a year, compute the date - in seconds since January 1,
** 1970, 00:00 LOCAL time - in that year that the rule refers to.
*/

static time_t
rpytime(rp, wantedy)
register struct rule *	rp;
register int		wantedy;
{
	register int	y, m, i;
	register long	dayoff;			/* with a nod to Margaret O. */
	register time_t	t;

	dayoff = 0;
	m = TM_JANUARY;
	y = EPOCH_YEAR;
	while (wantedy != y) {
		if (wantedy > y) {
			i = len_years[isleap(y)];
			++y;
		} else {
			--y;
			i = -len_years[isleap(y)];
		}
		dayoff = oadd(dayoff, eitol(i));
	}
	while (m != rp->r_month) {
		i = len_months[isleap(y)][m];
		dayoff = oadd(dayoff, eitol(i));
		++m;
	}
	i = rp->r_dayofmonth;
	if (m == TM_FEBRUARY && i == 29 && !isleap(y)) {
		if (rp->r_dycode == DC_DOWLEQ)
			--i;
		else {
			error("use of 2/29 in non leap-year");
			exit(1);
		}
	}
	--i;
	dayoff = oadd(dayoff, eitol(i));
	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		register long	wday;

#define LDAYS_PER_WEEK	((long) DAYS_PER_WEEK)
		wday = eitol(EPOCH_WDAY);
		/*
		** Don't trust mod of negative numbers.
		*/
		if (dayoff >= 0)
			wday = (wday + dayoff) % LDAYS_PER_WEEK;
		else {
			wday -= ((-dayoff) % LDAYS_PER_WEEK);
			if (wday < 0)
				wday += LDAYS_PER_WEEK;
		}
		while (wday != eitol(rp->r_wday))
			if (rp->r_dycode == DC_DOWGEQ) {
				dayoff = oadd(dayoff, (long) 1);
				if (++wday >= LDAYS_PER_WEEK)
					wday = 0;
				++i;
			} else {
				dayoff = oadd(dayoff, (long) -1);
				if (--wday < 0)
					wday = LDAYS_PER_WEEK;
				--i;
			}
		if (i < 0 || i >= len_months[isleap(y)][m]) {
			error("no day in month matches rule");
			exit(1);
		}
	}
	if (dayoff < 0 && !tt_signed) {
		if (wantedy == rp->r_loyear)
			return min_time;
		error("time before zero");
		exit(1);
	}
	t = (time_t) dayoff * SECS_PER_DAY;
	/*
	** Cheap overflow check.
	*/
	if (t / SECS_PER_DAY != dayoff) {
		if (wantedy == rp->r_hiyear)
			return max_time;
		if (wantedy == rp->r_loyear)
			return min_time;
		error("time overflow");
		exit(1);
	}
	return tadd(t, rp->r_tod);
}

static
newabbr(string)
char *	string;
{
	register int	i;

	i = strlen(string) + 1;
	if (charcnt + i >= TZ_MAX_CHARS) {
		error("too many, or too long, time zone abbreviations");
		exit(1);
	}
	(void) strcpy(&chars[charcnt], string);
	charcnt += eitol(i);
}

static
mkdirs(name)
char *	name;
{
	register char *	cp;

	if ((cp = name) == NULL || *cp == '\0')
		return 0;
	while ((cp = index(cp + 1,'/')) != 0) {
		*cp = '\0';
		if (access(name,F_OK) && mkdir(name,0755)) {
			perror(name);
			return -1;
		}
		*cp = '/';
	}
	return 0;
}

static long
eitol(i)
{
	long	l;

	l = i;
	if (i < 0 && l >= 0 || i == 0 && l != 0 || i > 0 && l <= 0) {
		(void) fprintf(stderr, "%s: %d did not sign extend correctly\n",
			progname, i);
		exit(1);
	}
	return l;
}

/*
** UNIX is a registered trademark of AT&T.
*/
