/*
 * Read up the messages in the named files and rehabilitate them into
 * UNIX+ style messages on standard output.
 *
 * +Unix is a trademark of Bell Laboratories.
 * The optional -d flag tests the arpa net to unix date format
 * transformer by reading lines from standard input, modifying them
 * to unix format, and outputing them on standard output.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>

char	*index(), *rindex(), *reform();
long	emitl();
int	errs;
int	dateerrs;
int	dflag;
int	lastblank;

main(argc, argv)
	char **argv;
{
	register char *cp, *fname;

	if (argc < 2) {
		fprintf(stderr, "Usage: rescue name ...\n");
		_exit(1);
	}
	lastblank = 1;
	while (--argc) {
		fname = *++argv;
		if (strcmp(fname, "-d") == 0) {
			dflag++;
			dtest();
			exit(0);
		}
		cp = rindex(fname, '/');
		if (cp == 0)
			cp = fname;
		else
			cp++;
		if (strcmp(cp, "cur") == 0)
			continue;
		if (strcmp(cp, "select") == 0)
			continue;
		if (*cp == '#')
			continue;
		while (*cp)
			if (!isdigit(*cp++)) {
				fprintf(stderr, "%s: not mh message file\n",
				    fname);
				goto blog;
			}
		fix(fname);
blog:		;
	}
	if (dateerrs)
		fprintf(stderr, "%d bad date(s) replaced by current date\n",
		    dateerrs);
	exit(errs);
}

/*
 * Fix the named file by putting together a reasonable header
 * and outputing the stuff onto standard output.
 */
fix(name)
	char name[];
{
	register FILE *in;
	char linebuf[BUFSIZ], from[BUFSIZ], date[BUFSIZ];
	register int inhead;
	register char *cp;
	int i;

	if ((in = fopen(name, "r")) == NULL) {
		perror(name);
		errs++;
		return;
	}
	from[0] = date[0] = 0;
	for (; from[0] == 0 || date[0] == 0;) {
		if (fgets(linebuf, BUFSIZ, in) == NULL)
			goto noheader;
		if (strlen(linebuf) == 1)
			goto noheader;
		if (linebuf[0] == '-')
			goto noheader;
		if (isfield(linebuf, "from")) {
			cp = index(linebuf, ':');
			if (cp == 0)
				goto noheader;
			cp++;
			while (*cp && isspace(*cp))
				cp++;
			/*
			** the below was: strcpy(from, cp);
			** but this will not work, since
			** headers can be like this:
			** From: foo (foo bar)
			** which /usr/ucb/Mail gags on... -layer
			*/
			i = 0;
			while(*cp && !isspace(*cp))
				from[i++] = *cp++;
			from[i] = '\0';
			continue;
		}
		if (isfield(linebuf, "date")) {
			cp = index(linebuf, ':');
			if (cp == 0)
				goto noheader;
			cp++;
			while (*cp && isspace(*cp))
				cp++;
			strcpy(date, cp);
			continue;
		}
	}
	/*
	 * Ready to print the from line
	 */
	zap(from);
	zap(date);
	if (!lastblank)
		printf("\n");
	printf("From %s %s\n", from, reform(date));
	lastblank = 0;
	rewind(in);
	inhead = 1;
	while (fgets(linebuf, BUFSIZ, in) != NULL) {
		if (inhead &&
		    (isfield(linebuf, "from") || isfield(linebuf, "date")))
			continue;
		if (strlen(linebuf) == 1)
			inhead = 0;
		fputs(linebuf, stdout);
		lastblank = 0;
		if (linebuf[0] == '\n' && linebuf[1] == 0)
			lastblank = 1;
	}
	fclose(in);
	return;
noheader:
	fprintf(stderr, "%s:  Missing, bad, or incomplete header\n", name);
	errs++;
	fclose(in);
}

/*
 * Determine if the passed line is a header of the given field name
 */
isfield(buf, field)
	char buf[];
	char field[];
{
	register char *cp, *cp2;

	cp = buf;
	cp2 = field;
	while (lower(*cp++) == lower(*cp2++))
		;
	if (*--cp == ':' && *--cp2 == 0)
		return(1);
	return(0);
}

/*
 * Lower case the given character
 */
lower(c)
	register int c;
{

	if (isupper(c))
		return(tolower(c));
	return(c);
}

/*
 * Remove trailing newline from str, if present.
 */
zap(str)
	char str[];
{
	register char *cp;

	cp = index(str, '\n');
	if (cp != 0)
		*cp = 0;
}

/*
 * Reformat the given Arpa net style date
 * back into a unix ctime(3) date.
 * Unfortunately, there appears to be NO standard arpa net
 * date format.
 */

char *month = "janfebmaraprmayjunjulaugsepoctnovdec";

char *
reform(date)
	char date[];
{
	static char retdate[35];
	char dbuf[BUFSIZ];
	register char *cp, *cp2, *mptr;
	struct tm d;
	long x, then;

	cp = date;
	cp2 = dbuf;
	while (*cp) {
		if (*cp == '(') {
			while (*cp != ')' && *cp)
				cp++;
			if (*cp)
				cp++;
			goto more;
		}
		if (*cp == '\t') {
			*cp2++ = ' ';
			cp++;
			continue;
		}
		if (*cp == '-') {
			*cp2++ = ' ';
			cp++;
			continue;
		}
		if (isupper(*cp)) {
			*cp2++ = tolower(*cp++);
			continue;
		}
		*cp2++ = *cp++;
more:		;
	}
	*cp2 = 0;
	/*
	 * Okie dokie.  Now pick off the date part and store
	 * it away.  Only possible formats here are:
	 *	mm/dd/yy
	 * and
	 *	dd monthname year
	 */
	if (index(dbuf, '/')) {
		d.tm_mon = atoi(dbuf) - 1;
		cp = index(dbuf, '/') + 1;
		d.tm_mday = atoi(cp);
		if ((cp = index(cp, '/')) == 0)
			goto baddate;
		cp++;
		d.tm_year = atoi(cp);
		if (d.tm_year > 1900)
			d.tm_year -= 1900;
		cp = index(cp, ' ');
		if (cp == 0)
			goto baddate;
	}
	else {
		d.tm_mday = atoi(dbuf);
		cp = index(dbuf, ' ');
		if (cp == 0)
			goto baddate;
		while (*cp && isspace(*cp))
			cp++;
		if (*cp == 0)
			goto baddate;
		for (mptr = month; *mptr; mptr += 3)
			if (strcmpn(mptr, cp, 3) == 0)
				break;
		if (*mptr == 0)
			goto baddate;
		d.tm_mon = (mptr - month)/3;
		cp = index(cp, ' ');
		if (cp == 0)
			goto baddate;
		while (*cp && isspace(*cp))
			cp++;
		if (*cp == 0)
			goto baddate;
		d.tm_year = atoi(cp);
		if (d.tm_year > 1900)
			d.tm_year -= 1900;
		cp = index(cp, ' ');
		if (cp == 0)
			goto baddate;
	}
	/*
	 * Got the month part, now fix up the time.
	 * Possibilities are:
	 *	hh:mm
	 *	hh:mm [am|pm]
	 *	hhmm edt
	 *	hh:mm:ss edt
	 * Basically, we lose by ignoring time zone.
	 */
	for (;;) {
		while (*cp && isspace(*cp))
			cp++;
		if (*cp == 0)
			goto baddate;
		if (strcmpn(cp, "at ", 3) != 0)
			break;
		cp += 3;
	}
	d.tm_sec = 0;
	if (index(cp, ':')) {
		d.tm_hour = atoi(cp);
		cp = index(cp, ':') + 1;
		d.tm_min = atoi(cp);
		if (index(cp, ':')) {
			cp = index(cp, ':') + 1;
			d.tm_sec = atoi(cp);
		}
		while (*cp && !isspace(*cp))
			cp++;
		while (*cp && isspace(*cp))
			cp++;
		if (strcmpn(cp, "pm", 2) == 0 && d.tm_hour < 12)
			d.tm_hour += 12;
	}
	else {
		x = atoi(cp);
		d.tm_hour = x / 100;
		d.tm_min = x % 100;
	}
	then = emitl(&d);
	strcpy(retdate, ctime(&then));
	zap(retdate);
	return(retdate);

baddate:
	dateerrs++;
	if (dflag)
		strcpy(retdate, "************************");
	else {
		then = time(0);
		strcpy(retdate, ctime(&then));
		zap(retdate);
	}
	return(retdate);
}

/*
 * Test the arpa net to UNIX date modifier.
 * Reads lines from standard input, converting them
 * to unix format, and displaying both on stdout.
 */
dtest()
{
	char buf[BUFSIZ];
	register char *cp;

	while (gets(buf)) {
		cp = reform(buf);
		printf("\"%s\"	\"%s\"\n", buf, cp);
	}
}

/*
 * Routine to convert a localtime(3) format date back into
 * a system format date.
 *
 * Hats off to Bob Kridle for the insight that the way to do
 * this is by binary search of the system date space.
 */

struct tm *localtime();

long
emitl(dp)
	struct tm *dp;
{
	long conv;
	register int i, bit;
	struct tm dcopy;

	dcopy = *dp;
	dp = &dcopy;
	conv = 0;
	for (i = 31; i >= 0; i--) {
		bit = 1 << i;
		conv |= bit;
		if (dcmp(localtime(&conv), dp) > 0)
			conv &= ~bit;
	}
	return(conv);
}

/*
 * Compare two localtime dates, return result.
 */

#define DECIDE(a) \
	if (dp->a > dp2->a) \
		return(1); \
	if (dp->a < dp2->a) \
		return(-1)

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
