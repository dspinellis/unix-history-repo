#ifndef lint
static char sccsid[] = "@(#)unixtomh.c	5.1 86/11/25";
#endif not lint

/*
 * This program copies the mail file in standard unix format
 * given as $1 to the file $2 in Rand Message Handler format.
 * The change made is to bracket each message with a line
 * containing 4 control-A's and to split the From line into
 * a From: field and a Date: field, with the date in Arpanet
 * standard format.
 *
 * This program is designed to be called from the rand mh program
 * ``inc''
 *
 * Set SENDMAIL if you are running sendmail -- this guarantees that
 * From: and Date: lines will appear already, and will put the info
 * in the UNIX-From line into a Received-From: field.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <ctype.h>

#define SENDMAIL

struct headline {
	char	*l_from;	/* The name of the sender */
	char	*l_tty;		/* His tty string (if any) */
	char	*l_date;	/* The entire date string */
};

char *savestr(), *copyin(), *copy(), *nextword(), *calloc();
char *index();

#define	NOSTR		((char *) 0)
#define	UUCP			/* Undo strange uucp naming */

main(argc, argv)
	char **argv;
{
	char linebuf[BUFSIZ];
	register int maybe;
	register FILE *inf, *outf;
	int inhdr, infld;

	if (argc > 3) {
		fprintf(stderr, "Usage: unixtomh name1 name2\n");
		exit(1);
	}
	outf = inf = NULL;
	if (argc < 3)
		outf = stdout;
	if (argc < 2)
		inf = stdin;
	if (inf == NULL && (inf = fopen(argv[1], "r")) == NULL) {
		perror(argv[1]);
		exit(1);
	}
	if (outf == NULL && (outf = fopen(argv[2], "w")) == NULL) {
		perror(argv[2]);
		exit(1);
	}
	maybe = 1;
	inhdr = 0;
	infld = 0;
	while (nullgets(linebuf, BUFSIZ, inf) > 0) {
		if (maybe && ishead(linebuf)) {
			fputs("\1\1\1\1\n", outf);
			inhdr++;
			dohead(linebuf, inf, outf);
			continue;
		}
		if (strlen(linebuf) == 0) {
			maybe = 1;
			inhdr = 0;
			infld = 0;
			putc('\n', outf);
			continue;
		}
		else
			maybe = 0;
#ifndef SENDMAIL
		if (inhdr && strcmpn(linebuf, "Date: ", 6) == 0)
			continue;
		if (inhdr && strcmpn(linebuf, "From: ", 6) == 0)
			continue;
#endif SENDMAIL
		if (infld && isspace(linebuf[0])) {
			fputs(linebuf, outf);
			putc('\n', outf);
			continue;
		}
		if (inhdr && !isspace(linebuf[0])) {
			char *colp, *sp;

			colp = index(linebuf, ':');
			sp = index(linebuf, ' ');
			if (colp == NOSTR || sp == NOSTR || sp < colp) {
				putc('\n', outf);
				inhdr = 0;
			}
			else
				infld = 1;
		}
		fputs(linebuf, outf);
		putc('\n', outf);
	}
	fputs("\1\1\1\1\n", outf);
	fflush(outf);
	if (ferror(outf)) {
		fprintf(stderr, "unixtomh: write: ");
		perror(argv[2]);
		exit(1);
	}
	exit(0);
}

/*
 * Get a line from the given file descriptor, don't return the
 * terminating newline.
 */

nullgets(linebuf, sz, file)
	char linebuf[];
	register FILE *file;
{
	register char *cp;
	register int c, cnt;

	cp = linebuf;
	cnt = sz;
	do {
		if (--cnt <= 0) {
			*cp = 0;
			return(1);
		}
		c = getc(file);
		*cp++ = c;
	} while (c != EOF && c != '\n');
	if (c == EOF && cp == linebuf+1)
		return(0);
	*--cp = 0;
	return(1);
}

/*
 * Output the fields extracted from the From line --
 * From: and Date:  Untangle UUCP stuff if appropriate.
 */

dohead(line, infile, outfile)
	char line[];
	register FILE *infile, *outfile;
{
	register char *cp;
	struct headline hl;
	char parbuf[BUFSIZ];
#ifdef UUCP
	char *word();
	char namebuf[BUFSIZ];
	char linebuf[BUFSIZ];
	int first;
	long curoff;
#endif UUCP

	parse(line, &hl, parbuf);
#ifndef SENDMAIL
	putdate(hl.l_date, outfile);
#endif SENDMAIL
#ifdef UUCP
	if (strcmp(hl.l_from, "uucp") == 0) {
		strcpy(namebuf, "");
		first = 1;
		for (;;) {
			curoff = ftell(infile);
			if (fgets(linebuf, BUFSIZ, infile) == NULL)
				break;
			if (strcmp(word(1, linebuf), ">From") != 0)
				break;
			if (strcmp(word(-3, linebuf), "remote") != 0)
				break;
			if (strcmp(word(-2, linebuf), "from") != 0)
				break;
			if (first) {
				strcpy(namebuf, word(-1, linebuf));
				strcat(namebuf, "!");
				strcat(namebuf, word(2, linebuf));
				first = 0;
			}
			else {
				strcpy(rindex(namebuf, '!')+1,
				    word(-1, linebuf));
				strcat(namebuf, "!");
				strcat(namebuf, word(2, linebuf));
			}
		}
		fseek(infile, curoff, 0);
#ifdef SENDMAIL
		if (!first)
			fprintf(outfile, "Return-Path: <%s>\n", namebuf);
#else SENDMAIL
		if (first)
			fprintf(outfile, "From: uucp\n");
		else
			fprintf(outfile, "From: %s\n", namebuf);
#endif SENDMAIL
		return;
	}
#endif UUCP
#ifdef SENDMAIL
	if (hl.l_from[0] == '<')
		fprintf(outfile, "Return-Path: %s\n", hl.l_from);
	else
		fprintf(outfile, "Return-Path: <%s>\n", hl.l_from);
#else SENDMAIL
	fprintf(outfile, "From: %s\n", hl.l_from);
#endif SENDMAIL
}

#ifdef UUCP

/*
 * Return liberal word i from the given string.
 * The words are numbered 1, 2, 3, . . .  from the left
 * and -1, -2, . . . from the right.
 */

char *
word(index, str)
	char str[];
{
	register char *cp;
	char *secbuf;
	register int c;
	static char retbuf[100];
	char *gword();

	cp = str;
	if ((c = index) > 0) {
		while (c-- > 0)
			cp = gword(cp, retbuf);
		return(retbuf);
	}
	if (c == 0)
		return("");
	secbuf = (char *) alloca(strlen(str) + 1);
	strcpy(secbuf, str);
	rev(secbuf);
	cp = word(-index, secbuf);
	rev(cp);
	return(cp);
}

/*
 * Skip leading blanks in the string, return
 * first liberal word collected.
 */

char *
gword(cp, buf)
	register char *cp;
	char buf[];
{
	register char *cp2;

	cp2 = buf;
	while (*cp && any(*cp, " \t\n"))
		cp++;
	while (*cp && !any(*cp, " \t\n"))
		*cp2++ = *cp++;
	*cp2 = 0;
	return(cp);
}

/*
 * Reverse the characters in the string in place
 */

rev(str)
	char str[];
{
	register char *cpl, *cpr;
	register int s;

	s = strlen(str);
	cpl = str;
	cpr = &str[s-1];
	while (cpl < cpr) {
		s = *cpl;
		*cpl++ = *cpr;
		*cpr-- = s;
	}
}
#endif UUCP

/*
 * Save a string in dynamic space.
 * This little goodie is needed for
 * a headline detector in head.c
 */

char *
savestr(str)
	char str[];
{
	register char *top;

	top = calloc(strlen(str) + 1, 1);
	if (top == NOSTR) {
		fprintf(stderr, "unixtomh:  Ran out of memory\n");
		exit(1);
	}
	copy(str, top);
	return(top);
}

/*
 * See if the passed line buffer is a mail header.
 * Return true if yes.  Note the extreme pains to
 * accomodate all funny formats.
 */

ishead(linebuf)
	char linebuf[];
{
	register char *cp;
	struct headline hl;
	char parbuf[BUFSIZ];

	cp = linebuf;
	if (!isname("From ", cp, 5))
		return(0);
	parse(cp, &hl, parbuf);
	if (hl.l_from == NOSTR || hl.l_date == NOSTR) {
		fail(linebuf, "No from or date field");
		return(0);
	}
	if (!isdate(hl.l_date)) {
		fail(linebuf, "Date field not legal date");
		return(0);
	}
	
	/*
	 * I guess we got it!
	 */

	return(1);
}

fail(linebuf, reason)
	char linebuf[], reason[];
{
	return;
}

/*
 * Split a headline into its useful components.
 * Copy the line into dynamic string space, then set
 * pointers into the copied line in the passed headline
 * structure.  Actually, it scans.
 */

parse(line, hl, pbuf)
	char line[], pbuf[];
	struct headline *hl;
{
	register char *cp, *dp;
	char *sp;
	char word[BUFSIZ];

	hl->l_from = NOSTR;
	hl->l_tty = NOSTR;
	hl->l_date = NOSTR;
	cp = line;
	sp = pbuf;

	/*
	 * Skip the first "word" of the line, which should be "From"
	 * anyway.
	 */

	cp = nextword(cp, word);
	dp = nextword(cp, word);
	if (word[0] != 0)
		hl->l_from = copyin(word, &sp);
	if (isname(dp, "tty", 3)) {
		cp = nextword(dp, word);
		hl->l_tty = copyin(word, &sp);
		if (cp != NOSTR)
			hl->l_date = copyin(cp, &sp);
	}
	else
		if (dp != NOSTR)
			hl->l_date = copyin(dp, &sp);
}

/*
 * Copy the string on the left into the string on the right
 * and bump the right (reference) string pointer by the length.
 * Thus, dynamically allocate space in the right string, copying
 * the left string into it.
 */

char *
copyin(src, space)
	char src[];
	char **space;
{
	register char *cp, *top;
	register int s;

	s = strlen(src);
	cp = *space;
	top = cp;
	strcpy(cp, src);
	cp += s + 1;
	*space = cp;
	return(top);
}

/*
 * See if the two passed strings agree in the first n characters.
 * Return true if they do, gnu.
 */

isname(as1, as2, acount)
	char *as1, *as2;
{
	register char *s1, *s2;
	register count;

	s1 = as1;
	s2 = as2;
	count = acount;
	if (count > 0)
		do
			if (*s1++ != *s2++)
				return(0);
		while (--count);
	return(1);
}

/*
 * Test to see if the passed string is a ctime(3) generated
 * date string as documented in the manual.  The template
 * below is used as the criterion of correctness.
 * Also, we check for a possible trailing time zone using
 * the auxtype template.
 */

#define	L	1		/* A lower case char */
#define	S	2		/* A space */
#define	D	3		/* A digit */
#define	O	4		/* An optional digit or space */
#define	C	5		/* A colon */
#define	N	6		/* A new line */
#define U	7		/* An upper case char */

char ctypes[] = {U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,D,D,D,D,0};
char tmztypes[] = {U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,U,U,U,S,D,D,D,D,0};

isdate(date)
	char date[];
{
	register char *cp;

	cp = date;
	if (cmatch(cp, ctypes))
		return(1);
	return(cmatch(cp, tmztypes));
}

/*
 * Match the given string against the given template.
 * Return 1 if they match, 0 if they don't
 */

cmatch(str, temp)
	char str[], temp[];
{
	register char *cp, *tp;
	register int c;

	cp = str;
	tp = temp;
	while (*cp != '\0' && *tp != 0) {
		c = *cp++;
		switch (*tp++) {
		case L:
			if (!islower(c))
				return(0);
			break;

		case S:
			if (c != ' ')
				return(0);
			break;

		case D:
			if (!isdigit(c))
				return(0);
			break;

		case O:
			if (c != ' ' && !isdigit(c))
				return(0);
			break;

		case C:
			if (c != ':')
				return(0);
			break;

		case N:
			if (c != '\n')
				return(0);
			break;

		case U:
			if (!isupper(c))
				return(0);
			break;
		}
	}
	if (*cp != '\0' || *tp != 0)
		return(0);
	return(1);
}

/*
 * Collect a liberal (space, tab delimited) word into the word buffer
 * passed.  Also, return a pointer to the next word following that,
 * or NOSTR if none follow.
 */

char *
nextword(wp, wbuf)
	char wp[], wbuf[];
{
	register char *cp, *cp2;

	if ((cp = wp) == NOSTR) {
		copy("", wbuf);
		return(NOSTR);
	}
	cp2 = wbuf;
	while (!any(*cp, " \t") && *cp != '\0')
		if (*cp == '"') {
 			*cp2++ = *cp++;
 			while (*cp != '\0' && *cp != '"')
 				*cp2++ = *cp++;
 			if (*cp == '"')
 				*cp2++ = *cp++;
 		} else
 			*cp2++ = *cp++;
	*cp2 = '\0';
	while (any(*cp, " \t"))
		cp++;
	if (*cp == '\0')
		return(NOSTR);
	return(cp);
}

/*
 * Copy str1 to str2, return pointer to null in str2.
 */

char *
copy(str1, str2)
	char *str1, *str2;
{
	register char *s1, *s2;

	s1 = str1;
	s2 = str2;
	while (*s1)
		*s2++ = *s1++;
	*s2 = 0;
	return(s2);
}

/*
 * Is ch any of the characters in str?
 */

any(ch, str)
	char *str;
{
	register char *f;
	register c;

	f = str;
	c = ch;
	while (*f)
		if (c == *f++)
			return(1);
	return(0);
}

/*
 * Convert lower case letters to upper case.
 */

raise(c)
	register int c;
{
	if (c >= 'a' && c <= 'z')
		c += 'A' - 'a';
	return(c);
}
