/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.1 (Berkeley) %G%";
#endif not lint

#include "rcv.h"

/*
 * Mail -- a mail program
 *
 * Routines for processing and detecting headlines.
 */

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
	if (strncmp("From ", cp, 5) != 0)
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

	if (1 /*value("debug") == NOSTR*/)
		return;
	fprintf(stderr, "\"%s\"\nnot a header because %s\n", linebuf, reason);
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
	char word[LINESIZE];

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
	if (!equal(word, ""))
		hl->l_from = copyin(word, &sp);
	if (strncmp(dp, "tty", 3) == 0) {
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
			if (c < 'a' || c > 'z')
				return(0);
			break;

		case U:
			if (c < 'A' || c > 'Z')
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
 * Test to see if the character is an ascii alphabetic.
 */

isalpha(c)
{
	register int ch;

	ch = raise(c);
	return(ch >= 'A' && ch <= 'Z');
}

/*
 * Test to see if the character is an ascii digit.
 */

isdigit(c)
{
	return(c >= '0' && c <= '9');
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
