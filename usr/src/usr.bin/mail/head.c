/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.5 (Berkeley) %G%";
#endif /* not lint */

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
	if (*cp++ != 'F' || *cp++ != 'r' || *cp++ != 'o' || *cp++ != 'm' ||
	    *cp++ != ' ')
		return (0);
	parse(linebuf, &hl, parbuf);
	if (hl.l_from == NOSTR || hl.l_date == NOSTR) {
		fail(linebuf, "No from or date field");
		return (0);
	}
	if (!isdate(hl.l_date)) {
		fail(linebuf, "Date field not legal date");
		return (0);
	}
	/*
	 * I guess we got it!
	 */
	return (1);
}

/*ARGSUSED*/
fail(linebuf, reason)
	char linebuf[], reason[];
{

	/*
	if (value("debug") == NOSTR)
		return;
	fprintf(stderr, "\"%s\"\nnot a header because %s\n", linebuf, reason);
	*/
}

/*
 * Split a headline into its useful components.
 * Copy the line into dynamic string space, then set
 * pointers into the copied line in the passed headline
 * structure.  Actually, it scans.
 */
parse(line, hl, pbuf)
	char line[], pbuf[];
	register struct headline *hl;
{
	register char *cp;
	char *sp;
	char word[LINESIZE];

	hl->l_from = NOSTR;
	hl->l_tty = NOSTR;
	hl->l_date = NOSTR;
	cp = line;
	sp = pbuf;
	/*
	 * Skip over "From" first.
	 */
	cp = nextword(cp, word);
	cp = nextword(cp, word);
	if (*word)
		hl->l_from = copyin(word, &sp);
	if (cp != NOSTR && cp[0] == 't' && cp[1] == 't' && cp[2] == 'y') {
		cp = nextword(cp, word);
		hl->l_tty = copyin(word, &sp);
	}
	if (cp != NOSTR)
		hl->l_date = copyin(cp, &sp);
}

/*
 * Copy the string on the left into the string on the right
 * and bump the right (reference) string pointer by the length.
 * Thus, dynamically allocate space in the right string, copying
 * the left string into it.
 */
char *
copyin(src, space)
	register char *src;
	char **space;
{
	register char *cp;
	char *top;

	top = cp = *space;
	while (*cp++ = *src++)
		;
	*space = cp;
	return (top);
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

char ctypes[] = { U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,D,D,D,D,0 };
char tmztypes[] = { U,L,L,S,U,L,L,S,O,D,S,D,D,C,D,D,C,D,D,S,U,U,U,S,D,D,D,D,0 };

isdate(date)
	char date[];
{

	if (cmatch(date, ctypes))
		return (1);
	return (cmatch(date, tmztypes));
}

/*
 * Match the given string (cp) against the given template (tp).
 * Return 1 if they match, 0 if they don't
 */

cmatch(cp, tp)
	register char *cp, *tp;
{

	while (*cp && *tp)
		switch (*tp++) {
		case L:
			if (!islower(*cp++))
				return 0;
			break;
		case U:
			if (!isupper(*cp++))
				return 0;
			break;
		case S:
			if (*cp++ != ' ')
				return 0;
			break;
		case D:
			if (!isdigit(*cp++))
				return 0;
			break;
		case O:
			if (*cp != ' ' && !isdigit(*cp))
				return 0;
			cp++;
			break;
		case C:
			if (*cp++ != ':')
				return 0;
			break;
		case N:
			if (*cp++ != '\n')
				return 0;
			break;
		}
	if (*cp || *tp)
		return 0;
	return (1);
}

/*
 * Collect a liberal (space, tab delimited) word into the word buffer
 * passed.  Also, return a pointer to the next word following that,
 * or NOSTR if none follow.
 */
char *
nextword(wp, wbuf)
	register char *wp, *wbuf;
{
	register c;

	if (wp == NOSTR) {
		*wbuf = 0;
		return (NOSTR);
	}
	while ((c = *wp++) && c != ' ' && c != '\t') {
		*wbuf++ = c;
		if (c == '"') {
 			while ((c = *wp++) && c != '"')
 				*wbuf++ = c;
 			if (c == '"')
 				*wbuf++ = c;
			else
				wp--;
 		}
	}
	*wbuf = '\0';
	for (; c == ' ' || c == '\t'; c = *wp++)
		;
	if (c == 0)
		return (NOSTR);
	return (wp - 1);
}

/*
 * Is c contained in s?
 */
any(c, s)
	register c;
	register char *s;
{

	while (*s)
		if (*s++ == c)
			return 1;
	return 0;
}
