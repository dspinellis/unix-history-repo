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
static char sccsid[] = "@(#)aux.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include "rcv.h"
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Auxiliary functions.
 */

/*
 * Return a pointer to a dynamic copy of the argument.
 */

char *
savestr(str)
	char *str;
{
	register char *cp, *cp2, *top;

	for (cp = str; *cp; cp++)
		;
	top = salloc(cp-str + 1);
	if (top == NOSTR)
		return(NOSTR);
	for (cp = str, cp2 = top; *cp; cp++)
		*cp2++ = *cp;
	*cp2 = 0;
	return(top);
}

/*
 * Announce a fatal error and die.
 */

/*VARARGS1*/
panic(fmt, a, b)
	char *fmt;
{
	fprintf(stderr, "panic: ");
	fprintf(stderr, fmt, a, b);
	putc('\n', stderr);
	exit(1);
}

/*
 * Touch the named message by setting its MTOUCH flag.
 * Touched messages have the effect of not being sent
 * back to the system mailbox on exit.
 */

touch(mesg)
{
	register struct message *mp;

	if (mesg < 1 || mesg > msgCount)
		return;
	mp = &message[mesg-1];
	mp->m_flag |= MTOUCH;
	if ((mp->m_flag & MREAD) == 0)
		mp->m_flag |= MREAD|MSTATUS;
}

/*
 * Test to see if the passed file name is a directory.
 * Return true if it is.
 */

isdir(name)
	char name[];
{
	struct stat sbuf;

	if (stat(name, &sbuf) < 0)
		return(0);
	return((sbuf.st_mode & S_IFMT) == S_IFDIR);
}

/*
 * Count the number of arguments in the given string raw list.
 */

argcount(argv)
	char **argv;
{
	register char **ap;

	for (ap = argv; *ap++ != NOSTR;)
		;	
	return ap - argv - 1;
}

/*
 * Return the desired header line from the passed message
 * pointer (or NOSTR if the desired header field is not available).
 */

char *
hfield(field, mp)
	char field[];
	struct message *mp;
{
	register FILE *ibuf;
	char linebuf[LINESIZE];
	register int lc;
	register char *hfield;
	char *colon;

	ibuf = setinput(mp);
	if ((lc = mp->m_lines - 1) < 0)
		return NOSTR;
	if (readline(ibuf, linebuf) < 0)
		return NOSTR;
	while (lc > 0) {
		if ((lc = gethfield(ibuf, linebuf, lc, &colon)) < 0)
			return NOSTR;
		if (hfield = ishfield(linebuf, colon, field))
			return savestr(hfield);
	}
	return NOSTR;
}

/*
 * Return the next header field found in the given message.
 * Return >= 0 if something found, < 0 elsewise.
 * "colon" is set to point to the colon in the header.
 * Must deal with \ continuations & other such fraud.
 */

gethfield(f, linebuf, rem, colon)
	register FILE *f;
	char linebuf[];
	register int rem;
	char **colon;
{
	char line2[LINESIZE];
	register char *cp, *cp2;
	register int c;

	for (;;) {
		if (--rem < 0)
			return -1;
		if ((c = readline(f, linebuf)) <= 0)
			return -1;
		for (cp = linebuf; isprint(*cp) && *cp != ' ' && *cp != ':';
		     cp++)
			;
		if (*cp != ':' || cp == linebuf)
			continue;
		/*
		 * I guess we got a headline.
		 * Handle wraparounding
		 */
		*colon = cp;
		cp = linebuf + c;
		for (;;) {
			while (--cp >= linebuf && (*cp == ' ' || *cp == '\t'))
				;
			cp++;
			if (rem <= 0)
				break;
			ungetc(c = getc(f), f);
			if (c != ' ' && c != '\t')
				break;
			if ((c = readline(f, line2)) < 0)
				break;
			rem--;
			for (cp2 = line2; *cp2 == ' ' || *cp2 == '\t'; cp2++)
				;
			c -= cp2 - line2;
			if (cp + c >= linebuf + LINESIZE - 2)
				break;
			*cp++ = ' ';
			bcopy(cp2, cp, c);
			cp += c;
		}
		*cp = 0;
		return rem;
	}
	/* NOTREACHED */
}

/*
 * Check whether the passed line is a header line of
 * the desired breed.  Return the field body, or 0.
 */

char*
ishfield(linebuf, colon, field)
	char linebuf[], field[];
	char *colon;
{
	register char *cp = colon;

	*cp = 0;
	if (!icequal(linebuf, field)) {
		*cp = ':';
		return 0;
	}
	*cp = ':';
	for (cp++; *cp == ' ' || *cp == '\t'; cp++)
		;
	return cp;
}

/*
 * Compare two strings, ignoring case.
 */

icequal(s1, s2)
	register char *s1, *s2;
{
	register c1, c2;

	for (;;) {
		if ((c1 = (unsigned char)*s1++) !=
		    (c2 = (unsigned char)*s2++)) {
			if (isupper(c1))
				c1 = tolower(c1);
			if (c1 != c2)
				return 0;
		}
		if (c1 == 0)
			return 1;
	}
	/*NOTREACHED*/
}

/*
 * Copy a string, lowercasing it as we go.
 */
istrcpy(dest, src)
	register char *dest, *src;
{

	do {
		if (isupper(*src))
			*dest++ = tolower(*src);
		else
			*dest++ = *src;
	} while (*src++ != 0);
}

/*
 * The following code deals with input stacking to do source
 * commands.  All but the current file pointer are saved on
 * the stack.
 */

static	int	ssp = -1;		/* Top of file stack */
struct sstack {
	FILE	*s_file;		/* File we were in. */
	int	s_cond;			/* Saved state of conditionals */
	int	s_loading;		/* Loading .mailrc, etc. */
} sstack[NOFILE];

/*
 * Pushdown current input file and switch to a new one.
 * Set the global flag "sourcing" so that others will realize
 * that they are no longer reading from a tty (in all probability).
 */

source(name)
	char name[];
{
	register FILE *fi;
	register char *cp;

	if ((cp = expand(name)) == NOSTR)
		return(1);
	if ((fi = fopen(cp, "r")) == NULL) {
		perror(cp);
		return(1);
	}
	if (ssp >= NOFILE - 2) {
		printf("Too much \"sourcing\" going on.\n");
		fclose(fi);
		return(1);
	}
	sstack[++ssp].s_file = input;
	sstack[ssp].s_cond = cond;
	sstack[ssp].s_loading = loading;
	loading = 0;
	cond = CANY;
	input = fi;
	sourcing++;
	return(0);
}

/*
 * Pop the current input back to the previous level.
 * Update the "sourcing" flag as appropriate.
 */

unstack()
{
	if (ssp < 0) {
		printf("\"Source\" stack over-pop.\n");
		sourcing = 0;
		return(1);
	}
	fclose(input);
	if (cond != CANY)
		printf("Unmatched \"if\"\n");
	cond = sstack[ssp].s_cond;
	loading = sstack[ssp].s_loading;
	input = sstack[ssp--].s_file;
	if (ssp < 0)
		sourcing = loading;
	return(0);
}

/*
 * Touch the indicated file.
 * This is nifty for the shell.
 */

alter(name)
	char name[];
{
	struct stat statb;
	long time();
	time_t time_p[2];

	if (stat(name, &statb) < 0)
		return;
	time_p[0] = time((long *) 0) + 1;
	time_p[1] = statb.st_mtime;
	utime(name, time_p);
}

/*
 * Examine the passed line buffer and
 * return true if it is all blanks and tabs.
 */

blankline(linebuf)
	char linebuf[];
{
	register char *cp;

	for (cp = linebuf; *cp; cp++)
		if (*cp != ' ' && *cp != '\t')
			return(0);
	return(1);
}

/*
 * Get sender's name from this message.  If the message has
 * a bunch of arpanet stuff in it, we may have to skin the name
 * before returning it.
 */
char *
nameof(mp, reptype)
	register struct message *mp;
{
	register char *cp, *cp2;

	cp = skin(name1(mp, reptype));
	if (reptype != 0 || charcount(cp, '!') < 2)
		return(cp);
	cp2 = rindex(cp, '!');
	cp2--;
	while (cp2 > cp && *cp2 != '!')
		cp2--;
	if (*cp2 == '!')
		return(cp2 + 1);
	return(cp);
}

/*
 * Skin an arpa net address according to the RFC 822 interpretation
 * of "host-phrase."
 */
char *
skin(name)
	char *name;
{
	register int c;
	register char *cp, *cp2;
	char *bufend;
	int gotlt, lastsp;
	char nbuf[BUFSIZ];
	int nesting;

	if (name == NOSTR)
		return(NOSTR);
	if (index(name, '(') == NOSTR && index(name, '<') == NOSTR
	    && index(name, ' ') == NOSTR)
		return(name);
	gotlt = 0;
	lastsp = 0;
	bufend = nbuf;
	for (cp = name, cp2 = bufend; c = *cp++; ) {
		switch (c) {
		case '(':
			/*
			 * Start of a "comment".
			 * Ignore it.
			 */
			nesting = 1;
			while ((c = *cp) != 0) {
				cp++;
				switch (c) {
				case '\\':
					if (*cp == 0)
						goto outcm;
					cp++;
					break;
				case '(':
					nesting++;
					break;

				case ')':
					--nesting;
					break;
				}

				if (nesting <= 0)
					break;
			}
		outcm:
			lastsp = 0;
			break;

		case '"':
			/*
			 * Start of a "quoted-string".
			 * Copy it in its entirety.
			 */
			while ((c = *cp) != 0) {
				cp++;
				switch (c) {
				case '\\':
					if ((c = *cp) == 0)
						goto outqs;
					cp++;
					break;
				case '"':
					goto outqs;
				}
				*cp2++ = c;
			}
		outqs:
			lastsp = 0;
			break;

		case ' ':
			if (cp[0] == 'a' && cp[1] == 't' && cp[2] == ' ')
				cp += 3, *cp2++ = '@';
			else
			if (cp[0] == '@' && cp[1] == ' ')
				cp += 2, *cp2++ = '@';
			else
				lastsp = 1;
			break;

		case '<':
			cp2 = bufend;
			gotlt++;
			lastsp = 0;
			break;

		case '>':
			if (gotlt) {
				gotlt = 0;
				while (*cp != ',' && *cp != 0)
					cp++;
				if (*cp == 0 )
					goto done;
				*cp2++ = ',';
				*cp2++ = ' ';
				bufend = cp2;
				break;
			}

			/* Fall into . . . */

		default:
			if (lastsp) {
				lastsp = 0;
				*cp2++ = ' ';
			}
			*cp2++ = c;
			break;
		}
	}
done:
	*cp2 = 0;

	return(savestr(nbuf));
}

/*
 * Fetch the sender's name from the passed message.
 * Reptype can be
 *	0 -- get sender's name for display purposes
 *	1 -- get sender's name for reply
 *	2 -- get sender's name for Reply
 */

char *
name1(mp, reptype)
	register struct message *mp;
{
	char namebuf[LINESIZE];
	char linebuf[LINESIZE];
	register char *cp, *cp2;
	register FILE *ibuf;
	int first = 1;

	if ((cp = hfield("from", mp)) != NOSTR)
		return cp;
	if (reptype == 0 && (cp = hfield("sender", mp)) != NOSTR)
		return cp;
	ibuf = setinput(mp);
	namebuf[0] = 0;
	if (readline(ibuf, linebuf) < 0)
		return(savestr(namebuf));
newname:
	for (cp = linebuf; *cp && *cp != ' '; cp++)
		;
	for (; *cp == ' ' || *cp == '\t'; cp++)
		;
	for (cp2 = &namebuf[strlen(namebuf)];
	     *cp && *cp != ' ' && *cp != '\t' && cp2 < namebuf + LINESIZE - 1;)
		*cp2++ = *cp++;
	*cp2 = '\0';
	if (readline(ibuf, linebuf) < 0)
		return(savestr(namebuf));
	if ((cp = index(linebuf, 'F')) == NULL)
		return(savestr(namebuf));
	if (strncmp(cp, "From", 4) != 0)
		return(savestr(namebuf));
	while ((cp = index(cp, 'r')) != NULL) {
		if (strncmp(cp, "remote", 6) == 0) {
			if ((cp = index(cp, 'f')) == NULL)
				break;
			if (strncmp(cp, "from", 4) != 0)
				break;
			if ((cp = index(cp, ' ')) == NULL)
				break;
			cp++;
			if (first) {
				strcpy(namebuf, cp);
				first = 0;
			} else
				strcpy(rindex(namebuf, '!')+1, cp);
			strcat(namebuf, "!");
			goto newname;
		}
		cp++;
	}
	return(savestr(namebuf));
}

/*
 * Count the occurances of c in str
 */
charcount(str, c)
	char *str;
{
	register char *cp;
	register int i;

	for (i = 0, cp = str; *cp; cp++)
		if (*cp == c)
			i++;
	return(i);
}

/*
 * Are any of the characters in the two strings the same?
 */

anyof(s1, s2)
	register char *s1, *s2;
{

	while (*s1)
		if (index(s2, *s1++))
			return 1;
	return 0;
}

/*
 * Convert c to upper case
 */

raise(c)
	register c;
{

	if (islower(c))
		return toupper(c);
	return c;
}

/*
 * Copy s1 to s2, return pointer to null in s2.
 */

char *
copy(s1, s2)
	register char *s1, *s2;
{

	while (*s2++ = *s1++)
		;
	return s2 - 1;
}

/*
 * See if the given header field is supposed to be ignored.
 */
isign(field, ignore)
	char *field;
	struct ignoretab ignore[2];
{
	char realfld[BUFSIZ];

	/*
	 * Lower-case the string, so that "Status" and "status"
	 * will hash to the same place.
	 */
	istrcpy(realfld, field);
	if (ignore[1].i_count > 0)
		return (!member(realfld, ignore + 1));
	else
		return (member(realfld, ignore));
}

member(realfield, table)
	register char *realfield;
	struct ignoretab *table;
{
	register struct ignore *igp;

	for (igp = table->i_head[hash(realfield)]; igp != 0; igp = igp->i_link)
		if (*igp->i_field == *realfield &&
		    equal(igp->i_field, realfield))
			return (1);
	return (0);
}
