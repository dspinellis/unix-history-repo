/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tr.c	4.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>

#define	NCHARS	256				/* size of u_char */
#define	OOBCH	257				/* out of band value */

typedef struct {
	char *str;
	int lastch, endrange;
	enum { NORM, INRANGE, EOS } state;
} STR;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	STR s1, s2;
	register int ch, indx, lastch;
	int cflag, dflag, sflag;
	u_char *tp, tab[NCHARS], squeeze[NCHARS];

	cflag = dflag = sflag = 0;
	while ((ch = getopt(argc, argv, "cds")) != EOF)
		switch((char)ch) {
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			fprintf(stderr,
			    "usage: tr [-cds] [string1 [string2]]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	/*
	 * the original tr was amazingly tolerant of the command line.
	 * Neither -c or -s have any effect unless there are two strings.
	 * Extra arguments are silently ignored.  Bag this noise, they
	 * should all be errors.
	 */
	if (argc < 2 && !dflag) {
		while ((ch = getchar()) != EOF)
			putchar(ch);
		exit(0);
	}

	bzero(tab, NCHARS);
	if (sflag) {
		s1.str = argv[1];
		s1.state = NORM;
		s1.lastch = OOBCH;
		while (next(&s1))
			squeeze[s1.lastch] = 1;
	}
	if (dflag) {
		s1.str = argv[0];
		s1.state = NORM;
		s1.lastch = OOBCH;
		while (next(&s1))
			tab[s1.lastch] = 1;
		if (cflag)
			for (tp = tab, indx = 0; indx < NCHARS; ++tp, ++indx)
				*tp = !*tp;
		if (sflag)
			for (lastch = OOBCH; (ch = getchar()) != EOF;) {
				if (tab[ch] || (squeeze[ch] && lastch == ch))
					continue;
				lastch = ch;
				putchar(ch);
			}
		else
			while ((ch = getchar()) != EOF)
				if (!tab[ch])
					putchar(ch);
	} else {
		s1.str = argv[0];
		s2.str = argv[1];
		s1.state = s2.state = NORM;
		s1.lastch = s2.lastch = OOBCH;
		if (cflag) {
			/*
			 * if cflag is set, tr just pretends it only got one
			 * character in string2.  As reasonable as anything
			 * else.  Should really be an error.
			 */
			while (next(&s2));
			lastch = s2.lastch;
			for (tp = tab, indx = 0; indx < NCHARS; ++tp, ++indx)
				*tp = lastch;
			while (next(&s1))
				tab[s1.lastch] = s1.lastch;
		} else {
			for (tp = tab, indx = 0; indx < NCHARS; ++tp, ++indx)
				*tp = indx;
			while (next(&s1)) {
				(void)next(&s2);
				tab[s1.lastch] = s2.lastch;
			}
		}
		if (sflag)
			for (lastch = OOBCH; (ch = getchar()) != EOF;) {
				ch = tab[ch];
				if (squeeze[ch] && lastch == ch)
					continue;
				lastch = ch;
				putchar(ch);
			}
		else
			while ((ch = getchar()) != EOF)
				putchar((int)tab[ch]);
	}
	exit(0);
}

next(s)
	register STR *s;
{
	register int ch;

	if (s->state == EOS)
		return(0);
	if (s->state == INRANGE) {
		if (++s->lastch == s->endrange)
			s->state = NORM;
		return(1);
	}
	if (!(ch = *s->str++)) {
		s->state = EOS;
		return(0);
	}
	if (ch == '\\') {			/* \### */
		s->lastch = tran(s);
		return(1);
	}
	if (ch == '-') {			/* ranges */
		if (s->lastch == OOBCH)		/* "-a" */
			goto fail2;
		if (!(ch = *s->str++))		/* "a-" */
			goto fail1;
		if (ch == '\\')			/* \### */
			ch = tran(s);
		if (s->lastch > ch) { 		/* "z-a" */
fail1:			--s->str;
fail2:			s->lastch = '-';
			return(1);
		}
		if (s->lastch == ch)		/* "a-a" */
			return(next(s));
		s->state = INRANGE;		/* "a-z" */
		s->endrange = ch;
		return(1);
	}
	s->lastch = ch;
	return(1);
}

/*
 * Translate \-escapes.  Up to 3 octal digits => char; no digits => literal.
 * Unadorned backslash "\" is like \000.
 */
tran(s)
	register STR *s;
{
	register int ch, cnt = 0, val = 0;

	for (;;) {
		ch = *s->str++;
		if (!isascii(ch) || !isdigit(ch) || ++cnt > 3)
			break;
		val = val * 8 + ch - '0';
	}
	if (cnt || ch == 0)
		s->str--;
	return (cnt ? val : ch);
}
