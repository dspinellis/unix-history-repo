/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tr.c	4.7 (Berkeley) 7/23/90";
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
