/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)str.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/cdefs.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

static int	backslash __P((STR *));
static int	bracket __P((STR *));
static int	c_class __P((const void *, const void *));
static void	genclass __P((STR *));
static void	genequiv __P((STR *));
static int	genrange __P((STR *));
static void	genseq __P((STR *));

int
next(s)
	register STR *s;
{
	register int ch;

	switch (s->state) {
	case EOS:
		return (0);
	case INFINITE:
		return (1);
	case NORMAL:
		switch (ch = *s->str) {
		case '\0':
			s->state = EOS;
			return (0);
		case '\\':
			s->lastch = backslash(s);
			break;
		case '[':
			if (bracket(s))
				return (next(s));
			/* FALLTHROUGH */
		default:
			++s->str;
			s->lastch = ch;
			break;
		}

		/* We can start a range at any time. */
		if (s->str[0] == '-' && genrange(s))
			return (next(s));
		return (1);
	case RANGE:
		if (s->cnt-- == 0) {
			s->state = NORMAL;
			return (next(s));
		}
		++s->lastch;
		return (1);
	case SEQUENCE:
		if (s->cnt-- == 0) {
			s->state = NORMAL;
			return (next(s));
		}
		return (1);
	case SET:
	case ULSET:
		if ((s->lastch = s->set[s->cnt++]) == OOBCH) {
			s->state = NORMAL;
			return (next(s));
		}
		return (1);
	}
	/* NOTREACHED */
}

static int
bracket(s)
	register STR *s;
{
	register char *p;

	switch (*++s->str) {
	case ':':				/* "[:class:]" */
		if ((p = strpbrk(s->str + 1, ":]")) == NULL)
			return (0);
		if (p[0] != ':' || p[1] != ']')
			return (0);
		*p = '\0';
		++s->str;
		genclass(s);
		s->str = p + 2;
		return (1);
	case '=':				/* "[=equiv=]" */
		if ((p = strpbrk(s->str + 1, "=]")) == NULL)
			return (0);
		if (p[0] != '=' || p[1] != ']')
			return (0);
		genequiv(s);
		return (1);
	default:				/* "[\###*]" or "[#*]" */
		if ((p = strpbrk(s->str + 1, "*]")) == NULL)
			return (0);
		if (p[0] != '*' || index(p, ']') == NULL)
			return (0);
		genseq(s);
		return (1);
	}
	/* NOTREACHED */
}

int isalnum __P((int)),
    isalpha __P((int)),
    isblank __P((int)),
    isspace __P((int)),
    iscntrl __P((int)),
    isdigit __P((int)),
    isgraph __P((int)),
    islower __P((int)),
    isprint __P((int)),
    ispunct __P((int)),
    isupper __P((int)),
    isxdigit __P((int));

typedef struct {
	char *name;
	int (*func) __P((int));
	u_int type;
	int *set;
} CLASS;

static CLASS classes[] = {
	{ "alnum",  isalnum,  T_CLASS, },
	{ "alpha",  isalpha,  T_CLASS, },
	{ "blank",  isblank,  T_CLASS, },
	{ "cntrl",  iscntrl,  T_CLASS, },
	{ "digit",  isdigit,  T_CLASS, },
	{ "graph",  isgraph,  T_CLASS, },
	{ "lower",  islower,  T_UL, },
	{ "print",  isupper,  T_CLASS, },
	{ "punct",  ispunct,  T_CLASS, },
	{ "space",  isspace,  T_CLASS, },
	{ "upper",  isupper,  T_UL, },
	{ "xdigit", isxdigit, T_CLASS, },
};

static void
genclass(s)
	STR *s;
{
	register int cnt, (*func) __P((int));
	CLASS *cp, tmp;
	int *p;

	tmp.name = s->str;
	if ((cp = (CLASS *)bsearch(&tmp, classes, sizeof(classes) /
	    sizeof(CLASS), sizeof(CLASS), c_class)) == NULL)
		err("unknown class %s", s->str);
	if (!(cp->type | s->type))
		err("class %s illegally used");

	if ((cp->set = p = malloc((NCHARS + 1) * sizeof(int))) == NULL)
		err("%s", strerror(errno));
	bzero(p, NCHARS);
	for (cnt = 0, func = cp->func; cnt < NCHARS; ++cnt)
		if ((func)(cnt))
			*p++ = cnt;
	*p = OOBCH;

	s->cnt = 0;
	s->state = cp->type & T_UL ? ULSET : SET;
	s->set = cp->set;
}

static int
c_class(a, b)
	const void *a, *b;
{
	return (strcmp(((CLASS *)a)->name, ((CLASS *)b)->name));
}

/*
 * English doesn't have any equivalence classes, so for now
 * we just syntax check and grab the character.
 */
static void
genequiv(s)
	STR *s;
{
	static int val[2] = { 0, OOBCH };

	if (*++s->str == '\\') {
		val[0] = backslash(s);
		if (*s->str != '=')
			err("misplaced equivalence equals sign");
	} else {
		val[0] = s->str[0];
		if (s->str[1] != '=')
			err("misplaced equivalence equals sign");
	}
	s->str += 2;
	s->cnt = 0;
	s->state = SET;
	s->set = val;
}

static int
genrange(s)
	STR *s;
{
	int stopval;
	char *savestart;

	savestart = s->str;
	stopval = *++s->str == '\\' ? backslash(s) : *s->str;
	if (stopval < s->lastch) {
		s->str = savestart;
		return (0);
	}
	s->cnt = stopval - s->lastch + 1;
	s->state = RANGE;
	--s->lastch;
	return (1);
}

static void
genseq(s)
	STR *s;
{
	char *ep;

	if (!(s->type & T_SEQ))
		err("sequences only valid in string1");

	if (*s->str == '\\')
		s->lastch = backslash(s);
	else
		s->lastch = *s->str++;
	if (*s->str != '*')
		err("misplaced sequence asterisk");

	switch (*++s->str) {
	case '\\':
		s->cnt = backslash(s);
		break;
	case ']':
		s->cnt = 0;
		++s->str;
		break;
	default:
		if (isdigit(*s->str)) {
			s->cnt = strtol(s->str, &ep, 0);
			if (*ep == ']') {
				s->str = ep + 1;
				break;
			}
		}
		err("illegal sequence count");
		/* NOTREACHED */
	}

	s->state = s->cnt ? SEQUENCE : INFINITE;
}

/* Use the #defines here, DON'T use them above. */
#include <ctype.h>

/*
 * Translate \??? into a character.  Up to 3 octal digits, if no digits either
 * an escape code or a literal character.
 */
static int
backslash(s)
	register STR *s;
{
	register int ch, cnt, val;

	for (cnt = val = 0;;) {
		ch = *++s->str;
		if (!isascii(ch) || !isdigit(ch))
			break;
		val = val * 8 + ch - '0';
		if (++cnt == 3)
			break;
	}
	if (cnt)
		return (val);
	++s->str;
	switch (ch) {
		case 'a':			/* escape characters */
			return ('\7');
		case 'b':
			return ('\b');
		case 'f':
			return ('\f');
		case 'n':
			return ('\n');
		case 'r':
			return ('\r');
		case 't':
			return ('\t');
		case 'v':
			return ('\13');
		case '\0':			/*  \" -> \ */
			s->state = EOS;
			return ('\\');
		default:			/* \x" -> x */
			return (ch);
	}
}
