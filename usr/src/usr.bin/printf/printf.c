/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef BUILTIN
#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */
#endif

#ifndef lint
static char sccsid[] = "@(#)printf.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PF(f, func) { \
	if (fieldwidth) \
		if (precision) \
			(void)printf(f, fieldwidth, precision, func); \
		else \
			(void)printf(f, fieldwidth, func); \
	else if (precision) \
		(void)printf(f, precision, func); \
	else \
		(void)printf(f, func); \
}

static int	 asciicode __P((void));
static void	 err __P((const char *fmt, ...));
static void	 escape __P((char *));
static int	 getchr __P((void));
static double	 getdouble __P((void));
static int	 getint __P((void));
static long	 getlong __P((void));
static char	*getstr __P((void));
static char	*mklong __P((char *, int));

static char **gargv;

int
#ifdef BUILTIN
progprintf(argc, argv)
#else
main(argc, argv)
#endif
	int argc;
	char **argv;
{
	static char *skip1, *skip2;
	register char *format, *fmt, *start;
	register int end, fieldwidth, precision;
	char convch, nextch;

	if (argc < 2) {
		(void)fprintf(stderr, "usage: printf format [arg ...]\n");
		return (1);
	}

	/*
	 * Basic algorithm is to scan the format string for conversion
	 * specifications -- once one is found, find out if the field
	 * width or precision is a '*'; if it is, gather up value.  Note,
	 * format strings are reused as necessary to use up the provided
	 * arguments, arguments of zero/null string are provided to use
	 * up the format string.
	 */
	skip1 = "#-+ 0";
	skip2 = "*0123456789";

	escape(fmt = format = *++argv);		/* backslash interpretation */
	gargv = ++argv;
	for (;;) {
		end = 0;
		/* find next format specification */
next:		for (start = fmt;; ++fmt) {
			if (!*fmt) {
				/* avoid infinite loop */
				if (end == 1) {
					err("missing format character");
					return (1);
				}
				end = 1;
				if (fmt > start)
					(void)printf("%s", start);
				if (!*gargv)
					return (0);
				fmt = format;
				goto next;
			}
			/* %% prints a % */
			if (*fmt == '%') {
				if (*++fmt != '%')
					break;
				*fmt++ = '\0';
				(void)printf("%s", start);
				goto next;
			}
		}

		/* skip to field width */
		for (; index(skip1, *fmt); ++fmt);
		fieldwidth = *fmt == '*' ? getint() : 0;

		/* skip to possible '.', get following precision */
		for (; index(skip2, *fmt); ++fmt);
		if (*fmt == '.')
			++fmt;
		precision = *fmt == '*' ? getint() : 0;

		/* skip to conversion char */
		for (; index(skip2, *fmt); ++fmt);
		if (!*fmt) {
			err("missing format character");
			return (1);
		}

		convch = *fmt;
		nextch = *++fmt;
		*fmt = '\0';
		switch(convch) {
		case 'c': {
			char p;

			p = getchr();
			PF(start, p);
			break;
		}
		case 's': {
			char *p;

			p = getstr();
			PF(start, p);
			break;
		}
		case 'd': case 'i': case 'o': case 'u': case 'x': case 'X': {
			long p;
			char *f;
			
			if ((f = mklong(start, convch)) == NULL)
				return (1);
			p = getlong();
			PF(f, p);
			break;
		}
		case 'e': case 'E': case 'f': case 'g': case 'G': {
			double p;

			p = getdouble();
			PF(start, p);
			break;
		}
		default:
			err("illegal format character.\n");
			return (1);
		}
		*fmt = nextch;
	}
	/* NOTREACHED */
}

static char *
mklong(str, ch)
	char *str;
	int ch;
{
	register char *copy;
	int len;

	len = strlen(str) + 2;
	if (copy = malloc((u_int)len)) {	/* never freed; XXX */
		bcopy(str, copy, len - 3);
		copy[len - 3] = 'l';
		copy[len - 2] = ch;
		copy[len - 1] = '\0';
	} else
		err("%s", strerror(errno));
	return(copy);
}

static void
escape(fmt)
	register char *fmt;
{
	register char *store;
	register int value, c;

	for (store = fmt; c = *fmt; ++fmt, ++store) {
		if (c != '\\') {
			*store = c;
			continue;
		}
		switch (*++fmt) {
		case '\0':		/* EOS, user error */
			*store = '\\';
			*++store = '\0';
			return;
		case '\\':		/* backslash */
		case '\'':		/* single quote */
			*store = *fmt;
			break;
		case 'a':		/* bell/alert */
			*store = '\7';
			break;
		case 'b':		/* backspace */
			*store = '\b';
			break;
		case 'f':		/* form-feed */
			*store = '\f';
			break;
		case 'n':		/* newline */
			*store = '\n';
			break;
		case 'r':		/* carriage-return */
			*store = '\r';
			break;
		case 't':		/* horizontal tab */
			*store = '\t';
			break;
		case 'v':		/* vertical tab */
			*store = '\13';
			break;
					/* octal constant */
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
			for (c = 3, value = 0;
			    c-- && *fmt >= '0' && *fmt <= '7'; ++fmt) {
				value <<= 3;
				value += *fmt - '0';
			}
			--fmt;
			*store = value;
			break;
		default:
			*store = *fmt;
			break;
		}
	}
	*store = '\0';
}

static int
getchr()
{
	if (!*gargv)
		return('\0');
	return((int)**gargv++);
}

static char *
getstr()
{
	if (!*gargv)
		return("");
	return(*gargv++);
}

static char *number = "+-.0123456789";
static int
getint()
{
	if (!*gargv)
		return(0);
	if (index(number, **gargv))
		return(atoi(*gargv++));
	return(asciicode());
}

static long
getlong()
{
	if (!*gargv)
		return((long)0);
	if (index(number, **gargv))
		return(strtol(*gargv++, (char **)NULL, 0));
	return((long)asciicode());
}

static double
getdouble()
{
	if (!*gargv)
		return((double)0);
	if (index(number, **gargv))
		return(atof(*gargv++));
	return((double)asciicode());
}

static int
asciicode()
{
	register int ch;

	ch = **gargv;
	if (ch == '\'' || ch == '"')
		ch = (*gargv)[1];
	++gargv;
	return(ch);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "printf: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
