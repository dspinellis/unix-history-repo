/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strftime.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <tzfile.h>

static char *afmt[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
};
static char *Afmt[] = {
	"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
	"Saturday",
};
static char *bfmt[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
	"Oct", "Nov", "Dec",
};
static char *Bfmt[] = {
	"January", "February", "March", "April", "May", "June", "July",
	"August", "September", "October", "November", "December",
};

static size_t gsize;
static char *pt;

size_t
strftime(s, maxsize, format, t)
	char *s;
	char *format;
	size_t maxsize;
	struct tm *t;
{
	size_t _fmt();

	pt = s;
	if ((gsize = maxsize) < 1)
		return(0);
	if (_fmt(format, t)) {
		*pt = '\0';
		return(maxsize - gsize);
	}
	return(0);
}

static size_t
_fmt(format, t)
	register char *format;
	struct tm *t;
{
	for (; *format; ++format) {
		if (*format == '%')
			switch(*++format) {
			case '\0':
				--format;
				break;
			case 'A':
				if (t->tm_wday < 0 || t->tm_wday > 6)
					return(0);
				if (!_add(Afmt[t->tm_wday]))
					return(0);
				continue;
			case 'a':
				if (t->tm_wday < 0 || t->tm_wday > 6)
					return(0);
				if (!_add(afmt[t->tm_wday]))
					return(0);
				continue;
			case 'B':
				if (t->tm_mon < 0 || t->tm_mon > 11)
					return(0);
				if (!_add(Bfmt[t->tm_mon]))
					return(0);
				continue;
			case 'b':
			case 'h':
				if (t->tm_mon < 0 || t->tm_mon > 11)
					return(0);
				if (!_add(bfmt[t->tm_mon]))
					return(0);
				continue;
			case 'c':
				if (!_fmt("%x %X %Z %Y", t))
					return(0);
				continue;
			case 'D':
				if (!_fmt("%m/%d/%y", t))
					return(0);
				continue;
			case 'd':
				if (!_conv(t->tm_mday, 2))
					return(0);
				continue;
			case 'H':
				if (!_conv(t->tm_hour, 2))
					return(0);
				continue;
			case 'I':
				if (!_conv(t->tm_hour % 12 ?
				    t->tm_hour % 12 : 12, 2))
					return(0);
				continue;
			case 'j':
				if (!_conv(t->tm_yday + 1, 3))
					return(0);
				continue;
			case 'M':
				if (!_conv(t->tm_min, 2))
					return(0);
				continue;
			case 'm':
				if (!_conv(t->tm_mon + 1, 2))
					return(0);
				continue;
			case 'n':
				if (!_add("\n"))
					return(0);
				continue;
			case 'p':
				if (!_add(t->tm_hour >= 12 ? "PM" : "AM"))
					return(0);
				continue;
			case 'R':
				if (!_fmt("%H:%M", t))
					return(0);
				continue;
			case 'r':
				if (!_fmt("%I:%M:%S %p", t))
					return(0);
				continue;
			case 'S':
				if (!_conv(t->tm_sec, 2))
					return(0);
				continue;
			case 'T':
			case 'X':
				if (!_fmt("%H:%M:%S", t))
					return(0);
				continue;
			case 't':
				if (!_add("\t"))
					return(0);
				continue;
			case 'U':
				if (!_conv((t->tm_yday + 7 - t->tm_wday) / 7,
				    2))
					return(0);
				continue;
			case 'W':
				if (!_conv((t->tm_yday + 7 -
				    (t->tm_wday ? (t->tm_wday - 1) : 6))
				    / 7, 2))
					return(0);
				continue;
			case 'w':
				if (!_conv(t->tm_wday, 1))
					return(0);
				continue;
			case 'x':
				if (!_fmt("%a %b %d %Y", t))
					return(0);
				continue;
			case 'y':
				if (!_conv((t->tm_year + TM_YEAR_BASE)
				    % 100, 2))
					return(0);
				continue;
			case 'Y':
				if (!_conv(t->tm_year + TM_YEAR_BASE, 4))
					return(0);
				continue;
			case 'Z':
				if (!t->tm_zone || !_add(t->tm_zone))
					return(0);
				continue;
			case '%':
			/*
			 * X311J/88-090 (4.12.3.5): if conversion char is
			 * undefined, behavior is undefined.  Print out the
			 * character itself as printf(3) also does.
			 */
			default:
				break;
		}
		if (!gsize--)
			return(0);
		*pt++ = *format;
	}
	return(gsize);
}

static
_conv(n, digits)
	int n, digits;
{
	static char buf[10];
	register char *p;

	for (p = buf + sizeof(buf) - 2; n > 0 && p > buf; n /= 10, --digits)
		*p-- = n % 10 + '0';
	while (p > buf && digits-- > 0)
		*p-- = '0';
	return(_add(++p));
}

static
_add(str)
	register char *str;
{
	for (;; ++pt, --gsize) {
		if (!gsize)
			return(0);
		if (!(*pt = *str++))
			return(1);
	}
}
