/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit at York University.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdef.h"
#include "stdd.h"
#include "extern.h"
#include "pathnames.h"

/*
 * find the index of second str in the first str.
 */
int
indx(s1, s2)
char *s1;
char *s2;
{
	register char *t;
	register char *p;
	register char *m;

	for (p = s1; *p; p++) {
		for (t = p, m = s2; *m && *m == *t; m++, t++);
		if (!*m)
			return (p - s1);
	}
	return (-1);
}
/*
 *  putback - push character back onto input
 */
void
putback(c)
char c;
{
	if (bp < endpbb)
		*bp++ = c;
	else
		oops("too many characters pushed back");
}

/*
 *  pbstr - push string back onto input
 *          putback is replicated to improve
 *          performance.
 */
void
pbstr(s)
register char *s;
{
	register char *es;
	register char *zp;

	es = s;
	zp = bp;

	while (*es)
		es++;
	es--;
	while (es >= s)
		if (zp < endpbb)
			*zp++ = *es--;
	if ((bp = zp) == endpbb)
		oops("too many characters pushed back");
}

/*
 *  pbnum - convert number to string, push back on input.
 */
void
pbnum(n)
int n;
{
	register int num;

	num = (n < 0) ? -n : n;
	do {
		putback(num % 10 + '0');
	}
	while ((num /= 10) > 0);

	if (n < 0)
		putback('-');
}

/*
 *  chrsave - put single char on string space
 */
void
chrsave(c)
char c;
{
	if (ep < endest)
		*ep++ = c;
	else
		oops("string space overflow");
}

/*
 * read in a diversion file, and dispose it.
 */
void
getdiv(n)
int n;
{
	register int c;
	register FILE *dfil;

	if (active == outfile[n])
		oops("%s: diversion still active.", "undivert");
	(void) fclose(outfile[n]);
	outfile[n] = NULL;
	m4temp[UNIQUE] = n + '0';
	if ((dfil = fopen(m4temp, "r")) == NULL)
		oops("%s: cannot undivert.", m4temp);
	else
		while ((c = getc(dfil)) != EOF)
			putc(c, active);
	(void) fclose(dfil);

#ifdef vms
	if (remove(m4temp))
#else
	if (unlink(m4temp) == -1)
#endif
		oops("%s: cannot unlink.", m4temp);
}

void
onintr(signo)
	int signo;
{
	oops("interrupted.");
}

/*
 * killdiv - get rid of the diversion files
 */
void
killdiv()
{
	register int n;

	for (n = 0; n < MAXOUT; n++)
		if (outfile[n] != NULL) {
			(void) fclose(outfile[n]);
			m4temp[UNIQUE] = n + '0';
#ifdef vms
			(void) remove(m4temp);
#else
			(void) unlink(m4temp);
#endif
		}
}

char *
xalloc(n)
unsigned long n;
{
	register char *p = malloc(n);

	if (p == NULL)
		oops("malloc: %s", strerror(errno));
	return p;
}

char *
xstrdup(s)
const char *s;
{
	register char *p = strdup(s);
	if (p == NULL)
		oops("strdup: %s", strerror(errno));
	return p;
}

char *
basename(s)
register char *s;
{
	register char *p;
	extern char *strrchr();

	if ((p = strrchr(s, '/')) == NULL)
		return s;

	return ++p;
}

void
usage()
{
	fprintf(stderr, "usage: m4 [-Dname[=val]] [-Uname]\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
oops(const char *fmt, ...)
#else
oops(fmt, va_alist)
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
	(void)fprintf(stderr, "%s: ", progname);
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
