/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)multibyte.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>

/*
 * Stub multibyte character functions.
 * These ignore the current fixed ("C") locale and
 * always indicate that no multibyte characters are supported.
 */

int
mblen(s, n)
	const char *s;
	size_t n;
{
	if (s && n && *s)
		return -1;
	return 0;
}

/*ARGSUSED*/
int
mbtowc(pwc, s, n)
	wchar_t *pwc;
	const char *s;
	size_t n;
{
	if (s && n && *s)
		return -1;
	return 0;
}

/*ARGSUSED*/
int
#ifdef __STDC__
wctomb(char *s, wchar_t wchar)
#else
wctomb(s, wchar)
	char *s;
	wchar_t wchar;
#endif
{
	if (s)
		return -1;
	return 0;
}

/*ARGSUSED*/
size_t
mbstowcs(pwcs, s, n)
	wchar_t *pwcs;
	const char *s;
	size_t n;
{
	if (s && n && *s)
		return -1;
	return 0;
}

/*ARGSUSED*/
size_t
wcstombs(s, pwcs, n)
	char *s;
	const wchar_t *pwcs;
	size_t n;
{
	if (pwcs && n && *pwcs)
		return -1;
	return 0;
}
