/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ansi.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <limits.h>
#include <stddef.h>
#include <rune.h>

int
mblen(s, n)
	const char *s;
	size_t n;
{
	char const *e;

	if (s == 0 || *s == 0)
		return (0);	/* No support for state dependent encodings. */

	if (sgetrune(s, (int)n, &e) == _INVALID_RUNE)
		return (s - e);
	return (e - s);
}

int
mbtowc(pwc, s, n)
	wchar_t *pwc;
	const char *s;
	size_t n;
{
	char const *e;
	rune_t r;

	if (s == 0 || *s == 0)
		return (0);	/* No support for state dependent encodings. */

	if ((r = sgetrune(s, (int)n, &e)) == _INVALID_RUNE)
		return (s - e);
	if (pwc)
		*pwc = r;
	return (e - s);
}

int
wctomb(s, wchar)
	char *s;
	wchar_t wchar;
{
	char *e;

	if (s == 0)
		return (0);	/* No support for state dependent encodings. */

	if (wchar == 0) {
		*s = 0;
		return (1);
	}

	sputrune(wchar, s, MB_CUR_MAX, &e);
	return (e ? e - s : -1);
}

size_t
mbstowcs(pwcs, s, n)
	wchar_t *pwcs;
	const char *s;
	size_t n;
{
	char const *e;
	int cnt = 0;

	if (!pwcs || !s)
		return (-1);

	while (n-- > 0) {
		*pwcs = sgetrune(s, MB_LEN_MAX, &e);
		if (*pwcs == _INVALID_RUNE)
			return (-1);
		if (*pwcs++ == 0)
			break;
		s = e;
		++cnt;
	}
	return (cnt);
}

size_t
wcstombs(s, pwcs, n)
	char *s;
	const wchar_t *pwcs;
	size_t n;
{
	char *e;
	int cnt = 0;

	if (!pwcs || !s)
		return (-1);

	while (n > 0) {
		if (*pwcs == 0) {
			*s = 0;
			break;
		}
		if (!sputrune(*pwcs++, s, (int)n, &e))
			return (-1);		/* encoding error */
		if (!e)			/* too long */
			return (cnt);
		cnt += e - s;
		s = e;
	}
	return (cnt);
}
