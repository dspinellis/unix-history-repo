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
static char sccsid[] = "@(#)mbrune.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <limits.h>
#include <rune.h>
#include <stddef.h>
#include <string.h>

char *
mbrune(string, c)
	const char *string;
	rune_t c;
{
	char const *result;
	rune_t r;

	while ((r = sgetrune(string, MB_LEN_MAX, &result))) {
		if (r == c)
			return ((char *)string);
		string = result == string ? string + 1 : result;
	}

	return (c == *string ? (char *)string : NULL);
}

char *
mbrrune(string, c)
	const char *string;
	rune_t c;
{
	const char *last = 0;
	char const *result;
	rune_t  r;

	while ((r = sgetrune(string, MB_LEN_MAX, &result))) {
		if (r == c)
			last = string;
		string = result == string ? string + 1 : result;
	}
	return (c == *string ? (char *)string : (char *)last);
}

char *
mbmb(string, pattern)
	const char *string;
	char *pattern;
{
	rune_t first, r;
	size_t plen, slen;
	char const *result;

	plen = strlen(pattern);
	slen = strlen(string);
	if (plen > slen)
		return (0);

	first = sgetrune(pattern, plen, &result);
	if (result == string)
		return (0);

	while (slen >= plen && (r = sgetrune(string, slen, &result))) {
		if (r == first) {
			if (strncmp(string, pattern, slen) == 0)
				return ((char *) string);
		}
		if (result == string) {
			--slen;
			++string;
		} else {
			slen -= result - string;
			string = result;
		}
	}
	return (0);
}
