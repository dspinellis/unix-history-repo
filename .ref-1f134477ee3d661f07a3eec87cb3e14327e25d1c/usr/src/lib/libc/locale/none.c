/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)none.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stddef.h>
#include <stdio.h>
#include <rune.h>
#include <errno.h>
#include <stdlib.h>

rune_t	_none_sgetrune __P((const char *, size_t, char const **));
int	_none_sputrune __P((rune_t, char *, size_t, char **));

int
_none_init(rl)
	_RuneLocale *rl;
{
	rl->sgetrune = _none_sgetrune;
	rl->sputrune = _none_sputrune;
	_CurrentRuneLocale = rl;
	__mb_cur_max = 1;
	return(0);
}

rune_t
_none_sgetrune(string, n, result)
	const char *string;
	size_t n;
	char const **result;
{
	int c;

	if (n < 1) {
		if (result)
			*result = string;
		return(_INVALID_RUNE);
	}
	if (result)
		*result = string + 1;
	return(*string & 0xff);
}

int
_none_sputrune(c, string, n, result)
	rune_t c;
	char *string, **result;
	size_t n;
{
	if (n >= 1) {
		if (string)
			*string = c;
		if (result)
			*result = string + 1;
	} else if (result)
		*result = (char *)0;
	return(1);
}
