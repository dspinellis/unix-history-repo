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
static char sccsid[] = "@(#)frune.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <limits.h>
#include <rune.h>
#include <stddef.h>
#include <stdio.h>

long
fgetrune(fp)
	FILE *fp;
{
	rune_t  r;
	int c, len;
	char buf[MB_LEN_MAX];
	char const *result;

	len = 0;
	do {
		if ((c = getc(fp)) == EOF) {
			if (len)
				break;
			return (EOF);
		}
		buf[len++] = c;

		if ((r = sgetrune(buf, len, &result)) != _INVALID_RUNE)
			return (r);
	} while (result == buf && len < MB_LEN_MAX);

	while (--len > 0)
		ungetc(buf[len], fp);
	return (_INVALID_RUNE);
}

int
fungetrune(r, fp)
	rune_t r;
	FILE* fp;
{
	int len;
	char buf[MB_LEN_MAX];

	len = sputrune(r, buf, MB_LEN_MAX, 0);
	while (len-- > 0)
		if (ungetc(buf[len], fp) == EOF)
			return (EOF);
	return (0);
}

int
fputrune(r, fp)
	rune_t r;
	FILE *fp;
{
	int i, len;
	char buf[MB_LEN_MAX];

	len = sputrune(r, buf, MB_LEN_MAX, 0);

	for (i = 0; i < len; ++i)
		if (putc(buf[i], fp) == EOF)
			return (EOF);

	return (0);
}
