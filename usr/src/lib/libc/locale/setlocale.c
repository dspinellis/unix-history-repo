/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setlocale.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <locale.h>

static char C[] = "C";

/*
 * The setlocale function.
 *
 * Sorry, for now we only accept the C locale.
 */
char *
setlocale(category, locale)
	int category;
	char *locale;
{

	if ((unsigned)category >= _LC_LAST)
		return (NULL);
	if (locale == NULL)
		return (C);
	if (strcmp(locale, C) == 0)
		return (C);
	return (NULL);
}
