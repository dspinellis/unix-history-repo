/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setlocale.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <locale.h>
#include <string.h>

static char C[] = "C";

/*
 * The setlocale function.
 *
 * Sorry, for now we only accept the C locale.
 */
char *
setlocale(category, locale)
	int category;
	const char *locale;
{
	if ((unsigned int)category >= _LC_LAST)
		return (NULL);
	if (locale == NULL)
		return (C);
	return(strcmp(locale, C) ? NULL : C);
}
