/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Roger L. Snyder.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)lsearch.c	5.3 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <unistd.h>

char *linear_base();

char *
lsearch(key, base, nelp, width, compar)
	char *key, *base;
	u_int *nelp, width;
	int (*compar)();
{
	return(linear_base(key, base, nelp, width, compar, 1));
}

char *
lfind(key, base, nelp, width, compar)
	char *key, *base;
	u_int *nelp, width;
	int (*compar)();
{
	return(linear_base(key, base, nelp, width, compar, 0));
}

static char *
linear_base(key, base, nelp, width, compar, add_flag)
	char *key, *base;
	u_int *nelp, width;
	int (*compar)(), add_flag;
{
	register char *element, *end;

	end = base + *nelp * width;
	for (element = base; element < end; element += width)
		if (!compar(element, key))		/* key found */
			return(element);

	if (!add_flag)					/* key not found */
		return(NULL);

	/*
	 * The UNIX System User's Manual, 1986 edition claims that
	 * a NULL pointer is returned by lsearch with errno set
	 * appropriately, if there is not enough room in the table
	 * to add a new item.  This can't be done as none of these
	 * routines have any method of determining the size of the
	 * table.  This comment was isn't in the 1986-87 System V
	 * manual.
	 */
	++*nelp;
	bcopy(key, end, (int)width);
	return(end);
}
