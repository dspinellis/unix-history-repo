/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mystring.h	8.2 (Berkeley) %G%
 */

#include <string.h>

void scopyn __P((const char *, char *, int));
int prefix __P((const char *, const char *));
int number __P((const char *));
int is_number __P((const char *));

#define equal(s1, s2)	(strcmp(s1, s2) == 0)
#define scopy(s1, s2)	((void)strcpy(s2, s1))
