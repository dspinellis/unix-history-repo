/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mystring.h	8.1 (Berkeley) %G%
 */

#include <string.h>

#ifdef __STDC__
void scopyn(const char *, char *, int);
int prefix(const char *, const char *);
int number(const char *);
int is_number(const char *);
#else
void scopyn();
int prefix();
int number();
int is_number();
#endif

#define equal(s1, s2)	(strcmp(s1, s2) == 0)
#define scopy(s1, s2)	((void)strcpy(s2, s1))
