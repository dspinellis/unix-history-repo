/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Borman at Krystal Technologies.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)isctype.c	8.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#define _ANSI_LIBRARY
#include <ctype.h>

#undef isalnum
int
isalnum(c)
	int c;
{
	return(__istype((c), (_A|_D)));
}

#undef isalpha
int
isalpha(c)
	int c;
{
	return (__istype((c), _A));
}

#undef isascii
int
isascii(c)
	int c;
{
	return((c & ~0x7F) == 0);
}

#undef isblank
int
isblank(c)
	int c;
{
	return (__istype((c), _B));
}

#undef iscntrl
int
iscntrl(c)
	int c;
{
	return (__istype((c), _C));
}

#undef isdigit
int
isdigit(c)
	int c;
{
	return (__isctype((c), _D));
}

#undef isgraph
int
isgraph(c)
	int c;
{
	return (__istype((c), _G));
}

#undef islower
int
islower(c)
	int c;
{
	return (__istype((c), _L));
}

#undef isprint
int
isprint(c)
	int c;
{
	return (__istype((c), _R));
}

#undef ispunct
int
ispunct(c)
	int c;
{
	return (__istype((c), _P));
}

#undef isspace
int
isspace(c)
	int c;
{
	return (__istype((c), _S));
}

#undef isupper
int
isupper(c)
	int c;
{
	return (__istype((c), _U));
}

#undef isxdigit
int
isxdigit(c)
	int c;
{
	return (__isctype((c), _X));
}

#undef toascii
int
toascii(c)
	int c;
{
	return (c & 0177);
}

#undef tolower
int
tolower(c)
	int c;
{
        return((c & _CRMASK) ? ___toupper(c) : _CurrentRuneLocale->mapupper[c]);
}

#undef toupper
int
toupper(c)
	int c;
{
        return((c & _CRMASK) ? ___tolower(c) : _CurrentRuneLocale->maplower[c]);
}
