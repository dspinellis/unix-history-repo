/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)isctype.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#define _ANSI_LIBRARY
#include <ctype.h>

#undef isalnum
isalnum(c)
	int c;
{
	return ((_ctype_ + 1)[c] & (_U|_L|_N));
}

#undef isalpha
isalpha(c)
	int c;
{
	return ((_ctype_ + 1)[c] & (_U|_L));
}

#undef isascii
isascii(c)
	int c;
{
	return (c <= 0177);
}

#undef isblank
isblank(c)
	int c;
{
	return (c == '\t' || c == ' ');
}

#undef iscntrl
iscntrl(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _C);
}

#undef isdigit
isdigit(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _N);
}

#undef isgraph
isgraph(c)
	int c;
{
	return ((_ctype_ + 1)[c] & (_P|_U|_L|_N));
}

#undef islower
islower(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _L);
}

#undef isprint
isprint(c)
	int c;
{
	return ((_ctype_ + 1)[c] & (_P|_U|_L|_N|_B));
}

#undef ispunct
ispunct(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _P);
}

#undef isspace
isspace(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _S);
}

#undef isupper
isupper(c)
	int c;
{
	return ((_ctype_ + 1)[c] & _U);
}

#undef isxdigit
isxdigit(c)
	int c;
{
	return ((_ctype_ + 1)[c] & (_N|_X));
}

#undef toascii
toascii(c)
	int c;
{
	return (c & 0177);
}

#undef tolower
tolower(c)
	int c;
{
	return (isupper(c) ? c - 'A' + 'a' : c);
}

#undef toupper
toupper(c)
	int c;
{
	return (islower(c) ? c - 'a' + 'A' : c);
}
