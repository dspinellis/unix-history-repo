/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ctype.h	5.6 (Berkeley) %G%
 */

#ifndef _CTYPE_H_
#define _CTYPE_H_

#define	_U	0x01
#define	_L	0x02
#define	_N	0x04
#define	_S	0x08
#define	_P	0x10
#define	_C	0x20
#define	_X	0x40
#define	_B	0x80

extern char __ctype_[], __maplower[], __mapupper[];

#define	isalnum(c)	((__ctype + 1)[c] & (_U|_L|_N))
#define	isalpha(c)	((__ctype + 1)[c] & (_U|_L))
#define	iscntrl(c)	((__ctype + 1)[c] & _C)
#define	isdigit(c)	((__ctype + 1)[c] & _N)
#define	isgraph(c)	((__ctype + 1)[c] & (_P|_U|_L|_N))
#define	islower(c)	((__ctype + 1)[c] & _L)
#define	isprint(c)	((__ctype + 1)[c] & (_P|_U|_L|_N|_B))
#define	ispunct(c)	((__ctype + 1)[c] & _P)
#define	isspace(c)	((__ctype + 1)[c] & _S)
#define	isupper(c)	((__ctype + 1)[c] & _U)
#define	isxdigit(c)	((__ctype + 1)[c] & (_N|_X))
#define	tolower(c)	((__maplower + 1)[c])
#define	toupper(c)	((__mapupper + 1)[c])

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
#define	isascii(c)	((unsigned int)(c) <= 0177)
#define	isblank(c)	((c) == '\t' || (c) == ' ')
#define	toascii(c)	((c) & 0177)
#endif

#endif /* !_CTYPE_H_ */
