/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ctype.h	5.4 (Berkeley) %G%
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

extern char	_ctype_[];

#define	isalnum(c)	((_ctype_ + 1)[c] & (_U|_L|_N))
#define	isalpha(c)	((_ctype_ + 1)[c] & (_U|_L))
#define	isascii(c)	((unsigned)(c) <= 0177)
#define	isblank(c)	((c) == '\t' || (c) == ' ')
#define	iscntrl(c)	((_ctype_ + 1)[c] & _C)
#define	isdigit(c)	((_ctype_ + 1)[c] & _N)
#define	isgraph(c)	((_ctype_ + 1)[c] & (_P|_U|_L|_N))
#define	islower(c)	((_ctype_ + 1)[c] & _L)
#define	isprint(c)	((_ctype_ + 1)[c] & (_P|_U|_L|_N|_B))
#define	ispunct(c)	((_ctype_ + 1)[c] & _P)
#define	isspace(c)	((_ctype_ + 1)[c] & _S)
#define	isupper(c)	((_ctype_ + 1)[c] & _U)
#define	isxdigit(c)	((_ctype_ + 1)[c] & (_N|_X))
#define	toascii(c)	((c) & 0177)
#define	tolower(c)	((c) - 'A' + 'a')
#define	toupper(c)	((c) - 'a' + 'A')

#endif /* !_CTYPE_H_ */
