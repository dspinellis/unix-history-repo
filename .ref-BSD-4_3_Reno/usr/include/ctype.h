/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
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
 *
 *	@(#)ctype.h	5.2 (Berkeley) 6/1/90
 */

#define	_U	0x01
#define	_L	0x02
#define	_N	0x04
#define	_S	0x08
#define	_P	0x10
#define	_C	0x20
#define	_X	0x40
#define	_B	0x80

extern char	_ctype_[];

#define	isdigit(c)	((_ctype_ + 1)[c] & _N)
#define	islower(c)	((_ctype_ + 1)[c] & _L)
#define	isspace(c)	((_ctype_ + 1)[c] & _S)
#define	ispunct(c)	((_ctype_ + 1)[c] & _P)
#define	isupper(c)	((_ctype_ + 1)[c] & _U)
#define	isalpha(c)	((_ctype_ + 1)[c] & (_U|_L))
#define	isxdigit(c)	((_ctype_ + 1)[c] & (_N|_X))
#define	isalnum(c)	((_ctype_ + 1)[c] & (_U|_L|_N))
#define	isprint(c)	((_ctype_ + 1)[c] & (_P|_U|_L|_N|_B))
#define	isgraph(c)	((_ctype_ + 1)[c] & (_P|_U|_L|_N))
#define	iscntrl(c)	((_ctype_ + 1)[c] & _C)
#define	isascii(c)	((unsigned)(c) <= 0177)
#define	toupper(c)	((c) - 'a' + 'A')
#define	tolower(c)	((c) - 'A' + 'a')
#define	toascii(c)	((c) & 0177)
