/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sh.char.h	5.3 (Berkeley) 3/29/86
 */

/*
 * Table for spotting special characters quickly
 *
 * Makes for very obscure but efficient coding.
 */

extern unsigned short _cmap[];

#define _Q	0x01		/* '" */
#define _Q1	0x02		/* ` */
#define _SP	0x04		/* space and tab */
#define _NL	0x08		/* \n */
#define _META	0x10		/* lex meta characters, sp #'`";&<>()|\t\n */
#define _GLOB	0x20		/* glob characters, *?{[` */
#define _ESC	0x40		/* \ */
#define _DOL	0x80		/* $ */
#define _DIG   0x100		/* 0-9 */
#define _LET   0x200		/* a-z, A-Z, _ */

#define cmap(c, bits)	(_cmap[(unsigned char)(c)] & (bits))

#define isglob(c)	cmap(c, _GLOB)
#define isspace(c)	cmap(c, _SP)
#define isspnl(c)	cmap(c, _SP|_NL)
#define ismeta(c)	cmap(c, _META)
#define digit(c)	cmap(c, _DIG)
#define letter(c)	cmap(c, _LET)
#define alnum(c)	(digit(c) || letter(c))
