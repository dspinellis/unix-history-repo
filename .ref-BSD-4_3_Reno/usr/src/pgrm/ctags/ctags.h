/*
 * Copyright (c) 1987 The Regents of the University of California.
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
 *	@(#)ctags.h	5.3 (Berkeley) 6/1/90
 */

#include <stdio.h>
#include <ctype.h>

#define	bool	char

#define	YES		1
#define	NO		0
#define	EOS		'\0'

#define	ENDLINE		50		/* max length of pattern */
#define	MAXTOKEN	250		/* max size of single token */

#define	SETLINE		{++lineno;lineftell = ftell(inf);}
#define	GETC(op,exp)	((c = getc(inf)) op (int)exp)

#define	iswhite(arg)	(_wht[arg])	/* T if char is white */
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token */
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token */
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens */
#define	isgood(arg)	(_gd[arg])	/* T if char can be after ')' */

typedef struct nd_st {			/* sorting structure */
	struct nd_st	*left,
			*right;		/* left and right sons */
	char	*entry,			/* function or type name */
		*file,			/* file name */
		*pat;			/* search pattern */
	int	lno;			/* for -x option */
	bool	been_warned;		/* set if noticed dup */
} NODE;

extern FILE	*inf;			/* ioptr for current input file */
extern long	lineftell;		/* ftell after getc( inf ) == '\n' */
extern int	lineno,			/* line number of current line */
		xflag;			/* -x: cxref style output */
extern bool	_wht[0177],_etk[0177],_itk[0177],_btk[0177],_gd[0177];
extern char	lbuf[BUFSIZ];
