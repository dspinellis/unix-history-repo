/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)asscanl.h	5.1 (Berkeley) 4/30/85
 */

/*
 *	This file contains definitions local to the files implementing
 *	the character scanner and the token buffer managers.
 *	It is not intended to be shared with any other parts of the
 *	assembler.
 *	The file ``asscan.h'' is shared with other parts of the assembler
 */
#include <stdio.h>
#include "as.h"
#include "asscan.h"

#define EOFCHAR	(-1)
/*
 *	The table of possible uses for each character to test set inclusion.
 */
#define	HEXFLAG		01		/* 'x' or 'X' */
#define	HEXLDIGIT	02		/* 'a' .. 'f' */
#define	HEXUDIGIT	04		/* 'A' .. 'F' */
#define	ALPHA		010		/* 'A' .. 'Z', 'a' .. 'z', '_'*/
#define	DIGIT		020		/* '0' .. '9' */
#define	FLOATEXP	040		/* 'd' 'e' 'D' 'E' 'g' 'h' 'G' 'H' */
#define	SIGN		0100		/* '+' .. '-'*/
#define	REGDIGIT	0200		/* '0' .. '5' */
#define	SZSPECBEGIN	0400		/* 'b', 'B', 'l', 'L', 'w', 'W' */
#define	POINT		01000		/* '.' */
#define	SPACE		02000		/* '\t' or ' ' */
#define	BSESCAPE	04000		/* bnrtf */
#define	STRESCAPE	010000		/* '"', '\\', '\n' */
#define	OCTDIGIT	020000		/* '0' .. '7' */
#define	FLOATFLAG	040000		/* 'd', 'D', 'f', 'F' */

#define	INCHARSET(val, kind) (charsets[val] & (kind) )
/*
 *	We use our own version of getchar/ungetc to get
 *	some speed improvement
 */
extern	char	*Ginbufptr;
extern	int	Ginbufcnt;
#define	REGTOMEMBUF	Ginbufptr = inbufptr, Ginbufcnt = inbufcnt
#define	MEMTOREGBUF	inbufptr = Ginbufptr, inbufcnt = Ginbufcnt
#undef getchar
#define	getchar() \
	(inbufcnt-- > 0 ? (*inbufptr++) : \
		(fillinbuffer(), \
		MEMTOREGBUF, \
		inbufptr[-1]))
#undef ungetc
#define ungetc(ch) \
	(++inbufcnt, *--inbufptr = ch)

/*
 *	Variables and definitions to manage the token buffering.
 *	We scan (lexically analyze) a large number of tokens, and
 *	then parse all of the tokens in the scan buffer.
 *	This reduces procedure call overhead when the parser
 *	demands a token, allows for an efficient reread during
 *	the second pass, and confuses the line number reporting
 *	for errors encountered in the scanner and in the parser.
 */
#define TOKDALLOP	8
struct	tokbufdesc *bufstart;	/*where the buffer list begins*/
struct	tokbufdesc *buftail;	/*last one on the list*/
struct	tokbufdesc *emptybuf;	/*the one being filled*/
/*
 *	If we are using VM, during the second pass we reclaim the used
 *	token buffers for saving the relocation information
 */
struct	tokbufdesc *tok_free;	/* free pool */
struct	tokbufdesc *tok_temp;	/* temporary for doing list manipulation */
/*
 *	Other token buffer managers
 */
int	bufno;			/*which buffer number: 0,1 for tmp file*/
struct 	tokbufdesc tokbuf[2];	/*our initial increment of buffers*/
ptrall	tokptr;			/*where the current token comes from*/
ptrall	tokub;			/*the last token in the current token buffer*/
/*
 *	as does not use fread and fwrite for the token buffering.
 *	The token buffers are integrals of BUFSIZ
 *	at all times, so we use direct read and write.
 *	fread and fwrite in stdio are HORRENDOUSLY inefficient,
 *	as they use putchar for each character, nested two deep in loops.
 */
#define writeTEST(pointer, size, nelements, ioptr) \
	write(ioptr->_file, pointer, nelements * size) != nelements * size

#define readTEST(pointer, size, nelements, ioptr) \
	read(ioptr->_file, pointer, nelements * size) != nelements * size

#define bskiplg(from, length) \
	*(lgtype *)from = length; \
	(bytetoktype *)from += sizeof(lgtype) + length

#define bskipfromto(from, to) \
	*(lgtype *)from = (bytetoktype *)to - (bytetoktype *)from - sizeof(lgtype); \
	(bytetoktype *)from += sizeof (lgtype) + (bytetoktype *)to - (bytetoktype *)from

#define eatskiplg(from) \
	(bytetoktype *)from += sizeof(lgtype) + *(lgtype *)from

#ifdef DEBUG
	ptrall	firsttoken;
#endif DEBUG

/*
 *	The following three variables are the slots for global
 *	communication with the parser.
 *	They are the semantic values associated with a particular token.
 *	The token itself is the return value from yylex()
 */
int	yylval;			/* normal semantic value */
Bignum	yybignum;		/* a big number */
struct	Opcode	yyopcode;	/* a structure opcode */

int	newfflag;
char	*newfname;
int	scanlineno;		/*the scanner's linenumber*/

/*
 *	Definitions for sets of characters
 */
readonly short charsets[];
readonly short type[];
