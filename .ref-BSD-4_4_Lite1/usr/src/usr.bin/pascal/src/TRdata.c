/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)TRdata.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#ifdef	PI1
#ifdef	DEBUG
char	*trnames[] =
{
	0,
	"MINUS",
	"MOD",
	"DIV",
	"DIVD",
	"MULT",
	"ADD",
	"SUB",
	"EQ",
	"NE",
	"LT",
	"GT",
	"LE",
	"GE",
	"NOT",
	"AND",
	"OR",
	"ASGN",
	"PLUS",
	"IN",
	"LISTPP",
	"PDEC",
	"FDEC",
	"PVAL",
	"PVAR",
	"PFUNC",
	"PPROC",
	"NIL",
	"STRNG",
	"CSTRNG",
	"PLUSC",
	"MINUSC",
	"ID",
	"INT",
	"FINT",
	"CINT",
	"CFINT",
	"TYPTR",
	"TYPACK",
	"TYSCAL",
	"TYRANG",
	"TYARY",
	"TYFILE",
	"TYSET",
	"TYREC",
	"TYFIELD",
	"TYVARPT",
	"TYVARNT",
	"CSTAT",
	"BLOCK",
	"BSTL",
	"LABEL",
	"PCALL",
	"FCALL",
	"CASE",
	"WITH",
	"WHILE",
	"REPEAT",
	"FORU",
	"FORD",
	"GOTO",
	"IF",
	"ASRT",
	"CSET",
	"RANG",
	"VAR",
	"ARGL",
	"ARY",
	"FIELD",
	"PTR",
	"WEXP",
	"PROG",
	"BINT",
	"CBINT",
	"IFEL",
	"IFX",
	"TYID",
	"COPSTR",
	"BOTTLE",
	"RFIELD",
	"FLDLST",
	"LAST"
};
#endif
#endif

#ifndef PC
#ifndef OBJ
char	*trdesc[] =
{
	0,
	"dp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dpp",
	"dp",
	"dpp",
	"dpp",
	"npp",
	"dp",
	"dpp",
	"pp",
	"n\"pp",
	"n\"pp",
	"pp",
	"pp",
	"pp",
	"p",
	"d",
	"dp",
	"p",
	"p",
	"p",
	"p",
	"dp",
	"dp",
	"p",
	"p",
	"np",
	"np",
	"np",
	"npp",
	"npp",
	"np",
	"np",
	"np",
	"pp",
	"nppp",
	"npp",
	"npp",
	"np",
	"np",
	"n\"p",
	"n\"p",
	"n\"p",
	"npp",
	"npp",
	"npp",
	"npp",
	"nppp",
	"nppp",
	"n\"",
	"nppp",
	"np",
	"dp",
	"pp",
	"n\"p",
	"p",
	"p",
	"pp",
	"",
	"ppp",
	"n\"pp",
	"dp",
	"p",
	"nppp",
	"nppp",
	"np",
	"s",
	"nnnnn",
	"npp",
	"npp",
	"x"
};
#endif
#endif
char	*opnames[] =
{
	0,
	"unary -",
	"mod",
	"div",
	"/",
	"*",
	"+",
	"-",
	"=",
	"<>",
	"<",
	">",
	"<=",
	">=",
	"not",
	"and",
	"or",
	":=",
	"unary +",
	"in"
};
