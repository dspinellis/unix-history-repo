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
static char sccsid[] = "@(#)yypanic.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"	/* must be included for yy.h */
#include "yy.h"

struct yytok oldpos;
/*
 * The routine yyPerror coordinates the panic when
 * the correction routines fail. Three types of panics
 * are possible - those in a declaration part, those
 * in a statement part, and those in an expression.
 *
 * Declaration part panics consider insertion of "begin",
 * expression part panics will stop on more symbols.
 * The panics are otherwise the same.
 *
 * ERROR MESSAGE SUPPRESSION STRATEGY: August 11, 1977
 *
 * If the parser has not made at least 2 moves since the last point of
 * error then we want to suppress the supplied error message.
 * Otherwise we print it.
 * We then skip input up to the next solid symbol.
 */
yyPerror(cp, kind)
	char *cp;
	register int kind;
{
	register int ishifts, brlev;

	copy((char *) (&oldpos), (char *) (&Y), sizeof oldpos);
	brlev = 0;
	if (yychar < 0)
		yychar = yylex();
	for (ishifts = yyshifts; ; yychar = yylex(), yyshifts++)
		switch (yychar) {
			case YILLCH:
				yerror("Illegal character");
				if (ishifts == yyshifts)
					yyOshifts = 0;
				continue;
			case YEOF:
				if (kind == PDECL) {
					/*
					 * we have paniced to end of file
					 * during declarations. Separately
					 * compiled segments can syntactically
					 * exit without any error message, so
					 * we force one here.
					 */
					yerror(cp);
					continuation();
					yyunexeof();
				}
				goto quiet;
			case ';':
				if (kind == PPROG)
					continue;
				if (kind == PDECL)
					yychar = yylex();
				goto resume;
			case YEND:
				if (kind == PPROG)
					continue;
			case YPROCEDURE:
			case YFUNCTION:
				goto resume;
			case YLABEL:
			case YTYPE:
			case YCONST:
			case YVAR:
				if (kind == PSTAT) {
					yerror("Declaration found when statement expected");
					goto quiet;
				}
			case YBEGIN:
				goto resume;
			case YFOR:
			case YREPEAT:
			case YWHILE:
			case YGOTO:
			case YIF:
				if (kind != PDECL)
					goto resume;
				yerror("Expected keyword begin after declarations, before statements");
				unyylex(&Y);
				yychar = YBEGIN;
				yylval = nullsem(YBEGIN);
				goto quiet;
			case YTHEN:
			case YELSE:
			case YDO:
				if (kind == PSTAT) {
					yychar = yylex();
					goto resume;
				}
				if (kind == PEXPR)
					goto resume;
				continue;
			case ')':
			case ']':
				if (kind != PEXPR)
					continue;
				if (brlev == 0)
					goto resume;
				if (brlev > 0)
					brlev--;
				continue;
			case '(':
			case '[':
				brlev++;
				continue;
			case ',':
				if (brlev != 0)
					continue;
			case YOF:
			case YTO:
			case YDOWNTO:
				if (kind == PEXPR)
					goto resume;
				continue;
#ifdef PI
			/*
			 * A rough approximation for now
			 * Should be much more lenient on suppressing
			 * warnings.
			 */
			case YID:
				syneflg = TRUE;
				continue;
#endif
		}
resume:
	if (yyOshifts >= 2) {
		if (yychar != -1)
			unyylex(&Y);
		copy((char *) (&Y), (char *) (&oldpos), sizeof Y);
		yerror(cp);
		yychar = yylex();
	}
quiet:
	if (yyshifts - ishifts > 2 && opt('r')) {
		setpfx('r');
		yerror("Parsing resumes");
	}
	/*
	 * If we paniced in the statement part,
	 * and didn't stop at a ';', then we insert
	 * a ';' to prevent the recovery from immediately
	 * inserting one and complaining about it.
	 */
	if (kind == PSTAT && yychar != ';') {
		unyylex(&Y);
		yyshifts--;
		yytshifts--;
		yychar = ';';
		yylval = nullsem(';');
	}
}
