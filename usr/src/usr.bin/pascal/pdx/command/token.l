%{
/*-
 * Copyright (c) 1982, 1993
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
static char sccsid[] = "@(#)token.l	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Token definitions for pdx scanner.
 */

#include "defs.h"
#include "command.h"
#include "y.tab.h"
#include "main.h"
#include "symtab.h"
#include "sym.h"
#include "process.h"
#include "process/pxinfo.h"

char *initfile = ".pdxinit";

/* override Lex default input macro. */
LOCAL int pdxinput();

#undef  YY_INPUT
#define YY_INPUT(buf,result,max_size)	\
	{				\
	int c = pdxinput();		\
	if ( c == EOF )			\
		result = YY_NULL;	\
	else				\
		{			\
		buf[0] = c;		\
		result = 1;		\
		}			\
	}

%}

blank		[ \t]
white		{blank}+
alpha		[a-zA-Z]
digit		[0-9]
n		{digit}+
h		[0-9a-fA-F]+
e		(("e"|"E")("+"|"-")?{n})
alphanum	[a-zA-Z0-9]
ident		{alpha}{alphanum}*
filenm		[^ \t\n"<>!*"]+
string		'[^']+'('[^']*')*
newline		"\n"
char		.

%Start File sh

%%

{white}		;
^sh{white}.*$	{ BEGIN 0; yylval.y_string = &yytext[3]; return(SH); }
^sh		{ BEGIN 0; yylval.y_string = NIL; return(SH); }
^{ident}	{ return(findcmd(yytext)); }
<File>{filenm}	{ yylval.y_string = strdup(yytext); return(FILENAME); }
{filenm}/":"	{ yylval.y_string = strdup(yytext); return(FILENAME); }
{n}?\.{n}{e}?	{ yylval.y_real = atof(yytext); return(REAL); }
0{n}		{ yylval.y_long = octal(yytext); return(INT); }
0x{h}		{ yylval.y_long = hex(yytext); return(INT); }
{n}		{ yylval.y_long = atol(yytext); return(INT); }
at		{ return(AT); }
{ident}		{ return(ident(yytext)); }
{string}	{ yylval.y_string = yytext; return(STRING); }
"%dp"		{ yylval.y_long = (long) DP; return(INT); }
{newline}	{ BEGIN 0; nlflag = TRUE; return('\n'); }
{char}		{ return(yylval.y_int = yytext[0]); }

%%

LOCAL SYMTAB *dbtab, *specialtab;

/*
 * Look for the given string in the debugger keyword table.
 * If it's there, return the associated token, otherwise report an error.
 */

LOCAL int findcmd(s)
char *s;
{
	register SYM *p;

	if ((p = st_lookup(dbtab, s)) == NIL) {
		error("\"%s\" is not a command", s);
	}
	yylval.y_int = tokval(p);
	switch (toknum(p)) {
		case ALIAS:
		case DUMP:
		case EDIT:
		case CHFILE:
		case RUN:
		case SOURCE:
		case STATUS:
			BEGIN File;
			break;

		default:
			/* do nothing */;
	}
	return(toknum(p));
}

/*
 * Look for a symbol, first in the special table (if, in, etc.)
 * then in the symbol table.  If it's there, return the SYM pointer,
 * otherwise it's an error.
 */

LOCAL int ident(s)
char *s;
{
	register SYM *p;

	if ((p = st_lookup(specialtab, s)) != NIL) {
		yylval.y_sym = p;
		return(toknum(p));
	}
	p = st_lookup(symtab, s);
	if (p == NIL) {
		if (strcmp(s, "nil") == 0) {
			yylval.y_long = 0L;
			return(INT);
		} else {
			error("\"%s\" is not defined", s);
		}
	}
	yylval.y_sym = p;
	return(NAME);
}

/*
 * Convert a string to octal.  No check that digits are less than 8.
 */

LOCAL int octal(s)
char *s;
{
	register char *p;
	register int n;

	n = 0;
	for (p = s; *p != '\0'; p++) {
		n = 8*n + (*p - '0');
	}
	return(n);
}

/*
 * Convert a string to hex.
 */

LOCAL int hex(s)
char *s;
{
	register char *p;
	register int n;

	n = 0;
	for (p = s+2; *p != '\0'; p++) {
		n *= 16;
		if (*p >= 'a' && *p <= 'f') {
			n += (*p - 'a' + 10);
		} else if (*p >= 'A' && *p <= 'F') {
			n += (*p - 'A' + 10);
		} else {
			n += (*p - '0');
		}
	}
	return(n);
}

/*
 * Initialize the debugger keyword table (dbtab) and special symbol
 * table (specialtab).
 */

#define db_keyword(nm, n)	make_keyword(dbtab, nm, n)
#define sp_keyword(nm, n)	make_keyword(specialtab, nm, n)

lexinit()
{
	dbtab = st_creat(150);
	db_keyword("alias", ALIAS);
	db_keyword("assign", ASSIGN);
	db_keyword("call", CALL);
	db_keyword("cont", CONT);
	db_keyword("delete", DELETE);
	db_keyword("dump", DUMP);
	db_keyword("edit", EDIT);
	db_keyword("file", CHFILE);
	db_keyword("gripe", GRIPE);
	db_keyword("help", HELP);
	db_keyword("list", LIST);
	db_keyword("next", NEXT);
	db_keyword("pi", REMAKE);
	db_keyword("print", PRINT);
	db_keyword("quit", QUIT);
	db_keyword("run", RUN);
	db_keyword("sh", SH);
	db_keyword("source", SOURCE);
	db_keyword("status", STATUS);
	db_keyword("step", STEP);
	db_keyword("stop", STOP);
	db_keyword("stopi", STOPI);
	db_keyword("trace", TRACE);
	db_keyword("tracei", TRACEI);
	db_keyword("whatis", WHATIS);
	db_keyword("where", WHERE);
	db_keyword("which", WHICH);
	db_keyword("xd", XD);
	db_keyword("xi", XI);

	specialtab = st_creat(10);
	sp_keyword("div", DIV);
	sp_keyword("mod", MOD);
	sp_keyword("in", IN);
	sp_keyword("if", IF);
	sp_keyword("and", AND);
	sp_keyword("or", OR);
}

/*
 * Send an alias directive over to the symbol table manager.
 */

alias(new, old)
char *new, *old;
{
	if (old == NIL) {
		print_alias(dbtab, new);
	} else {
		enter_alias(dbtab, new, old);
	}
}

/*
 * Input file management routines, "yyin" is Lex's idea of
 * where the input comes from.
 */

#define MAXINPUT 10

LOCAL FILE *infp[MAXINPUT];
LOCAL FILE **curfp = &infp[0];

LOCAL BOOLEAN isnewfile;
LOCAL BOOLEAN firsttime;

/*
 * Initially, we set the input to the initfile if it exists.
 * If it does exist, we play a game or two to avoid generating
 * multiple prompts.
 */

initinput()
{
	FILE *fp;

	firsttime = FALSE;
	fp = fopen(initfile, "r");
	if (fp != NIL) {
		fclose(fp);
		setinput(initfile);
		if (!option('r')) {
			firsttime = TRUE;
		}
	}
	nlflag = TRUE;
}

/*
 * Set the input to the named file.  It is expected that the file exists
 * and is readable.
 */

setinput(filename)
char *filename;
{
	register FILE *fp;

	if ((fp = fopen(filename, "r")) == NIL) {
		error("can't open %s", filename);
	}
	if (curfp >= &infp[MAXINPUT]) {
		error("unreasonable input nesting on %s", filename);
	}
	*curfp++ = yyin;
	yyin = fp;
	isnewfile = TRUE;
}

BOOLEAN isstdin()
{
	return((BOOLEAN) (yyin == stdin));
}

LOCAL int pdxinput()
{
	register int c;

	if (isnewfile) {
		isnewfile = FALSE;
		return('\n');
	}
	while ((c = getc(yyin)) == EOF) {
		if (curfp == &infp[0]) {
			return(0);
		} else {
			fclose(yyin);
			yyin = *--curfp;
			if (yyin == stdin) {
				if (firsttime) {
					firsttime = FALSE;
				} else {
					prompt();
				}
			}
		}
	}
	return(c);
}

/*
 * prompt for a command
 */

prompt()
{
	nlflag = FALSE;
	if (yyin == stdin) {
		printf("> ");
		fflush(stdout);
	}
}
