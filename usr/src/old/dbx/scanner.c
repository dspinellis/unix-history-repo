/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)scanner.c	1.10 (Berkeley) %G%";

/*
 * Debugger scanner.
 */

#include <ctype.h>
#include "defs.h"
#include "scanner.h"
#include "main.h"
#include "keywords.h"
#include "tree.h"
#include "symbols.h"
#include "names.h"
#include "y.tab.h"

#ifndef public
typedef int Token;
#endif

typedef struct {
	int	s_type;
#define	ST_FILE		0
#define	ST_ALIAS	1
	char	*s_name;
	int	s_lineno;
	union {
		File	su_file;
		struct sum {
			char	*sum_data;
			char	*sum_cur;
		} su_macro;
	} su;
#define	s_file	su.su_file
#define	s_macro	su.su_macro
#define	s_data	s_macro.sum_data
#define	s_cur	s_macro.sum_cur
} STREAM;

#define	NSTREAMS	10
private	STREAM stack[NSTREAMS];
private	STREAM *sp = &stack[-1];

public String initfile = ".dbxinit";

private Token getident();
private Token getnum();
private Token getstring();
private Char charcon();

#define MAXLINESIZE 1024
private Char yytext[MAXLINESIZE];
private Boolean shellmode;
private	Boolean doaliases;

public scanner_init()
{
	register Integer i;

	if (sp < stack)
		(void) pushinput(ST_FILE, nil, stdin);
	shellmode = false;
	doaliases = true;
	errfilename = nil;
	errlineno = sp->s_lineno = 0;
	yytext[0] = '\0';
}

#define	MAXDEPTH	25
/*
 * Read a single token.
 * There are two "modes" of operation:  one as in a compiler,
 * and one for reading shell-like syntax.
 */
public Token yylex()
{
	register int c;
	register char *p;
	register Token t;
	static int depth = 0;

	depth++;
	if (depth > MAXDEPTH) {
	    depth = 0;
	    error("Alias loop (maximum %d deep).\n", MAXDEPTH);
	}
again:
	do
		c = getch();
	while (c == ' ' || c == '\t');
	if (isalpha(c) || c == '_' || c == '$') {
		t = getident(c);
		if (t == NAME && doaliases) {
			p = findalias(yylval.y_name);
			if (p != nil) {
				if (lexdebug)
					fprintf(stderr, "alias %s to \"%s\"\n",
					    ident(yylval.y_name), p);
				if (!pushinput(ST_ALIAS, "", p)) {
					unwindinput(ST_ALIAS);
					error("Alias stack overflow.");
				}
				t = yylex();
			}
		}
		goto done;
	}
	if (isdigit(c)) {
		t = shellmode ? getident(c) : getnum(c);
		goto done;
	}
	switch (c) {

	case '\n':
		t = '\n';
		if (sp->s_lineno != 0) {
			sp->s_lineno++;
			if (sp->s_type == ST_FILE)
				errlineno = sp->s_lineno;
		}
		break;

	case '"':
	case '\'':
		t = getstring(c);
		break;

	case '.':
		if (shellmode) {
			t = getident(c);
			break;
		}
		c = getch();
		ungetch(c);
		t = isdigit(c) ? getnum('.') : '.';
		break;

	case '<':
		c = getch();
		if (shellmode || c != '<') {
			ungetch(c);
			t = '<';
		} else
			t = LFORMER;
		break;

	case '>':
		c = getch();
		if (shellmode || c != '>') {
			ungetch(c);
			t = '>';
		} else
			t = RFORMER;
		break;

	case '#':
		c = getch();
		if (c != '^') {
			ungetch(c);
			t = '#';
		} else
			t = ABSTRACTION;
		break;

	case '-':
		if (shellmode) {
			t = getident(c);
			break;
		}
		c = getch();
		if (c != '>') {
			ungetch(c);
			t = '-';
		} else
			t = ARROW;
		break;

	case EOF:
		t = 0;
		break;

	default:
		t = shellmode && index("!&*()[];", c) == nil ?
		    getident(c) : c;
		break;
	}
done:
	if (lexdebug) {
		fprintf(stderr, "token ");
		print_token(stderr, t);
		fprintf(stderr, "\n");
	}
	depth--;
	return (t);
}

/*
 * Scan an identifier and check to see if it's a keyword.
 */
private Token getident(c)
Char c;
{
	register Char *p, *q;
	Token t;

	q = yytext;
	if (shellmode) {
		do {
			*q++ = c;
			c = getch();
		} while (index(" \t\n!&<>*[]();", c) == nil);
	} else {
		do {
			*q++ = c;
			c = getch();
		} while (isalnum(c) || c == '_' || c == '$');
	}
	ungetch(c);
	*q = '\0';
	yylval.y_name = identname(yytext, false);
	if (shellmode)
		return (NAME);
	t = findkeyword(yylval.y_name);
	return (t == nil ? NAME : t);
}

/*
 * Scan a number.
 */
private Token getnum(c)
Char c;
{
	register Char *q;
	register Token t;
	Integer base = 10;

	q = yytext;
	if (c == '0') {
		c = getch();
		if (c == 'x') {
			base = 16;
		} else {
			base = 8;
			ungetch(c);
			c = '0';
		}
	}
	if (base == 16) {
		while (isdigit(c = getch()) ||
		    (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
			*q++ = c;
	} else {
		do {
			*q++ = c;
			c = getch();
		} while (isdigit(c));
	}
	if (c == '.') {
		do {
			*q++ = c;
			c = getch();
		} while (isdigit(c));
		if (c == 'e' || c == 'E') {
			c = getch();
			if (c == '+' || c == '-' || isdigit(c)) {
				*q++ = 'e';
				do {
					*q++ = c;
					c = getch();
				} while (isdigit(c));
			}
		}
		ungetch(c);
		*q = '\0';
		yylval.y_real = atof(yytext);
		return (REAL);
	}
	ungetch(c);
	*q = '\0';
	switch (base) {

	case 10:
		yylval.y_int = atol(yytext);
		break;

	case 8:
		yylval.y_int = octal(yytext);
		break;

	case 16:
		yylval.y_int = hex(yytext);
		break;

	default:
		badcaseval(base);
	}
	return (INT);
}

/*
 * Convert a string of octal digits to an integer.
 */
private int octal(s)
String s;
{
	register Char *p;
	register Integer n;

	n = 0;
	for (p = s; *p != '\0'; p++)
		n = (n << 3) + (*p - '0');
	return (n);
}

/*
 * Convert a string of hexadecimal digits to an integer.
 */
private int hex(s)
String s;
{
	register Char *p;
	register Integer n;

	n = 0;
	for (p = s; *p != '\0'; p++) {
		n <<= 4;
		if (*p >= 'a' && *p <= 'f')
			n += (*p - 'a' + 10);
		else if (*p >= 'A' && *p <= 'F')
			n += (*p - 'A' + 10);
		else
			n += (*p - '0');
	}
	return (n);
}

/*
 * Scan a string.
 */
private Token getstring(match)
Char match;
{
	register Char *q, c;

	q = yytext;
	for (;;) {
		c = getch();
		if (c == '\n' || c == EOF) {
			error("Unterminated string.");
			break;
		}
		if (c == match)
			break;
		*q++ = charcon(c);
	}
	*q = '\0';
	yylval.y_string = strdup(yytext);
	return (STRING);
}

/*
 * Process a character constant.
 * Watch out for backslashes.
 */
private Char charcon(c)
Char c;
{
	register char *cp;

	if (c == '\\') {
		c = getch();
		if (isdigit(c)) {
			int v;

			v = 0;
			do {
				v = (v << 3) + (c - '0');
				c = getch();
			} while (isdigit(c));
			ungetch(c);
			return (v);
		}
		for (cp = "f\ft\tb\bn\nr\rv\v"; *cp != c; cp += 2)
			;
		if (*cp != '\0')
			c = *cp;
	}
	return (c);
}

/*
 * Parser error handling.
 */
public yyerror(s)
String s;
{

	if (streq(s, "syntax error")) {
		beginerrmsg();
		fprintf(stderr, "Syntax error");
		if (yytext[0] != '\0')
			fprintf(stderr, " on \"%s\".", yytext);
		enderrmsg();
		return;
	}
	error(s);
}

/*
 * Eat the current line.
 */
private Char lastc = '\0';

public gobble()
{
	register char c;

	if (lastc != '\n' && lastc != EOF)
		while ((c = getch()) != EOF && c != '\n')
			;
}

/*
 * Input file management routines.
 */
public setinput(filename)
Filename filename;
{
	File f;

	f = fopen(filename, "r");
	if (f == nil)
		error("%s: Can't open.", filename);
	if (!pushinput(ST_FILE, filename, f)) {
		unwindinput(ST_FILE);
		error("Source file nesting too deep.");
	}
}

/*
 * Send the current line to the shell.
 */
public shellline()
{
	register Char *p, c;

	for (p = yytext; (c = getch()) != EOF && c != '\n'; *p++ = c)
		;
	*p = '\0';
	shell(yytext);
	erecover();
}

/*
 * Read the rest of the current line in "shell mode".
 */
public beginshellmode()
{

	shellmode = true;
}

public endshellmode()
{

	shellmode = false;
}

public stopaliasing()
{

	doaliases = false;
}

public startaliasing()
{

	doaliases = true;
}

/*
 * Print out a token for debugging.
 */
public print_token(f, t)
File f;
Token t;
{

	switch (t) {

	case '\n':
		fprintf(f, "char '\\n'");
		return;

	case EOF:
		fprintf(f, "EOF");
		return;

	case NAME:
	case STRING:
		fprintf(f, "%s, \"%s\"", keywdstring(t), ident(yylval.y_name));
		return;
	}
	if (t < 256)
		fprintf(f, "char '%c'", t);
	else
		fprintf(f, "%s", keywdstring(t));
}

public int getch()
{
	int c;

again:
	switch (sp->s_type) {

	case ST_FILE:
		c = getc(sp->s_file);
		if (c == EOF && isterm(sp->s_file)) {
			clearerr(sp->s_file);
			putchar('\n');
			c = '\n';
		}
		break;

	case ST_ALIAS:
		c = *sp->s_cur++;
		if (c == '\0') {
			c = EOF;
			--sp->s_cur;
		}
		break;

	default:
		panic("Invalid input stream (type %d) to getch.",
		    sp->s_type);
	}
	if (c == EOF && popinput())
		goto again;
	return (lastc = c);
}

private int ungetch(c)
Char c;
{
	Char uc;

	if (c != EOF) switch (sp->s_type) {

	case ST_FILE:
		uc = ungetc(c, sp->s_file);
		break;

	case ST_ALIAS:
		if (sp->s_cur == sp->s_data)
			panic("Illegal ungetch on alias.");
		*--sp->s_cur = c;
		uc = c;
		break;

	default:
		panic("Invalid input stream (type %d) to ungetch.",
		    sp->s_type);
	}
	lastc = '\0';
	return (uc);
}

/*
 * Push the current input stream and
 * make the supplied stream the current.
 */
/*VARARGS3*/
public pushinput(type, name, info)
int type;
Filename name;
{

	if (sp >= &stack[NSTREAMS])
		return (0);
	++sp;
	sp->s_type = type;
	switch (type) {

	case ST_FILE:
		sp->s_file = (File)info;
		errfilename = sp->s_name = name;
		errlineno = sp->s_lineno = 1;
		break;

	case ST_ALIAS:
		sp->s_cur = sp->s_data = (char *)info;
		break;

	default:
		panic("Invalid input stream (type %d) to pushinput.", type);
	}
	return (1);
}

public popinput()
{

	if (sp <= &stack[0])		/* never pop stdin or equivalent */
		return (0);
	if (sp->s_type == ST_FILE && sp->s_file != stdin)
		fclose(sp->s_file);
	--sp;
	if (sp->s_type == ST_FILE)
		errfilename = sp->s_name;
	errlineno = sp->s_lineno;
	return (1);
}

/*
 * Unwind the input stack of all input types specified.
 * This is called to recover from an infinite
 * loop in alias processing or source file including.
 */
public unwindinput(type)
Integer type;
{

	while (sp->s_type == type && popinput())
		;
}

/*
 * Return whether we are currently reading from standard input.
 */
public Boolean isstdin()
{

	return ((Boolean)(sp->s_type == ST_FILE && sp->s_file == stdin));
}

public Boolean istty()
{

	return ((Boolean)isterm(sp->s_file));
}
