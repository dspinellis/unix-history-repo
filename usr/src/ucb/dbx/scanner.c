/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)scanner.c	5.1 (Berkeley) 5/31/85";
#endif not lint

static char rcsid[] = "$Header: scanner.c,v 1.5 84/12/26 10:42:05 linton Exp $";

/*
 * Debugger scanner.
 */

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

#define MAXLINESIZE 10240

#endif

public String initfile = ".dbxinit";

typedef enum { WHITE, ALPHA, NUM, OTHER } Charclass;

private Charclass class[256 + 1];
private Charclass *lexclass = class + 1;

#define isdigit(c) (lexclass[c] == NUM)
#define isalnum(c) (lexclass[c] == ALPHA or lexclass[c] == NUM)
#define ishexdigit(c) ( \
    isdigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F') \
)

public boolean chkalias;
public char scanner_linebuf[MAXLINESIZE];

private File in;
private char *curchar, *prevchar;

#define MAXINCLDEPTH 10

private struct {
    File savefile;
    Filename savefn;
    int savelineno;
} inclinfo[MAXINCLDEPTH];

private unsigned int curinclindex;

private Token getident();
private Token getnum();
private Token getstring();
private Boolean eofinput();
private char charcon();

private enterlexclass(class, s)
Charclass class;
String s;
{
    register char *p;

    for (p = s; *p != '\0'; p++) {
	lexclass[*p] = class;
    }
}

public scanner_init()
{
    register Integer i;

    for (i = 0; i < 257; i++) {
	class[i] = OTHER;
    }
    enterlexclass(WHITE, " \t");
    enterlexclass(ALPHA, "abcdefghijklmnopqrstuvwxyz");
    enterlexclass(ALPHA, "ABCDEFGHIJKLMNOPQRSTUVWXYZ_$");
    enterlexclass(NUM, "0123456789");
    in = stdin;
    errfilename = nil;
    errlineno = 0;
    curchar = scanner_linebuf;
    scanner_linebuf[0] = '\0';
    chkalias = true;
}

/*
 * Read a single token.
 *
 * The input is line buffered.  Tokens cannot cross line boundaries.
 *
 * There are two "modes" of operation:  one as in a compiler,
 * and one for reading shell-like syntax.  In the first mode
 * there is the additional choice of doing alias processing.
 */

private Boolean shellmode;

public Token yylex()
{
    register int c;
    register char *p;
    register Token t;
    String line;
    integer n;

    p = curchar;
    if (*p == '\0') {
	do {
	    if (isterm(in)) {
		printf("(%s) ", cmdname);
	    }
	    fflush(stdout);
	    line = fgets(scanner_linebuf, MAXLINESIZE, in);
	} while (line == nil and not eofinput());
	if (line == nil) {
	    c = EOF;
	} else {
	    p = scanner_linebuf;
	    while (lexclass[*p] == WHITE) {
		p++;
	    }
	    shellmode = false;
	}
	chkalias = true;
    } else {
	while (lexclass[*p] == WHITE) {
	    p++;
	}
    }
    curchar = p;
    prevchar = curchar;
    c = *p;
    if (lexclass[c] == ALPHA) {
	t = getident(chkalias);
    } else if (lexclass[c] == NUM) {
	if (shellmode) {
	    t = getident(chkalias);
	} else {
	    t = getnum();
	}
    } else {
	++curchar;
	switch (c) {
	    case '\n':
		t = '\n';
		if (errlineno != 0) {
		    errlineno++;
		}
		break;

	    case '"':
	    case '\'':
		t = getstring(c);
		break;

	    case '.':
		if (shellmode) {
		    --curchar;
		    t = getident(chkalias);
		} else if (isdigit(*curchar)) {
		    --curchar;
		    t = getnum();
		} else {
		    t = '.';
		}
		break;

	    case '-':
		if (shellmode) {
		    --curchar;
		    t = getident(chkalias);
		} else if (*curchar == '>') {
		    ++curchar;
		    t = ARROW;
		} else {
		    t = '-';
		}
		break;

	    case '#':
		if (not isterm(in)) {
		    *p = '\0';
		    curchar = p;
		    t = '\n';
		    ++errlineno;
		} else {
		    t = '#';
		}
		break;

	    case '\\':
		if (*(p+1) == '\n') {
		    n = MAXLINESIZE - (p - &scanner_linebuf[0]);
		    if (n > 1) {
			if (fgets(p, n, in) == nil) {
			    t = 0;
			} else {
			    curchar = p;
			    t = yylex();
			}
		    } else {
			t = '\\';
		    }
		} else {
		    t = '\\';
		}
		break;

	    case EOF:
		t = 0;
		break;

	    default:
		if (shellmode and index("!&*<>()[]", c) == nil) {
		    --curchar;
		    t = getident(chkalias);
		} else {
		    t = c;
		}
		break;
	}
    }
    chkalias = false;
#   ifdef LEXDEBUG
	if (lexdebug) {
	    fprintf(stderr, "yylex returns ");
	    print_token(stderr, t);
	    fprintf(stderr, "\n");
	}
#   endif
    return t;
}

/*
 * Put the given string before the current character
 * in the current line, thus inserting it into the input stream.
 */

public insertinput (s)
String s;
{
    register char *p, *q;
    int need, avail, shift;

    q = s;
    need = strlen(q);
    avail = curchar - &scanner_linebuf[0];
    if (need <= avail) {
	curchar = &scanner_linebuf[avail - need];
	p = curchar;
	while (*q != '\0') {
	    *p++ = *q++;
	}
    } else {
	p = curchar;
	while (*p != '\0') {
	    ++p;
	}
	shift = need - avail;
	if (p + shift >= &scanner_linebuf[MAXLINESIZE]) {
	    error("alias expansion too large");
	}
	for (;;) {
	    *(p + shift) = *p;
	    if (p == curchar) {
		break;
	    }
	    --p;
	}
	p = &scanner_linebuf[0];
	while (*q != '\0') {
	    *p++ = *q++;
	}
	curchar = &scanner_linebuf[0];
    }
}

/*
 * Get the actuals for a macro call.
 */

private String movetochar (str, c)
String str;
char c;
{
    register char *p;

    while (*p != c) {
	if (*p == '\0') {
	    error("missing ')' in macro call");
	} else if (*p == ')') {
	    error("not enough parameters in macro call");
	} else if (*p == ',') {
	    error("too many parameters in macro call");
	}
	++p;
    }
    return p;
}

private String *getactuals (n)
integer n;
{
    String *a;
    register char *p;
    int i;

    a = newarr(String, n);
    p = curchar;
    while (*p != '(') {
	if (lexclass[*p] != WHITE) {
	    error("missing actuals for macro");
	}
	++p;
    }
    ++p;
    for (i = 0; i < n - 1; i++) {
	a[i] = p;
	p = movetochar(p, ',');
	*p = '\0';
	++p;
    }
    a[n-1] = p;
    p = movetochar(p, ')');
    *p = '\0';
    curchar = p + 1;
    return a;
}

/*
 * Do command macro expansion, assuming curchar points to the beginning
 * of the actuals, and we are not in shell mode.
 */

private expand (pl, str)
List pl;
String str;
{
    char buf[4096], namebuf[100];
    register char *p, *q, *r;
    String *actual;
    Name n;
    integer i;
    boolean match;

    if (pl == nil) {
	insertinput(str);
    } else {
	actual = getactuals(list_size(pl));
	p = buf;
	q = str;
	while (*q != '\0') {
	    if (p >= &buf[4096]) {
		error("alias expansion too large");
	    }
	    if (lexclass[*q] == ALPHA) {
		r = namebuf;
		do {
		    *r++ = *q++;
		} while (isalnum(*q));
		*r = '\0';
		i = 0;
		match = false;
		foreach(Name, n, pl)
		    if (streq(ident(n), namebuf)) {
			match = true;
			break;
		    }
		    ++i;
		endfor
		if (match) {
		    r = actual[i];
		} else {
		    r = namebuf;
		}
		while (*r != '\0') {
		    *p++ = *r++;
		}
	    } else {
		*p++ = *q++;
	    }
	}
	*p = '\0';
	insertinput(buf);
    }
}

/*
 * Parser error handling.
 */

public yyerror(s)
String s;
{
    register char *p;
    register integer start;

    if (streq(s, "syntax error")) {
	beginerrmsg();
	p = prevchar;
	start = p - &scanner_linebuf[0];
	if (p > &scanner_linebuf[0]) {
	    while (lexclass[*p] == WHITE and p > &scanner_linebuf[0]) {
		--p;
	    }
	}
	fprintf(stderr, "%s", scanner_linebuf);
	if (start != 0) {
	    fprintf(stderr, "%*c", start, ' ');
	}
	if (p == &scanner_linebuf[0]) {
	    fprintf(stderr, "^ unrecognized command");
	} else {
	    fprintf(stderr, "^ syntax error");
	}
	enderrmsg();
    } else {
	error(s);
    }
}

/*
 * Eat the current line.
 */

public gobble ()
{
    curchar = scanner_linebuf;
    scanner_linebuf[0] = '\0';
}

/*
 * Scan an identifier.
 *
 * If chkalias is true, check first to see if it's an alias.
 * Otherwise, check to see if it's a keyword.
 */

private Token getident (chkalias)
boolean chkalias;
{
    char buf[1024];
    register char *p, *q;
    register Token t;
    List pl;
    String str;

    p = curchar;
    q = buf;
    if (shellmode) {
	do {
	    *q++ = *p++;
	} while (index(" \t\n!&<>*[]()'\"", *p) == nil);
    } else {
	do {
	    *q++ = *p++;
	} while (isalnum(*p));
    }
    curchar = p;
    *q = '\0';
    yylval.y_name = identname(buf, false);
    if (chkalias) {
	if (findalias(yylval.y_name, &pl, &str)) {
	    expand(pl, str);
	    while (lexclass[*curchar] == WHITE) {
		++curchar;
	    }
	    if (pl == nil) {
		t = getident(false);
	    } else {
		t = getident(true);
	    }
	} else if (shellmode) {
	    t = NAME;
	} else {
	    t = findkeyword(yylval.y_name, NAME);
	}
    } else if (shellmode) {
	t = NAME;
    } else {
	t = findkeyword(yylval.y_name, NAME);
    }
    return t;
}

/*
 * Scan a number.
 */

private Token getnum()
{
    char buf[1024];
    register Char *p, *q;
    register Token t;
    Integer base;

    p = curchar;
    q = buf;
    if (*p == '0') {
	if (*(p+1) == 'x') {
	    p += 2;
	    base = 16;
	} else if (*(p+1) == 't') {
	    base = 10;
	} else if (varIsSet("$hexin")) {
	    base = 16;
	} else {
	    base = 8;
	}
    } else if (varIsSet("$hexin")) {
	base = 16;
    } else if (varIsSet("$octin")) {
	base = 8;
    } else {
	base = 10;
    }
    if (base == 16) {
	do {
	    *q++ = *p++;
	} while (ishexdigit(*p));
    } else {
	do {
	    *q++ = *p++;
	} while (isdigit(*p));
    }
    if (*p == '.') {
	do {
	    *q++ = *p++;
	} while (isdigit(*p));
	if (*p == 'e' or *p == 'E') {
	    p++;
	    if (*p == '+' or *p == '-' or isdigit(*p)) {
		*q++ = 'e';
		do {
		    *q++ = *p++;
		} while (isdigit(*p));
	    }
	}
	*q = '\0';
	yylval.y_real = atof(buf);
	t = REAL;
    } else {
	*q = '\0';
	switch (base) {
	    case 10:
		yylval.y_int = atol(buf);
		break;

	    case 8:
		yylval.y_int = octal(buf);
		break;

	    case 16:
		yylval.y_int = hex(buf);
		break;

	    default:
		badcaseval(base);
	}
	t = INT;
    }
    curchar = p;
    return t;
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
    for (p = s; *p != '\0'; p++) {
	n = 8*n + (*p - '0');
    }
    return n;
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
	n *= 16;
	if (*p >= 'a' and *p <= 'f') {
	    n += (*p - 'a' + 10);
	} else if (*p >= 'A' and *p <= 'F') {
	    n += (*p - 'A' + 10);
	} else {
	    n += (*p - '0');
	}
    }
    return n;
}

/*
 * Scan a string.
 */

private Token getstring (quote)
char quote;
{
    register char *p, *q;
    char buf[MAXLINESIZE];
    boolean endofstring;
    Token t;

    p = curchar;
    q = buf;
    endofstring = false;
    while (not endofstring) {
	if (*p == '\\' and *(p+1) == '\n') {
	    if (fgets(scanner_linebuf, MAXLINESIZE, in) == nil) {
		error("non-terminated string");
	    }
	    p = &scanner_linebuf[0] - 1;
	} else if (*p == '\n' or *p == '\0') {
	    error("non-terminated string");
	    endofstring = true;
	} else if (*p == quote) {
	    endofstring = true;
	} else {
	    curchar = p;
	    *q++ = charcon(p);
	    p = curchar;
	}
	p++;
    }
    curchar = p;
    *q = '\0';
    if (quote == '\'' and buf[1] == '\0') {
	yylval.y_char = buf[0];
	t = CHAR;
    } else {
	yylval.y_string = strdup(buf);
	t = STRING;
    }
    return t;
}

/*
 * Process a character constant.
 * Watch out for backslashes.
 */

private char charcon (s)
String s;
{
    register char *p, *q;
    char c, buf[10];

    p = s;
    if (*p == '\\') {
	++p;
	switch (*p) {
	    case '\\':
		c = '\\';
		break;

	    case 'n':
		c = '\n';
		break;

	    case 'r':
		c = '\r';
		break;

	    case 't':
		c = '\t';
		break;

	    case '\'':
	    case '"':
		c = *p;
		break;

	    default:
		if (isdigit(*p)) {
		    q = buf;
		    do {
			*q++ = *p++;
		    } while (isdigit(*p));
		    *q = '\0';
		    c = (char) octal(buf);
		}
		--p;
		break;
	}
	curchar = p;
    } else {
	c = *p;
    }
    return c;
}

/*
 * Input file management routines.
 */

public setinput(filename)
Filename filename;
{
    File f;

    f = fopen(filename, "r");
    if (f == nil) {
	error("can't open %s", filename);
    } else {
	if (curinclindex >= MAXINCLDEPTH) {
	    error("unreasonable input nesting on \"%s\"", filename);
	}
	inclinfo[curinclindex].savefile = in;
	inclinfo[curinclindex].savefn = errfilename;
	inclinfo[curinclindex].savelineno = errlineno;
	curinclindex++;
	in = f;
	errfilename = filename;
	errlineno = 1;
    }
}

private Boolean eofinput()
{
    register Boolean b;

    if (curinclindex == 0) {
	if (isterm(in)) {
	    putchar('\n');
	    clearerr(in);
	    b = false;
	} else {
	    b = true;
	}
    } else {
	fclose(in);
	--curinclindex;
	in = inclinfo[curinclindex].savefile;
	errfilename = inclinfo[curinclindex].savefn;
	errlineno = inclinfo[curinclindex].savelineno;
	b = false;
    }
    return b;
}

/*
 * Pop the current input.  Return whether successful.
 */

public Boolean popinput()
{
    Boolean b;

    if (curinclindex == 0) {
	b = false;
    } else {
	b = (Boolean) (not eofinput());
    }
    return b;
}

/*
 * Return whether we are currently reading from standard input.
 */

public Boolean isstdin()
{
    return (Boolean) (in == stdin);
}

/*
 * Send the current line to the shell.
 */

public shellline()
{
    register char *p;

    p = curchar;
    while (*p != '\0' and (*p == '\n' or lexclass[*p] == WHITE)) {
	++p;
    }
    shell(p);
    if (*p == '\0' and isterm(in)) {
	putchar('\n');
    }
    erecover();
}

/*
 * Read the rest of the current line in "shell mode".
 */

public beginshellmode()
{
    shellmode = true;
}

/*
 * Print out a token for debugging.
 */

public print_token(f, t)
File f;
Token t;
{
    if (t == '\n') {
	fprintf(f, "char '\\n'");
    } else if (t == EOF) {
	fprintf(f, "EOF");
    } else if (t < 256) {
	fprintf(f, "char '%c'", t);
    } else {
	fprintf(f, "\"%s\"", keywdstring(t));
    }
}
