/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)scanner.c 1.2 %G%";

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

#define MAXLINESIZE 1024

private File in;
private Char linebuf[MAXLINESIZE];
private Char *curchar;

#define MAXINCLDEPTH 10

private struct {
    File savefile;
    Filename savefn;
    int savelineno;
} inclinfo[MAXINCLDEPTH];

private unsigned int curinclindex;

private Boolean firsttoken = true;
private Boolean firstinit = true;

private Token getident();
private Token getnum();
private Token getstring();
private Boolean eofinput();
private Char charcon();
private Char charlookup();

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
    curchar = linebuf;
    linebuf[0] = '\0';
    if (runfirst) {
	firstinit = false;
	firsttoken = false;
    } else if (firstinit and isterm(in)) {
	firstinit = false;
	printf("> ");
	fflush(stdout);
    }
}

/*
 * Read a single token.
 *
 * Input is line buffered.
 *
 * There are two "modes" of operation:  one as in a compiler,
 * and one for reading shell-like syntax.
 */

private Boolean shellmode;

public Token yylex()
{
    register int c;
    register char *p;
    register Token t;
    String line;

    p = curchar;
    if (*p == '\0') {
	do {
	    if (isterm(in)) {
		if (firsttoken) {
		    firsttoken = false;
		} else {
		    printf("> ");
		    fflush(stdout);
		}
	    }
	    line = fgets(linebuf, MAXLINESIZE, in);
	} while (line == nil and not eofinput());
	if (line == nil) {
	    c = EOF;
	} else {
	    p = linebuf;
	    while (lexclass[*p] == WHITE) {
		p++;
	    }
	    shellmode = false;
	}
    } else {
	while (lexclass[*p] == WHITE) {
	    p++;
	}
    }
    curchar = p;
    c = *p;
    if (lexclass[c] == ALPHA) {
	t = getident();
    } else if (lexclass[c] == NUM) {
	t = getnum();
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
		t = getstring();
		break;

	    case '.':
		if (shellmode) {
		    --curchar;
		    t = getident();
		} else if (isdigit(*curchar)) {
		    --curchar;
		    t = getnum();
		} else {
		    t = '.';
		}
		break;

	    case '<':
		if (not shellmode and *curchar == '<') {
		    ++curchar;
		    t = LFORMER;
		} else {
		    t = '<';
		}
		break;

	    case '>':
		if (not shellmode and *curchar == '>') {
		    ++curchar;
		    t = RFORMER;
		} else {
		    t = '>';
		}
		break;

	    case '#':
		if (*curchar == '^') {
		    ++curchar;
		    t = ABSTRACTION;
		} else {
		    t = '#';
		}
		break;

	    case '-':
		if (shellmode) {
		    --curchar;
		    t = getident();
		} else if (*curchar == '>') {
		    ++curchar;
		    t = ARROW;
		} else {
		    t = '-';
		}
		break;

	    case EOF:
		t = 0;
		break;

	    default:
		if (shellmode and index("!&*()[]", c) == nil) {
		    --curchar;
		    t = getident();
		} else {
		    t = c;
		}
		break;
	}
    }
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
 * Parser error handling.
 */

public yyerror(s)
String s;
{
    register Char *p, *tokenbegin, *tokenend;
    register Integer len;

    if (streq(s, "syntax error")) {
	beginerrmsg();
	tokenend = curchar - 1;
	tokenbegin = tokenend;
	while (lexclass[*tokenbegin] != WHITE and tokenbegin > &linebuf[0]) {
	    --tokenbegin;
	}
	len = tokenend - tokenbegin + 1;
	p = tokenbegin;
	if (p > &linebuf[0]) {
	    while (lexclass[*p] == WHITE and p > &linebuf[0]) {
		--p;
	    }
	}
	if (p == &linebuf[0]) {
	    fprintf(stderr, "unrecognized command \"%.*s\"", len, tokenbegin);
	} else {
	    fprintf(stderr, "syntax error");
	    if (len != 0) {
		fprintf(stderr, " on \"%.*s\"", len, tokenbegin);
	    }
	}
	enderrmsg();
    } else {
	error(s);
    }
}

/*
 * Eat the current line.
 */

public gobble()
{
    curchar = linebuf;
    linebuf[0] = '\0';
}

/*
 * Scan an identifier and check to see if it's a keyword.
 */

private Token getident()
{
    char buf[256];
    register Char *p, *q;
    register Token t;

    p = curchar;
    q = buf;
    if (shellmode) {
	do {
	    *q++ = *p++;
	} while (index(" \t\n!&<>*[]()", *p) == nil);
    } else {
	do {
	    *q++ = *p++;
	} while (isalnum(*p));
    }
    curchar = p;
    *q = '\0';
    yylval.y_name = identname(buf, false);
    if (not shellmode) {
	t = findkeyword(yylval.y_name);
	if (t == nil) {
	    t = NAME;
	}
    } else {
	t = NAME;
    }
    return t;
}

/*
 * Scan a number.
 */

private Token getnum()
{
    char buf[256];
    register Char *p, *q;
    register Token t;
    Integer base;

    p = curchar;
    q = buf;
    if (*p == '0') {
	if (*(p+1) == 'x') {
	    p += 2;
	    base = 16;
	} else {
	    base = 8;
	}
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

private Token getstring()
{
    char buf[256];
    register Char *p, *q;
    Boolean endofstring;

    p = curchar;
    q = buf;
    endofstring = false;
    while (not endofstring) {
	if (*p == '\n' or *p == '\0') {
	    error("non-terminated string");
	    endofstring = true;
	} else if (*p == '"') {
	    if (*(p+1) != '"') {
		endofstring = true;
	    } else {
		*q++ = *p;
	    }
	} else {
	    *q++ = charcon(*p);
	}
	p++;
    }
    curchar = p;
    *q = '\0';
    yylval.y_string = strdup(buf);
    return STRING;
}

/*
 * Process a character constant.
 * Watch out for backslashes.
 */

private Char charcon(ch)
Char ch;
{
    Char c, buf[10], *p, *q;

    p = curchar;
    if (ch == '\\') {
	if (*p != '\\') {
	    q = buf;
	    do {
		*q++ = *p++;
	    } while (*p != '\\' and *p != '\n' and *p != '\0');
	    if (*p != '\\') {
		ungetc(*p, in);
		error("non-terminated character constant");
	    }
	    *q = '\0';
	    if (isdigit(buf[0])) {
		c = (Char) octal(buf);
	    } else {
		c = charlookup(buf);
	    }
	    curchar = p;
	} else {
	    c = '\\';
	}
    } else {
	c = ch;
    }
    return c;
}

/*
 * Do a lookup for a ASCII character name.
 */

private String ascii[] = {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    "BS",  "HT",  "NL",  "VT",  "NP",  "CR",  "SO",  "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
    "SP", nil
};

private char charlookup(s)
String s;
{
    register int i;

    for (i = 0; ascii[i] != NULL; i++) {
	if (streq(s, ascii[i])) {
	    return i;
	}
    }
    if (streq(s, "DEL")) {
	return 0177;
    }
    error("unknown ascii name \"%s\"", s);
    return '?';
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
