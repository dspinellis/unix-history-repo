/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/* $Header: getc.c,v 2.5 85/08/22 16:02:44 timo Exp $ */

/* B editor -- read key definitions from file */

#include "b.h"
#include "feat.h"
#ifdef LINDA
#include "b1mem.h"
#define syserr EDsyserr
#else !LINDA
#define freemem(p) free(p)
#endif !LINDA
#include "file.h"
#include "keys.h"

#include <ctype.h>

extern bool dflag;

#define ESC '\033'

/*
This file contains a little parser for key definition files.
To allow sufficient freedom in preparing such a file, a simple
grammar has been defined according to which the file is parsed.
The parsing process is extremely simple, as it can be done
top-down using recursive descent.


Lexical conventions:

- Blanks between lexical symbols are gnored.
- From '#' to end of line is comment (except inside strings).
- Strings are delimited by single or double quotes and
  use the same escape sequences as C strings, plus:
  \e or \E means an ESCape ('\033').
- Command names are like C identifiers ([a-zA-Z_][a-zA-Z0-9_]*).
  Upper/lower case distinction is significant.
- numbers are octal or decimal integers in C-style (leading zero means octal)
- After '^' a character is expected, this must be a letter or one of @^_[]\ .

Syntax in modified BNF ([] mean 0 or 1, * means 0 or more, + means 1 or more):

   file: line*
   line: [def] [comment]
   def: commandname '=' rhs
   rhs: item+
   item: string | '^' character | number


Notes:

- A definition for command "term_init" defines a string to be sent
  TO the terminal at initialization time, e.g. to set programmable
  function key definitions.  Similar for "term_done" on exiting.
- Command names are  conventional editor commands.

*/


#ifndef LINDA
/* Defines subroutine that used to be in the support levels: */

Hidden string getmem(nbytes)
	unsigned nbytes;
{
	string malloc();
	string pointer= malloc(nbytes);

	if (pointer == NULL)
		syserr("memory full in initkeys");
	return pointer;
}

Hidden string regetmem(pp, nbytes)
	string *pp;
	unsigned nbytes;
{
	*pp= realloc(*pp, nbytes);
	if (*pp == NULL)
		syserr("memory full in initkeys (regetmem)");
}
#endif !LINDA


#define COMMENT '#' /* Not B-like but very UNIX-like */
#define MAXDEFS 100

Hidden FILE *fp; /* File from which to read */
Hidden string filename; /* File name for error messages */
Hidden char nextc; /* Next character to be analyzed */
Hidden bool eof; /* EOF seen? */
Hidden int lcount; /* Current line number */
Hidden bool errcount; /* Number of errors detected */


struct tabent {
	int code;
	string name;
	string def;
};

/* Table of key definitions, mostly filled by reading definitions from a file.
   The "I" macro has two arguments: the default for termcap and that for
   the IBM PC.  It expands to either depending on whether IBMPC is defined.
   'def' fields initialized with a string starting with '=' are termcap names,
   and are replaced by the corresponding termcap entry (NULL if none).
   On the IBM PC, 'extended codes' are by convention a null character
   followed by another character (usually the scan code).  Since the null
   character is rather unsuitable for use in C strings, we use \377 (hex FF)
   instead, a code which has no assigned graphic is the extended IBM PC
   character set.  E.g., F1 is 0-59, which we encode as \377\073 (since
   \073 is octal for 59 decimal).  For the exact codes, see for instance the
   BASIC 2.0 manual, appendix G, or the XT Technical Reference, page 2-14.
*/

#ifdef IBMPC
#define I(tc, ibm) ibm
#else !IBMPC
#define I(tc, ibm) tc
#endif !IBMPC

Visible struct tabent deftab[MAXDEFS] = {
	/* General rule:
	   unix => ctrl-x
	   IBM  => alt-x
	   where x is first letter of command name
	*/
	{0377, "ignore", NULL}, /* Entry to ignore a key */
	{COPY, "copy", I(NULL, "\377\056")},
	{DELETE, "delete", I(NULL, "\377\040")},
	{DELETE, "delete", I(NULL, "\377\123")}, /* IBM DEL key */
	{ACCEPT, "accept", I(NULL, "\377\022")}, /* ^E, alt-E */
	{ACCEPT, "end", I(NULL, "\377\117")}, /* IBM END key */
	{'\t', "tab", NULL}, /* = ACCEPT in Bed, insert tab in Linda */
	{UNDO, "undo"}, /* Always backspace = ^H */
	{REDRAW, "redraw", I(NULL, "\377\046")}, /* ^L, alt-L */
	{REDRAW, "look"},
	{RETURN, "newline"}, /* Always ^M */
	{REDO, "redo", I(NULL, "\177")}, /* IBM ctrl-BS = ASCII 177 (DEL) */
	{EXIT, "exit", I(NULL, "\377\055")}, /* ^X, alt-X */

#ifdef RECORDING
	/*
	 * The IBM-PC has a problem here in ANSI.SYS mode: ctrl-P is
	 * unusable because it means Print Screen, and alt-R is unusable
	 * because it transmits 0, 19 but 19 is ctrl-S which means stop
	 * output :-(.
	 * The only reasonable place to put the things would then be on
	 * function keys.  You should do this in the key definitions file. (?)
	 */
	{PLAYBACK, "play", I(NULL, "\377\031")},
	{PLAYBACK, "playback", I(NULL, "\377\031")},
	{RECORD, "record", I(NULL, "\377\023")},
#endif RECORDING

#ifdef LINDA
	{BFIND, "bfind", I(NULL, "\377\060")},
	{FIND, "find", I(NULL, "\377\041")},
	{GLOBAL, "global", I(NULL, "\377\042")},
	{JOIN, "join", I(NULL, "\377\044")},
	{TOGGLE, "toggle", I(NULL, "\377\024")},
	{YANK, "yank", I(NULL, "\377\025")},
	{LITERAL, "literal", I(NULL, "\377\057")}, /* ^V, alt-V */
#endif LINDA

	{WIDEN, "widen", I("=k1", "\377\073")}, /* IBM F1 */
	{NARROW, "narrow", I("=k2", "\377\075")}, /* IBM F3 (!!!) */
	{NARROW, "first"},
	{RNARROW, "rnarrow", I("=k3", "\377\076")}, /* IBM F4 (!!!) */
	{RNARROW, "last"},
	{EXTEND, "extend", I("=k4", "\377\074")}, /* IBM F2 (!!!) */
	{UPARROW, "up", I("=ku", "\377\110")},
	{UPLINE, "upline", I("=k5", "\377\110")},
	{LEFTARROW, "left", I("=kl", "\377\113")},
	{PREVIOUS, "previous", I("=k6", NULL)},
	{RITEARROW, "right", I("=kr", "\377\115")},
	{NEXT, "next", I("=k7", NULL)},
	{DOWNARROW, "down", I("=kd", "\377\120")},
	{DOWNLINE, "downline", I("=k8", "\377\120")},

	{GOTO, "goto", I("\033g", NULL)}, /* Doesn't exist on IBM */
#ifdef HELPFUL
	{HELP, "help", I("\033?", "\377\104")}, /* ESC ?, IBM F10 */
#endif HELPFUL

	{0, "term_init", I("=ks", NULL)},
	{0, "term_done", I("=ke", NULL)},
};

#undef I

Hidden int ndefs;


Hidden Procedure err(fmt, arg)
	string fmt, arg;
{
	if (errcount == 0)
		fprintf(stderr, "Errors in key definitions file:\n");
	++errcount;
	fprintf(stderr, "%s, line %d: ", filename, lcount);
	fprintf(stderr, fmt, arg);
	fprintf(stderr, "\n");
}

Hidden Procedure adv()
{
	int c;

	if (eof)
		return;
	c= getc(fp);
	if (c == EOF) {
		nextc= '\n';
		eof= Yes;
	}
	else {
		nextc= c;
		if (c == '\n')
			++lcount;
	}
}

Hidden Procedure skipsp()
{
	while (nextc == ' ' || nextc == '\t')
		adv();
}

Hidden int lookup(name)
	string name;
{
	int i;

	for (i= 0; i < ndefs; ++i) {
		if (deftab[i].name != NULL && strcmp(name, deftab[i].name) == 0)
			return i;
	}
	return -1;
}

Hidden Procedure store(code, name, def)
	int code;
	string name;
	string def;
{
	struct tabent *d, *last= deftab+ndefs; 
	string p, q;

	/* Undefine conflicting definitions.  Conflicts arise
	   when a command definition is an initial subsequence
	   of another, or vice versa.  Key definitions (code < 0)
	   are not undefined. */
	if (code > 0) {
		for (d= deftab; d < last; ++d) {
			if (d->code >= 0 && d->def != NULL) {
				for (p= def, q= d->def; *p == *q; ++p, ++q) {
					if (*p == '\0' || *q == '\0') {
						d->def= NULL;
						break;
					}
				}
			}
		}
	}

	/* Find a free slot with the same code and NULL definition */
	/* (For code == 0, the name must match instead of the code,
	   and the definition need not be NULL) */
	for (d= deftab; d < last; ++d) {
		if (code == 0 ? strcmp(name, d->name) == 0
			: (d->code == code && d->def == NULL))
			break;
	}
	if (d == last) { /* Extend definition table */
		if (ndefs >= MAXDEFS) {
			err("Too many key definitions", "");
			return;
		}
		++ndefs;
		d->code= code;
		d->name= name;
	}
	d->def= def;
}

Hidden string savestr(s)
	string s;
{
	string new;

	new= getmem((unsigned) (strlen(s) + 1));
	strcpy(new, s);
	return new;
}

Hidden Procedure append(to, item)
	string *to, item;
{
	int len= strlen(*to) + strlen(item) + 1;
	regetmem(to, len);
	strcat(*to, item);
}

Hidden string getname()
{
	char buffer[20];
	string bp;

	if (!isalpha(nextc) && nextc != '_') {
		err("No name where expected", "");
		return NULL;
	}
	for (bp= buffer; isalnum(nextc) || nextc == '_'; ) {
		if (bp < buffer + sizeof buffer - 1)
			*bp++ = nextc;
		adv();
	}
	*bp= '\0';
	return savestr(buffer);
}

Hidden int getnumber()
{
	int base= (nextc == '0') ? 8 : 10;
	int i= 0;
	int d;

	for (;; adv()) {
		d= nextc-'0';
		if (d < 0 || d > 9)
			break;
		if (d > base) {
			err("8 or 9 in octal number", "");
			return 0;
		}
		i= i*base + d;
	}
	return i;
}

Hidden string getstring()
{
	char buf[256]; /* Arbitrary limit */
	char quote= nextc;
	char c;
	int len= 0;

	adv();
	while (nextc != quote) {
		if (nextc == '\n') {
			err("closing string quote not found", "");
			return NULL;
		}
		if (nextc != '\\') {
			c= nextc;
			adv();
		}
		else {
			adv();
			switch (nextc) {

			case 'r': c= '\r'; adv(); break;
			case 'n': c= '\n'; adv(); break;
			case 'b': c= '\b'; adv(); break;
			case 't': c= '\t'; adv(); break;
			case 'f': c= '\f'; adv(); break;

			case 'E':
			case 'e': c= ESC; adv(); break;

			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
				c= nextc-'0';
				adv();
				if (nextc >= '0' && nextc < '8') {
					c= 8*c + nextc-'0';
					adv();
					if (nextc >= '0' && nextc < '8') {
						c= 8*c + nextc-'0';
						adv();
					}
				}
				break;

			default: c=nextc; adv(); break;

			}
		}
		if (len >= sizeof buf) {
			err("string too long", "");
			return NULL;
		}
		buf[len++]= c;
	}
	adv();
	buf[len]= '\0';
	return savestr(buf);
}

Hidden string getitem()
{
	char buf[2];
	string keyname;
	int i;

	switch (nextc) {
	case '"':
	case '\'':
		return getstring();
	case '^':
		adv();
		if (isalpha(nextc) || index("@^_[]\\?", nextc)) {
			if (nextc == '?')
				buf[0]= '\177';
			else
				buf[0]= nextc & 037;
			buf[1]= '\0';
			adv();
			return savestr(buf);
		}
		err("Invalid character after '^'", "");
		return NULL;
	default:
		if (isdigit(nextc)) {
			buf[0]= getnumber();
			buf[1]= '\0';
			return savestr(buf);
		}
		if (isalpha(nextc) || nextc == '_') {
			keyname= getname(); /* Cannot fail */
			if (strlen(keyname) == 1)
				return savestr(keyname);
				/* Single letters stand for themselves */
			i= lookup(keyname);
			if (i < 0 || deftab[i].code <= 0) {
				err("%s: not a key name", keyname);
				freemem(keyname);
				return NULL;
			}
			else if (deftab[i].def == NULL) {
				err("%s: undefined key", keyname);
				freemem(keyname);
				return NULL;
			}
			else
				return savestr(deftab[i].def);
		}
		err("Invalid item", "");
		return NULL;
	}
}

Hidden string getrhs()
{
	string first, item;

	skipsp();
	first= getitem();
	if (first != NULL) {
		for (;;) {
			skipsp();
			if (nextc == '\n' || nextc == COMMENT)
				break;
			item= getitem();
			if (item == NULL) {
				freemem(first);
				return NULL;
			}
			append(&first, item);
			freemem(item);
		}
	}
	return first;
}

Hidden Procedure getdef()
{
	string name;
	int key;
	string rhs;

	name= getname();
	if (name == NULL)
		return;
	skipsp();
	if (nextc != '=') {
		err("Command name %s not followed by '='", name);
		return;
	}
	key= lookup(name);
	if (key < 0) {
		err("Unknown command: %s", name);
		return;
	}
	if (deftab[key].code < 0) {
		err("No redefinition of %s allowed", name);
		return;
	}
	adv();
	rhs= getrhs();
	if (rhs != NULL)
		store(deftab[key].code, name, rhs);
}

Hidden Procedure getline()
{
	adv();
	skipsp();
	if (nextc != COMMENT && nextc != '\n')
		getdef();
	while (nextc != '\n')
		adv();
}

#ifndef NDEBUG
Hidden Procedure dump(where)
	string where;
{
	int i;
	string s;

	printf("\nDump of key definitions %s.\n\n", where);
	printf("Code    Name            Definition\n");
	for (i= 0; i < ndefs; ++i) {
		printf("%04o    ", deftab[i].code);
		if (deftab[i].name != NULL)
			printf("%-15s ", deftab[i].name);
		else
			printf("%16s", "");
		s= deftab[i].def;
		if (s != NULL) {
			for (; *s != '\0'; ++s) {
				if (isascii(*s) && (isprint(*s) || *s == ' '))
					fputc(*s, stdout);
				else
					printf("\\%03o", *s&0377);
			}
		}
		printf("\n");
	}
	fflush(stdout);
}
#endif !NDEBUG

Hidden Procedure countdefs()
{
	struct tabent *d;

	d= deftab;
	while (d->name != NULL || d->code != 0 || d->def != NULL) {
		++d;
		if (d >= deftab+MAXDEFS)
			syserr("too many predefined keys");
	}
	ndefs= d-deftab;
}

Hidden Procedure process()
{
	errcount= 0;
	lcount= 1;
	eof= No;
	do {
		getline();
	} while (!eof);
}

Hidden bool try(dir, file, type)
	string dir, file, type;
{
	char buffer[200];

#ifdef IBMPC
	sprintf(buffer, "%.150s\\%.9s%.3s", dir, file, type);
#else !IBMPC
	sprintf(buffer, "%.150s/%.20s%.20s", dir, file, type);
#endif !IBMPC
	fp= fopen(buffer, "r");
	if (fp == NULL)
		return No;
	filename= buffer;
	process();
	fclose(fp);
#ifndef NDEBUG
	if (dflag)
		dump("after try");
#endif NDEBUG
	return Yes;
}

#ifndef IBMPC
Hidden Procedure readtermcap()
{
	string tgetstr();
	char buffer[1024]; /* Constant dictated by termcap manual entry */
	static char area[1024];
	string endarea= area;
	string anentry;
	struct tabent *d, *last;

	switch (tgetent(buffer, getenv("TERM"))) {

	default:
		fprintf(stderr, "*** Bad tgetent() return value.\n");
		/* Fall through */
	case -1:
		fprintf(stderr, "*** Can't read termcap.\n");
		/* Fall through again */
	case 0:
		fprintf(stderr, "*** No description for your terminal.\n");
		exit(1);

	case 1:
		break;
	}
	last= deftab+ndefs;
	for (d= deftab; d < last; ++d) {
		if (d->def != NULL && d->def[0] == '=') {
			anentry= tgetstr(d->def+1, &endarea);
			if (anentry != NULL && anentry[0] != '\0')
				d->def= anentry;
			else
				d->def= NULL;
		}
	}
}
#endif !IBMPC

Visible Procedure initkeys()
{
	string term= NULL;

	countdefs();
#ifndef NDEBUG
	if (dflag)
		dump("before termcap");
#endif NDEBUG
#ifndef IBMPC
	readtermcap();
#ifndef NDEBUG
	if (dflag)
		dump("after termcap");
#endif NDEBUG
	term= getenv("TERM");
	if (term != NULL && term[0] == '\0')
		term= NULL;
#endif !IBMPC
#ifdef DEBUG
	/* Try in the current directory. Only for debugging porpoises. */
	if (term != NULL)
		if (try(".", keyfile, term)) return;
#endif DEBUG
	if (term != NULL) {
		if (try(homedir, keyfile, term)) return;
		if (try(libdir, keyfile, term)) return;
	}
#ifdef DEBUG
	if (try(".", keyfile, deftype)) return;
#endif DEBUG
	if (try(homedir, keyfile, deftype)) return;
	if (try(libdir, keyfile, deftype)) return;
#ifndef NDEBUG
	printf("[No key definitions file found, using defaults.]\n");
#endif !NDEBUG
}


/* Output a named string to the terminal */

Hidden Procedure outstring(name)
	string name;
{
	int i= lookup(name);
	string def;

	if (i >= 0 && (def= deftab[i].def) != NULL)
		fputs(def, stdout);
}


/* Output the terminal's initialization sequence, if any. */

Visible Procedure
initgetc()
{
	outstring("term_init");
}


/* Output a sequence, if any, to return the terminal to a 'normal' state. */

Visible Procedure endgetc()
{
	outstring("term_done");
}


/* Read a command from the keyboard, decoding composite key definitions. */

#ifndef IBMPC
/* Strip high bit from input characters (matters only on PWB systems?) */
#define getch() (getchar() & 0177)
#endif !IBMPC

Visible int inchar()
{
	int c;
	struct tabent *d, *last;
	char buffer[100];
	int len;

	c= getch();
	if (c == EOF)
		return c;
#ifdef IBMPC
	if (c == 0)
		c= 0377;
#endif IBMPC
	last= deftab+ndefs;
	for (d= deftab; d < last; ++d) {
		if (d->code > 0 && d->def != NULL && c == (d->def[0] & 0377))
			break;
	}
	if (d == last) {
		if (c == ESC) {
			/* Kludge to make ESC-char by default equal to
			   char|MASK -- the command definitions do the rest:
			   e.g. WIDEN is 'w'|MASK, so ESC-w means WIDEN. */
			c= getch();
			if (c == EOF)
				return EOF;
			return (c&0177) | MASK;
		}
		return c;
	}
	if (d->def[1] == '\0')
		return d->code;
	buffer[0]= c;
	len= 1;
	for (;;) {
		c= getch();
		if (c == EOF)
			return EOF;
		buffer[len]= c;
		if (len < sizeof buffer - 1)
			++len;
		for (d= deftab; d < last; ++d) {
			if (d->code > 0 && d->def != NULL
				&& strncmp(buffer, d->def, len) == 0)
				break;
		}
		if (d == last) {
			if (buffer[0] == ESC && len == 2) {
				/* Same kludge as above */
				return c&0177 | MASK;
			}
			return 0377; /* Hope this rings a bell */
		}
		if (d->def[len] == '\0')
			return d->code;
	}
}
