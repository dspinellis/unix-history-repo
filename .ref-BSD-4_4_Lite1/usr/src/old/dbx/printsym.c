/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)printsym.c	5.7 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Printing of symbolic information.
 */

#include "defs.h"
#include "symbols.h"
#include "languages.h"
#include "printsym.h"
#include "tree.h"
#include "eval.h"
#include "mappings.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"
#include "names.h"
#include "keywords.h"
#include "main.h"
#include <ctype.h>

#ifndef public
#endif

/*
 * Maximum number of arguments to a function.
 * This is used as a check for the possibility that the stack has been
 * overwritten and therefore a saved argument pointer might indicate
 * to an absurdly large number of arguments.
 */

#define MAXARGSPASSED 20

/*
 * Return a pointer to the string for the name of the class that
 * the given symbol belongs to.
 */

private String clname[] = {
    "bad use", "constant", "type", "variable", "array", "array",
    "dynarray", "subarray", "fileptr", "record", "field",
    "procedure", "function", "funcvar",
    "ref", "pointer", "file", "set", "range", "label", "withptr",
    "scalar", "string", "program", "improper", "variant",
    "procparam", "funcparam", "module", "tag", "common", "extref", "typeref"
};

public String classname(s)
Symbol s;
{
    return clname[ord(s->class)];
}

/*
 * Note the entry of the given block, unless it's the main program.
 */

public printentry(s)
Symbol s;
{
    if (s != program) {
	printf("\nentering %s ", classname(s));
	printname(stdout, s);
	printf("\n");
    }
}

/*
 * Note the exit of the given block
 */

public printexit(s)
Symbol s;
{
    if (s != program) {
	printf("leaving %s ", classname(s));
	printname(stdout, s);
	printf("\n\n");
    }
}

/*
 * Note the call of s from t.
 */

public printcall(s, t)
Symbol s, t;
{
    printf("calling ");
    printname(stdout, s);
    printparams(s, nil);
    printf(" from %s ", classname(t));
    printname(stdout, t);
    printf("\n");
}

/*
 * Note the return from s.  If s is a function, print the value
 * it is returning.  This is somewhat painful, since the function
 * has actually just returned.
 */

public printrtn(s)
Symbol s;
{
    register Symbol t;
    register int len;
    Boolean isindirect;

    printf("returning ");
    if (s->class == FUNC && (!istypename(s->type,"void"))) {
	len = size(s->type);
	if (canpush(len)) {
	    t = rtype(s->type);
	    isindirect = (Boolean) (t->class == RECORD or t->class == VARNT);
	    pushretval(len, isindirect);
	    printval(s->type);
	    putchar(' ');
	} else {
	    printf("(value too large) ");
	}
    }
    printf("from ");
    printname(stdout, s);
    printf("\n");
}

/*
 * Print the values of the parameters of the given procedure or function.
 * The frame distinguishes recursive instances of a procedure.
 *
 * If the procedure or function is internal, the argument count is
 * not valid so we ignore it.
 */

public printparams(f, frame)
Symbol f;
Frame frame;
{
    Symbol param;
    int n, m, s;

    n = nargspassed(frame);
    if (isinternal(f)) {
	n = 0;
    }
    printf("(");
    param = f->chain;
    if (param != nil or n > 0) {
	m = n;
	if (param != nil) {
	    for (;;) {
		s = psize(param) div sizeof(Word);
		if (s == 0) {
		    s = 1;
		}
		m -= s;
		if (showaggrs) {
		    printv(param, frame);
		} else {
		    printparamv(param, frame);
		}
		param = param->chain;
	    if (param == nil) break;
		printf(", ");
	    }
	}
	if (m > 0) {
	    if (m > MAXARGSPASSED) {
		m = MAXARGSPASSED;
	    }
	    if (f->chain != nil) {
		printf(", ");
	    }
	    for (;;) {
		--m;
		printf("0x%x", argn(n - m, frame));
	    if (m <= 0) break;
		printf(", ");
	    }
	}
    }
    printf(")");
}

/*
 * Test if a symbol should be printed.  We don't print files,
 * for example, simply because there's no good way to do it.
 * The symbol must be within the given function.
 */

public Boolean should_print(s)
Symbol s;
{
    Boolean b;
    register Symbol t;

    switch (s->class) {
	case VAR:
	case FVAR:
	    if (isparam(s)) {
		b = false;
	    } else {
		t = rtype(s->type);
		if (t == nil) {
		    b = false;
		} else {
		    switch (t->class) {
			case FILET:
			case SET:
			case BADUSE:
			    b = false;
			    break;

			default:
			    b = true;
			    break;
		    }
		}
	    }
	    break;

	default:
	    b = false;
	    break;
    }
    return b;
}

/*
 * Print out a parameter value.
 *
 * Since this is intended to be printed on a single line with other information
 * aggregate values are not printed.
 */

public printparamv (p, frame)
Symbol p;
Frame frame;
{
    Symbol t;

    t = rtype(p->type);
    switch (t->class) {
	case ARRAY:
	case OPENARRAY:
	case DYNARRAY:
	case SUBARRAY:
	    t = rtype(t->type);
	    if (compatible(t, t_char)) {
		printv(p, frame);
	    } else {
		printf("%s = (...)", symname(p));
	    }
	    break;

	case RECORD:
	    printf("%s = (...)", symname(p));
	    break;

	default:
	    printv(p, frame);
	    break;
    }
}

/*
 * Print the name and value of a variable.
 */

public printv(s, frame)
Symbol s;
Frame frame;
{
    Address addr;
    int len;

    if (isambiguous(s) and ismodule(container(s))) {
	printname(stdout, s);
	printf(" = ");
    } else {
	printf("%s = ", symname(s));
    }
    if (isvarparam(s) and not isopenarray(s)) {
	rpush(address(s, frame), sizeof(Address));
	addr = pop(Address);
    } else {
	addr = address(s, frame);
    }
    len = size(s);
    if (not canpush(len)) {
	printf("*** expression too large ***");
    } else if (isreg(s)) {
	push(Address, addr);
	printval(s->type);
    } else {
	rpush(addr, len);
	printval(s->type);
    }
}

/*
 * Print out the name of a symbol.
 */

public printname(f, s)
File f;
Symbol s;
{
    if (s == nil) {
	fprintf(f, "(noname)");
    } else if (s == program) {
	fprintf(f, ".");
    } else if (isredirected() or isambiguous(s)) {
	printwhich(f, s);
    } else {
	fprintf(f, "%s", symname(s));
    }
}

/*
 * Print the fully specified variable that is described by the given identifer.
 */

public printwhich(f, s)
File f;
Symbol s;
{
    printouter(f, container(s));
    fprintf(f, "%s", symname(s));
}

/*
 * Print the fully qualified name of each symbol that has the same name
 * as the given symbol.
 */

public printwhereis(f, s)
File f;
Symbol s;
{
    register Name n;
    register Symbol t;

    checkref(s);
    n = s->name;
    t = lookup(n);
    printwhich(f, t);
    t = t->next_sym;
    while (t != nil) {
	if (t->name == n) {
	    putc(' ', f);
	    printwhich(f, t);
	}
	t = t->next_sym;
    }
    putc('\n', f);
}

private printouter(f, s)
File f;
Symbol s;
{
    Symbol outer;

    if (s != nil) {
	outer = container(s);
	if (outer != nil and outer != program) {
	    printouter(f, outer);
	}
	fprintf(f, "%s.", symname(s));
    }
}

public printdecl(s)
Symbol s;
{
    Language lang;

    checkref(s);
    if (s->language == nil or s->language == primlang) {
	lang = findlanguage(".s");
    } else {
	lang = s->language;
    }
    (*language_op(lang, L_PRINTDECL))(s);
}

/*
 * Straight dump of symbol information.
 */

public psym(s)
Symbol s;
{
    printf("name\t%s\n", symname(s));
    printf("lang\t%s\n", language_name(s->language));
    printf("level\t%d\n", s->level);
    printf("class\t%s\n", classname(s));
    printf("type\t0x%x", s->type);
    if (s->type != nil and s->type->name != nil) {
	printf(" (%s)", symname(s->type));
    }
    printf("\nchain\t0x%x", s->chain);
    if (s->chain != nil and s->chain->name != nil) {
	printf(" (%s)", symname(s->chain));
    }
    printf("\nblock\t0x%x", s->block);
    if (s->block != nil and s->block->name != nil) {
	printf(" (");
	printname(stdout, s->block);
	putchar(')');
    }
    putchar('\n');
    switch (s->class) {
	case TYPE:
	    printf("size\t%d\n", size(s));
	    break;

	case VAR:
	case REF:
	    switch (s->storage) {
		case INREG:
		    printf("reg\t%d\n", s->symvalue.offset);
		    break;

		case STK:
		    printf("offset\t%d\n", s->symvalue.offset);
		    break;

		case EXT:
		    printf("address\t0x%x\n", s->symvalue.offset);
		    break;
	    }
	    printf("size\t%d\n", size(s));
	    break;

	case RECORD:
	case VARNT:
	    printf("size\t%d\n", s->symvalue.offset);
	    break;

	case FIELD:
	    printf("offset\t%d\n", s->symvalue.field.offset);
	    printf("size\t%d\n", s->symvalue.field.length);
	    break;

	case PROG:
	case PROC:
	case FUNC:
	    printf("address\t0x%x\n", s->symvalue.funcv.beginaddr);
	    if (isinline(s)) {
		printf("inline procedure\n");
	    }
	    if (nosource(s)) {
		printf("does not have source information\n");
	    } else {
		printf("has source information\n");
	    }
	    break;

	case RANGE:
	    prangetype(s->symvalue.rangev.lowertype);
	    printf("lower\t%d\n", s->symvalue.rangev.lower);
	    prangetype(s->symvalue.rangev.uppertype);
	    printf("upper\t%d\n", s->symvalue.rangev.upper);
	    break;

	default:
	    /* do nothing */
	    break;
    }
}

private prangetype(r)
Rangetype r;
{
    switch (r) {
	case R_CONST:
	    printf("CONST");
	    break;

	case R_ARG:
	    printf("ARG");
	    break;

	case R_TEMP:
	    printf("TEMP");
	    break;

	case R_ADJUST:
	    printf("ADJUST");
	    break;
    }
}

/*
 * Print out the value on top of the stack according to the given type.
 */

public printval(t)
Symbol t;
{
    Symbol s;

    checkref(t);
    if (t->class == TYPEREF) {
	resolveRef(t);
    }
    switch (t->class) {
	case PROC:
	case FUNC:
	    s = pop(Symbol);
	    printf("%s", symname(s));
	    break;

	default:
	    if (t->language == nil or t->language == primlang) {
		(*language_op(findlanguage(".c"), L_PRINTVAL))(t);
	    } else {
		(*language_op(t->language, L_PRINTVAL))(t);
	    }
	    break;
    }
}

/*
 * Print out the value of a record, field by field.
 */

public printrecord(s)
Symbol s;
{
    Symbol f;

    if (s->chain == nil) {
	error("record has no fields");
    }
    printf("(");
    sp -= size(s);
    f = s->chain;
    if (f != nil) {
	for (;;) {
	    printfield(f);
	    f = f->chain;
	if (f == nil) break;
	    printf(", ");
	}
    }
    printf(")");
}

/*
 * Print out a field.
 */

private printfield(f)
Symbol f;
{
    Stack *savesp;
    register int off, len;

    printf("%s = ", symname(f));
    savesp = sp;
    off = f->symvalue.field.offset;
    len = f->symvalue.field.length;
    sp += ((off + len + BITSPERBYTE - 1) div BITSPERBYTE);
    printval(f);
    sp = savesp;
}

/*
 * Print out the contents of an array.
 * Haven't quite figured out what the best format is.
 *
 * This is rather inefficient.
 *
 * The "2*elsize" is there since "printval" drops the stack by elsize.
 */

public printarray(a)
Symbol a;
{
    Stack *savesp, *newsp;
    Symbol eltype;
    long elsize;
    String sep;

    savesp = sp;
    sp -= (size(a));
    newsp = sp;
    eltype = rtype(a->type);
    elsize = size(eltype);
    printf("(");
    if (eltype->class == RECORD or eltype->class == ARRAY or
      eltype->class == VARNT) {
	sep = "\n";
	putchar('\n');
    } else {
	sep = ", ";
    }
    for (sp += elsize; sp <= savesp; sp += 2*elsize) {
	if (sp - elsize != newsp) {
	    fputs(sep, stdout);
	}
	printval(eltype);
    }
    sp = newsp;
    if (streq(sep, "\n")) {
	putchar('\n');
    }
    printf(")");
}

/*
 * Print out the value of a real number in Pascal notation.
 * This is, unfortunately, different than what one gets
 * from "%g" in printf.
 */

public prtreal(r)
double r;
{
    extern char *index();
    char buf[256];

#   ifdef IRIS
	sprintf(buf, "%lg", r);
#   else
	sprintf(buf, "%g", r);
#   endif
    if (buf[0] == '.') {
	printf("0%s", buf);
    } else if (buf[0] == '-' and buf[1] == '.') {
	printf("-0%s", &buf[1]);
    } else {
	printf("%s", buf);
    }
    if (index(buf, '.') == nil) {
	printf(".0");
    }
}

/*
 * Print out a character using ^? notation for unprintables.
 */

public printchar(c)
char c;
{
    if (c == 0) {
	putchar('\\');
	putchar('0');
    } else if (c == '\n') {
	putchar('\\');
	putchar('n');
    } else if (c > 0 and c < ' ') {
	putchar('^');
	putchar(c - 1 + 'A');
    } else if (c >= ' ' && c <= '~') {
	putchar(c);
    } else {
	printf("\\0%o",c&0xff);
    }
}

/*
 * Print out a value for a range type (integer, char, or boolean).
 */

public printRangeVal (val, t)
long val;
Symbol t;
{
    if (t == t_boolean->type or istypename(t->type, "boolean")) {
	if ((boolean) val) {
	    printf("true");
	} else {
	    printf("false");
	}
    } else if (t == t_char->type or istypename(t->type, "char")) {
	if (varIsSet("$hexchars")) {
	    printf("0x%lx", val);
	} else {
	    putchar('\'');
	    printchar(val);
	    putchar('\'');
	}
    } else if (varIsSet("$hexints")) {
	printf("0x%lx", val);
    } else if (t->symvalue.rangev.lower >= 0) {
	printf("%lu", val);
    } else {
	printf("%ld", val);
    }
}

/*
 * Print out an enumerated value by finding the corresponding
 * name in the enumeration list.
 */

public printEnum (i, t)
integer i;
Symbol t;
{
    register Symbol e;

    e = t->chain;
    while (e != nil and e->symvalue.constval->value.lcon != i) {
	e = e->chain;
    }
    if (e != nil) {
	printf("%s", symname(e));
    } else {
	printf("%d", i);
    }
}

/*
 * Print out a null-terminated string (pointer to char)
 * starting at the given address.
 */

public printString (addr, quotes)
Address addr;
boolean quotes;
{
    register Address a;
    register integer i, len;
    register boolean endofstring;
    register int unprintables;
#define	MAXGARBAGE	4
    union {
	char ch[sizeof(Word)];
	int word;
    } u;

    if (varIsSet("$hexstrings")) {
	printf("0x%x", addr);
    } else {
	if (quotes) {
	    putchar('"');
	}
	a = addr;
	unprintables = 0;
	endofstring = false;
	while (not endofstring) {
	    dread(&u, a, sizeof(u));
	    i = 0;
	    do {
		if (u.ch[i] == '\0') {
		    endofstring = true;
		} else {
		    printchar(u.ch[i]);
		    if (!isascii(u.ch[i]) and ++unprintables > MAXGARBAGE) {
			endofstring = true;
			printf("...");
		    }
		}
		++i;
	    } while (i < sizeof(Word) and not endofstring);
	    a += sizeof(Word);
	}
	if (quotes) {
	    putchar('"');
	}
    }
}
