/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)printsym.c 1.2 %G%";

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
#include "main.h"

#ifndef public
#endif

/*
 * Return a pointer to the string for the name of the class that
 * the given symbol belongs to.
 */

private String clname[] = {
    "bad use", "constant", "type", "variable", "array", "fileptr",
    "record", "field", "procedure", "function", "funcvar",
    "ref", "pointer", "file", "set", "range", "label", "withptr",
    "scalar", "string", "program", "improper", "variant",
    "procparam", "funcparam", "module", "typeref", "tag"
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
	printf("\nentering %s %s\n", classname(s), symname(s));
    }
}

/*
 * Note the exit of the given block
 */

public printexit(s)
Symbol s;
{
    if (s != program) {
	printf("leaving %s %s\n\n", classname(s), symname(s));
    }
}

/*
 * Note the call of s from t.
 */

public printcall(s, t)
Symbol s, t;
{
    printf("calling %s", symname(s));
    printparams(s, nil);
    printf(" from %s %s\n", classname(t), symname(t));
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
    if (s->class == FUNC) {
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
    printf("from %s\n", symname(s));
}

/*
 * Print the values of the parameters of the given procedure or function.
 * The frame distinguishes recursive instances of a procedure.
 */

public printparams(f, frame)
Symbol f;
Frame frame;
{
    Symbol param;
    int n, m, s;

    n = nargspassed(frame);
    param = f->chain;
    if (param != nil or n > 0) {
	printf("(");
	m = n;
	if (param != nil) {
	    for (;;) {
		s = size(param) div sizeof(Word);
		if (s == 0) {
		    s = 1;
		}
		m -= s;
		printv(param, frame);
		param = param->chain;
	    if (param == nil) break;
		printf(", ");
	    }
	}
	if (m > 0) {
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
	printf(")");
    }
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
	    t = rtype(s->type);
	    b = (Boolean) (
		not isparam(s) and
		t != nil and t->class != FILET and t->class != SET
	    );
	    break;

	default:
	    b = false;
	    break;
    }
    return b;
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
    if (isvarparam(s)) {
	rpush(address(s, frame), sizeof(Address));
	addr = pop(Address);
	len = size(s->type);
    } else {
	addr = address(s, frame);
	len = size(s);
    }
    if (canpush(len)) {
	rpush(addr, len);
	printval(s->type);
    } else {
	printf("*** expression too large ***");
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
    checkref(s);
    (*language_op(s->language, L_PRINTDECL))(s);
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
    if (s->block->name != nil) {
	printf(" (");
	printname(stdout, s->block);
	putchar(')');
    }
    putchar('\n');
    switch (s->class) {
	case VAR:
	case REF:
	    if (s->level >= 3) {
		printf("address\t0x%x\n", s->symvalue.offset);
	    } else {
		printf("offset\t%d\n", s->symvalue.offset);
	    }
	    break;

	case RECORD:
	case VARNT:
	    printf("size\t%d\n", s->symvalue.offset);
	    break;

	case FIELD:
	    printf("offset\t%d\n", s->symvalue.field.offset);
	    printf("size\t%d\n", s->symvalue.field.length);
	    break;

	case PROC:
	case FUNC:
	    printf("address\t0x%x\n", s->symvalue.funcv.beginaddr);
	    break;

	case RANGE:
	    printf("lower\t%d\n", s->symvalue.rangev.lower);
	    printf("upper\t%d\n", s->symvalue.rangev.upper);
	    break;

	default:
	    /* do nothing */
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
    switch (t->class) {
	case PROC:
	case FUNC:
	    s = pop(Symbol);
	    printf("%s", symname(s));
	    break;

	default:
	    if (t->language == nil) {
		error("unknown language");
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
    if (s->chain == nil) {
	error("record has no fields");
    }
    printf("(");
    sp -= size(s);
    printfield(s->chain);
    printf(")");
}

/*
 * Print out a field, first printing out other fields.
 * This is done because the fields are chained together backwards.
 */

private printfield(s)
Symbol s;
{
    Stack *savesp;

    if (s->chain != nil) {
	printfield(s->chain);
	printf(", ");
    }
    printf("%s = ", symname(s));
    savesp = sp;
    sp += ((s->symvalue.field.offset div BITSPERBYTE) + size(s->type));
    printval(s);
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
    sp -= size(a);
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

    sprintf(buf, "%g", r);
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
    } else {
	putchar(c);
    }
}
