/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)c.c	5.8 (Berkeley) 5/11/88";
#endif not lint

static char rcsid[] = "$Header: c.c,v 1.5 88/04/02 01:25:44 donn Exp $";

/*
 * C-dependent symbol routines.
 */

#include "defs.h"
#include "symbols.h"
#include "printsym.h"
#include "languages.h"
#include "c.h"
#include "tree.h"
#include "eval.h"
#include "operators.h"
#include "mappings.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"

#ifndef public
#   include "tree.h"
#endif

#define isdouble(range) ( \
    range->symvalue.rangev.upper == 0 and range->symvalue.rangev.lower > 0 \
)

#define isrange(t, name) (t->class == RANGE and istypename(t->type, name))

private Language langC;
private Language langCplpl;

/*
 * Initialize C language information.
 */

public c_init()
{
    langC = language_define("c", ".c");
    language_setop(langC, L_PRINTDECL, c_printdecl);
    language_setop(langC, L_PRINTVAL, c_printval);
    language_setop(langC, L_TYPEMATCH, c_typematch);
    language_setop(langC, L_BUILDAREF, c_buildaref);
    language_setop(langC, L_EVALAREF, c_evalaref);
    language_setop(langC, L_MODINIT, c_modinit);
    language_setop(langC, L_HASMODULES, c_hasmodules);
    language_setop(langC, L_PASSADDR, c_passaddr);

    langCplpl = language_define("c++", "..c");
    language_setop(langCplpl, L_PRINTDECL, c_printdecl);
    language_setop(langCplpl, L_PRINTVAL, c_printval);
    language_setop(langCplpl, L_TYPEMATCH, c_typematch);
    language_setop(langCplpl, L_BUILDAREF, c_buildaref);
    language_setop(langCplpl, L_EVALAREF, c_evalaref);
    language_setop(langCplpl, L_MODINIT, c_modinit);
    language_setop(langCplpl, L_HASMODULES, c_hasmodules);
    language_setop(langCplpl, L_PASSADDR, c_passaddr);
}

/*
 * Test if two types are compatible.
 */

public Boolean c_typematch(type1, type2)
Symbol type1, type2;
{
    Boolean b;
    register Symbol t1, t2, tmp;

    t1 = type1;
    t2 = type2;
    if (t1 == t2) {
	b = true;
    } else {
	t1 = rtype(t1);
	t2 = rtype(t2);
	if (t1 == t_char->type or t1 == t_int->type or t1 == t_real->type) {
	    tmp = t1;
	    t1 = t2;
	    t2 = tmp;
	}
	b = (Boolean) (
	    (
		isrange(t1, "int") and !isdouble(t1) /* sigh */ and
		(t2 == t_int->type or t2 == t_char->type)
	    ) or (
		isrange(t1, "char") and
		(t2 == t_char->type or t2 == t_int->type)
	    ) or (
		t1->class == RANGE and isdouble(t1) and t2 == t_real->type
	    ) or (
		t1->class == RANGE and t2->class == RANGE and
		t1->symvalue.rangev.lower == t2->symvalue.rangev.lower and
		t1->symvalue.rangev.upper == t2->symvalue.rangev.upper
	    ) or (
		t1->class != RANGE and t1->type == t2->type and (
		    (t1->class == t2->class) or
		    (t1->class == SCAL and t2->class == CONST) or
		    (t1->class == CONST and t2->class == SCAL)
		)
	    ) or (
		t1->class == PTR and c_typematch(t1->type, t_char) and
		t2->class == ARRAY and c_typematch(t2->type, t_char) and
		t2->language == primlang
	    )
	);
    }
    return b;
}

/*
 * Print out the declaration of a C variable.
 */

public c_printdecl(s)
Symbol s;
{
    printdecl(s, 0);
}

private printdecl(s, indent)
register Symbol s;
Integer indent;
{
    register Symbol t;
    Boolean semicolon, newline;

    semicolon = true;
    newline = true;
    if (indent > 0) {
	printf("%*c", indent, ' ');
    }
    if (s->class == TYPE) {
	printf("typedef ");
    }
    switch (s->class) {
	case CONST:
	    if (s->type->class == SCAL) {
		printf("enumeration constant with value ");
		eval(s->symvalue.constval);
		c_printval(s);
	    } else {
		printf("const %s = ", symname(s));
		printval(s);
	    }
	    break;

	case TYPE:
	case VAR:
	    if (s->class != TYPE and s->storage == INREG) {
		printf("register ");
	    }
	    if (s->type->class == ARRAY) {
		printtype(s->type, s->type->type, indent);
		t = rtype(s->type->chain);
		assert(t->class == RANGE);
		printf(" %s[%d]", symname(s), t->symvalue.rangev.upper + 1);
	    } else {
		printtype(s, s->type, indent);
		if (s->type->class != PTR) {
		    printf(" ");
		}
		printf("%s", symname(s));
	    }
	    break;

	case FIELD:
	    if (s->type->class == ARRAY) {
		printtype(s->type, s->type->type, indent);
		t = rtype(s->type->chain);
		assert(t->class == RANGE);
		printf(" %s[%d]", symname(s), t->symvalue.rangev.upper + 1);
	    } else {
		printtype(s, s->type, indent);
		if (s->type->class != PTR) {
		    printf(" ");
		}
		printf("%s", symname(s));
	    }
	    if (isbitfield(s)) {
		printf(" : %d", s->symvalue.field.length);
	    }
	    break;

	case TAG:
	    if (s->type == nil) {
		findtype(s);
		if (s->type == nil) {
		    error("unexpected missing type information");
		}
	    }
	    printtype(s, s->type, indent);
	    break;

	case RANGE:
	case ARRAY:
	case RECORD:
	case VARNT:
	case PTR:
	case FFUNC:
	    semicolon = false;
	    printtype(s, s, indent);
	    break;

	case SCAL:
	    printf("(enumeration constant, value %d)", s->symvalue.iconval);
	    break;

	case PROC:
	    semicolon = false;
	    printf("%s", symname(s));
	    c_listparams(s);
	    newline = false;
	    break;

	case FUNC:
	    semicolon = false;
	    if (not istypename(s->type, "void")) {
		printtype(s, s->type, indent);
		printf(" ");
	    }
	    printf("%s", symname(s));
	    c_listparams(s);
	    newline = false;
	    break;

	case MODULE:
	    semicolon = false;
	    printf("source file \"%s.c\"", symname(s));
	    break;

	case PROG:
	    semicolon = false;
	    printf("executable file \"%s\"", symname(s));
	    break;

	default:
	    printf("[%s]", classname(s));
	    break;
    }
    if (semicolon) {
	putchar(';');
    }
    if (newline) {
	putchar('\n');
    }
}

/*
 * Recursive whiz-bang procedure to print the type portion
 * of a declaration.
 *
 * The symbol associated with the type is passed to allow
 * searching for type names without getting "type blah = blah".
 */

private printtype(s, t, indent)
Symbol s;
Symbol t;
Integer indent;
{
    register Symbol i;
    long r0, r1;
    register String p;

    checkref(s);
    checkref(t);
    switch (t->class) {
	case VAR:
	case CONST:
	case PROC:
	    panic("printtype: class %s", classname(t));
	    break;

	case ARRAY:
	    printf("array[");
	    i = t->chain;
	    if (i != nil) {
		for (;;) {
		    printtype(i, i, indent);
		    i = i->chain;
		    if (i == nil) {
			break;
		    }
		    printf(", ");
		}
	    }
	    printf("] of ");
	    printtype(t, t->type, indent);
	    break;

	case RECORD:
	case VARNT:
	    printf("%s ", c_classname(t));
	    if (s->name != nil and s->class == TAG) {
		p = symname(s);
		if (p[0] == '$' and p[1] == '$') {
		    printf("%s ", &p[2]);
		} else {
		    printf("%s ", p);
		}
	    }
	    printf("{\n");
	    for (i = t->chain; i != nil; i = i->chain) {
		assert(i->class == FIELD);
		printdecl(i, indent+4);
	    }
	    if (indent > 0) {
		printf("%*c", indent, ' ');
	    }
	    printf("}");
	    break;

	case RANGE:
	    r0 = t->symvalue.rangev.lower;
	    r1 = t->symvalue.rangev.upper;
	    if (istypename(t->type, "char")) {
		if (r0 < 0x20 or r0 > 0x7e) {
		    printf("%ld..", r0);
		} else {
		    printf("'%c'..", (char) r0);
		}
		if (r1 < 0x20 or r1 > 0x7e) {
		    printf("\\%lo", r1);
		} else {
		    printf("'%c'", (char) r1);
		}
	    } else if (r0 > 0 and r1 == 0) {
		printf("%ld byte real", r0);
	    } else if (r0 >= 0) {
		printf("%lu..%lu", r0, r1);
	    } else {
		printf("%ld..%ld", r0, r1);
	    }
	    break;

	case PTR:
	    printtype(t, t->type, indent);
	    if (t->type->class != PTR) {
		printf(" ");
	    }
	    printf("*");
	    break;

	case FUNC:
	case FFUNC:
	    printtype(t, t->type, indent);
	    printf("()");
	    break;

	case TYPE:
	    if (t->name != nil) {
		printname(stdout, t);
	    } else {
		printtype(t, t->type, indent);
	    }
	    break;

	case TYPEREF:
	    printf("@%s", symname(t));
	    break;

	case SCAL:
	    printf("enum ");
	    if (s->name != nil and s->class == TAG) {
		printf("%s ", symname(s));
	    }
	    printf("{ ");
	    i = t->chain;
	    if (i != nil) {
		for (;;) {
		    printf("%s", symname(i));
		    i = i->chain;
		if (i == nil) break;
		    printf(", ");
		}
	    }
	    printf(" }");
	    break;

	case TAG:
	    if (t->type == nil) {
		printf("unresolved tag %s", symname(t));
	    } else {
		i = rtype(t->type);
		printf("%s %s", c_classname(i), symname(t));
	    }
	    break;

	default:
	    printf("(class %d)", t->class);
	    break;
    }
}

/*
 * List the parameters of a procedure or function.
 * No attempt is made to combine like types.
 */

public c_listparams(s)
Symbol s;
{
    register Symbol t;

    putchar('(');
    for (t = s->chain; t != nil; t = t->chain) {
	printf("%s", symname(t));
	if (t->chain != nil) {
	    printf(", ");
	}
    }
    putchar(')');
    if (s->chain != nil) {
	printf("\n");
	for (t = s->chain; t != nil; t = t->chain) {
	    if (t->class != VAR) {
		panic("unexpected class %d for parameter", t->class);
	    }
	    printdecl(t, 0);
	}
    } else {
	putchar('\n');
    }
}

/*
 * Print out the value on the top of the expression stack
 * in the format for the type of the given symbol.
 */

public c_printval(s)
Symbol s;
{
    register Symbol t;
    register Address a;
    integer i, len;
    register String str;

    switch (s->class) {
	case CONST:
	case TYPE:
	case VAR:
	case REF:
	case FVAR:
	case TAG:
	    c_printval(s->type);
	    break;

	case FIELD:
	    if (isbitfield(s)) {
		i = extractField(s);
		t = rtype(s->type);
		if (t->class == SCAL) {
		    printEnum(i, t);
		} else {
		    printRangeVal(i, t);
		}
	    } else {
		c_printval(s->type);
	    }
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if ((t->class == RANGE and istypename(t->type, "char")) or
		t == t_char->type
	    ) {
		len = size(s);
		str = (String) (sp -= len);
		if (s->language != primlang) {
		    putchar('"');
		}
		while (--len > 0 and *str != '\0') {
		    printchar(*str++);
		}
		if (*str != '\0') {	/* XXX - pitch trailing null */
		    printchar(*str);
		}
		if (s->language != primlang) {
		    putchar('"');
		}
	    } else {
		printarray(s);
	    }
	    break;

	case RECORD:
	    c_printstruct(s);
	    break;

	case RANGE:
	    if (s == t_boolean->type or istypename(s->type, "boolean")) {
		printRangeVal(popsmall(s), s);
	    } else if (s == t_char->type or istypename(s->type, "char")) {
		printRangeVal(pop(char), s);
	    } else if (s == t_real->type or isdouble(s)) {
		switch (s->symvalue.rangev.lower) {
		    case sizeof(float):
			prtreal((double) (pop(float)));
			break;

		    case sizeof(double):
			prtreal(pop(double));
			break;

		    default:
			panic("bad real size %d", t->symvalue.rangev.lower);
			break;
		}
	    } else {
		printRangeVal(popsmall(s), s);
	    }
	    break;

	case PTR:
	    t = rtype(s->type);
	    a = pop(Address);
	    if (a == 0) {
		printf("(nil)");
	    } else if (t->class == RANGE and istypename(t->type, "char")) {
		printString(a, (boolean) (s->language != primlang));
	    } else {
		printf("0x%x", a);
	    }
	    break;

	case SCAL:
	    i = pop(Integer);
	    printEnum(i, s);
	    break;

	/*
	 * Unresolved structure pointers?
	 */
	case BADUSE:
	    a = pop(Address);
	    printf("@%x", a);
	    break;

	default:
	    if (ord(s->class) > ord(TYPEREF)) {
		panic("printval: bad class %d", ord(s->class));
	    }
	    sp -= size(s);
	    printf("[%s]", c_classname(s));
	    break;
    }
}

/*
 * Print out a C structure.
 */

private c_printstruct (s)
Symbol s;
{
    Symbol f;
    Stack *savesp;
    integer n, off, len;

    sp -= size(s);
    savesp = sp;
    printf("(");
    f = s->chain;
    for (;;) {
	off = f->symvalue.field.offset;
	len = f->symvalue.field.length;
	n = (off + len + BITSPERBYTE - 1) div BITSPERBYTE;
	sp += n;
	printf("%s = ", symname(f));
	c_printval(f);
	sp = savesp;
	f = f->chain;
    if (f == nil) break;
	printf(", ");
    }
    printf(")");
}

/*
 * Return the C name for the particular class of a symbol.
 */

public String c_classname(s)
Symbol s;
{
    String str;

    switch (s->class) {
	case RECORD:
	    str = "struct";
	    break;

	case VARNT:
	    str = "union";
	    break;

	case SCAL:
	    str = "enum";
	    break;

	default:
	    str = classname(s);
    }
    return str;
}

public Node c_buildaref(a, slist)
Node a, slist;
{
    register Symbol t;
    register Node p;
    Symbol etype, atype, eltype;
    Node r, esub;

    t = rtype(a->nodetype);
    eltype = t->type;
    if (t->class == PTR) {
	p = slist->value.arg[0];
	if (not compatible(p->nodetype, t_int)) {
	    beginerrmsg();
	    fprintf(stderr, "subscript must be integer-compatible");
	    enderrmsg();
	}
	r = build(O_MUL, p, build(O_LCON, (long) size(eltype)));
	r = build(O_ADD, build(O_RVAL, a), r);
	r->nodetype = eltype;
    } else if (t->class != ARRAY) {
	beginerrmsg();
	fprintf(stderr, "\"");
	prtree(stderr, a);
	fprintf(stderr, "\" is not an array");
	enderrmsg();
    } else {
	r = a;
	p = slist;
	t = t->chain;
	for (; p != nil and t != nil; p = p->value.arg[1], t = t->chain) {
	    esub = p->value.arg[0];
	    etype = rtype(esub->nodetype);
	    atype = rtype(t);
	    if (not compatible(atype, etype)) {
		beginerrmsg();
		fprintf(stderr, "subscript \"");
		prtree(stderr, esub);
		fprintf(stderr, "\" is the wrong type");
		enderrmsg();
	    }
	    r = build(O_INDEX, r, esub);
	    r->nodetype = eltype;
	}
	if (p != nil or t != nil) {
	    beginerrmsg();
	    if (p != nil) {
		fprintf(stderr, "too many subscripts for \"");
	    } else {
		fprintf(stderr, "not enough subscripts for \"");
	    }
	    prtree(stderr, a);
	    fprintf(stderr, "\"");
	    enderrmsg();
	}
    }
    return r;
}

/*
 * Evaluate a subscript index.
 */

public c_evalaref(s, base, i)
Symbol s;
Address base;
long i;
{
    Symbol t;
    long lb, ub;

    t = rtype(s);
    s = t->chain;
    lb = s->symvalue.rangev.lower;
    ub = s->symvalue.rangev.upper;
    if (i < lb or i > ub) {
	warning("subscript out of range");
    }
    push(long, base + (i - lb) * size(t->type));
}

/*
 * Initialize typetable information.
 */

public c_modinit (typetable)
Symbol typetable[];
{
    /* nothing right now */
}

public boolean c_hasmodules ()
{
    return false;
}

public boolean c_passaddr (param, exprtype)
Symbol param, exprtype;
{
    boolean b;
    Symbol t;

    t = rtype(exprtype);
    b = (boolean) (t->class == ARRAY);
    return b;
}
