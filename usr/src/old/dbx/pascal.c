/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)pascal.c 1.2 12/15/82";

static char rcsid[] = "$Header: pascal.c,v 1.3 84/03/27 10:23:04 linton Exp $";

/*
 * Pascal-dependent symbol routines.
 */

#include "defs.h"
#include "symbols.h"
#include "pascal.h"
#include "languages.h"
#include "tree.h"
#include "eval.h"
#include "mappings.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"

#ifndef public
#endif

private Language pasc;

/*
 * Initialize Pascal information.
 */

public pascal_init()
{
    pasc = language_define("pascal", ".p");
    language_setop(pasc, L_PRINTDECL, pascal_printdecl);
    language_setop(pasc, L_PRINTVAL, pascal_printval);
    language_setop(pasc, L_TYPEMATCH, pascal_typematch);
    language_setop(pasc, L_BUILDAREF, pascal_buildaref);
    language_setop(pasc, L_EVALAREF, pascal_evalaref);
    language_setop(pasc, L_MODINIT, pascal_modinit);
    language_setop(pasc, L_HASMODULES, pascal_hasmodules);
    language_setop(pasc, L_PASSADDR, pascal_passaddr);
    initTypes();
}

/*
 * Compatible tests if two types are compatible.  The issue
 * is complicated a bit by ranges.
 *
 * Integers and reals are not compatible since they cannot always be mixed.
 */

public Boolean pascal_typematch(type1, type2)
Symbol type1, type2;
{
    Boolean b;
    register Symbol t1, t2;

    t1 = rtype(t1);
    t2 = rtype(t2);
    b = (Boolean)
	(t1->type == t2->type and (
	    (t1->class == RANGE and t2->class == RANGE) or
	    (t1->class == SCAL and t2->class == CONST) or
	    (t1->class == CONST and t2->class == SCAL) or
	    (t1->type == t_char and t1->class == ARRAY and t2->class == ARRAY)
	) or
	(t1 == t_nil and t2->class == PTR) or
	(t1->class == PTR and t2 == t_nil)
    );
    return b;
}

public pascal_printdecl(s)
Symbol s;
{
    register Symbol t;
    Boolean semicolon;

    semicolon = true;
    switch (s->class) {
	case CONST:
	    if (s->type->class == SCAL) {
		printf("(enumeration constant, ord %ld)",
		    s->symvalue.iconval);
	    } else {
		printf("const %s = ", symname(s));
		printval(s);
	    }
	    break;

	case TYPE:
	    printf("type %s = ", symname(s));
	    printtype(s, s->type);
	    break;

	case VAR:
	    if (isparam(s)) {
		printf("(parameter) %s : ", symname(s));
	    } else {
		printf("var %s : ", symname(s));
	    }
	    printtype(s, s->type);
	    break;

	case REF:
	    printf("(var parameter) %s : ", symname(s));
	    printtype(s, s->type);
	    break;

	case RANGE:
	case ARRAY:
	case RECORD:
	case VARNT:
	case PTR:
	    printtype(s, s);
	    semicolon = false;
	    break;

	case FVAR:
	    printf("(function variable) %s : ", symname(s));
	    printtype(s, s->type);
	    break;

	case FIELD:
	    printf("(field) %s : ", symname(s));
	    printtype(s, s->type);
	    break;

	case PROC:
	    printf("procedure %s", symname(s));
	    listparams(s);
	    break;

	case PROG:
	    printf("program %s", symname(s));
	    t = s->chain;
	    if (t != nil) {
		printf("(%s", symname(t));
		for (t = t->chain; t != nil; t = t->chain) {
		    printf(", %s", symname(t));
		}
		printf(")");
	    }
	    break;

	case FUNC:
	    printf("function %s", symname(s));
	    listparams(s);
	    printf(" : ");
	    printtype(s, s->type);
	    break;

	default:
	    error("class %s in printdecl", classname(s));
    }
    if (semicolon) {
	putchar(';');
    }
    putchar('\n');
}

/*
 * Recursive whiz-bang procedure to print the type portion
 * of a declaration.  Doesn't work quite right for variant records.
 *
 * The symbol associated with the type is passed to allow
 * searching for type names without getting "type blah = blah".
 */

private printtype(s, t)
Symbol s;
Symbol t;
{
    register Symbol tmp;

    switch (t->class) {
	case VAR:
	case CONST:
	case FUNC:
	case PROC:
	    panic("printtype: class %s", classname(t));
	    break;

	case ARRAY:
	    printf("array[");
	    tmp = t->chain;
	    if (tmp != nil) {
		for (;;) {
		    printtype(tmp, tmp);
		    tmp = tmp->chain;
		    if (tmp == nil) {
			break;
		    }
		    printf(", ");
		}
	    }
	    printf("] of ");
	    printtype(t, t->type);
	    break;

	case RECORD:
	    printf("record\n");
	    if (t->chain != nil) {
		printtype(t->chain, t->chain);
	    }
	    printf("end");
	    break;

	case FIELD:
	    if (t->chain != nil) {
		printtype(t->chain, t->chain);
	    }
	    printf("\t%s : ", symname(t));
	    printtype(t, t->type);
	    printf(";\n");
	    break;

	case RANGE: {
	    long r0, r1;

	    r0 = t->symvalue.rangev.lower;
	    r1 = t->symvalue.rangev.upper;
	    if (t == t_char or istypename(t,"char")) {
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
	}

	case PTR:
	    putchar('*');
	    printtype(t, t->type);
	    break;

	case TYPE:
	    if (symname(t) != nil) {
		printf("%s", symname(t));
	    } else {
		printtype(t, t->type);
	    }
	    break;

	case SCAL:
	    printf("(");
	    t = t->chain;
	    if (t != nil) {
		printf("%s", symname(t));
		t = t->chain;
		while (t != nil) {
		    printf(", %s", symname(t));
		    t = t->chain;
		}
	    } else {
		panic("empty enumeration");
	    }
	    printf(")");
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

private listparams(s)
Symbol s;
{
    Symbol t;

    if (s->chain != nil) {
	putchar('(');
	for (t = s->chain; t != nil; t = t->chain) {
	    switch (t->class) {
		case REF:
		    printf("var ");
		    break;

		case FPROC:
		    printf("procedure ");
		    break;

		case FFUNC:
		    printf("function ");
		    break;

		case VAR:
		    break;

		default:
		    panic("unexpected class %d for parameter", t->class);
	    }
	    printf("%s : ", symname(t));
	    printtype(t, t->type);
	    if (t->chain != nil) {
		printf("; ");
	    }
	}
	putchar(')');
    }
}

/*
 * Print out the value on the top of the expression stack
 * in the format for the type of the given symbol.
 */

public pascal_printval(s)
Symbol s;
{
    Symbol t;
    Address a;
    int len;
    double r;

    switch (s->class) {
	case CONST:
	case TYPE:
	case VAR:
	case REF:
	case FVAR:
	case TAG:
	case FIELD:
	    pascal_printval(s->type);
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if (t->class==RANGE and istypename(t->type,"char")) {
		len = size(s);
		sp -= len;
		printf("'%.*s'", len, sp);
		break;
	    } else {
		printarray(s);
	    }
	    break;

	case RECORD:
	    printrecord(s);
	    break;

	case VARNT:
	    error("can't print out variant records");
	    break;


	case RANGE:
	    if (s == t_boolean) {
		printf(((Boolean) popsmall(s)) == true ? "true" : "false");
	    } else if (s == t_char or istypename(s,"char")) {
		printf("'%c'", pop(char));
	    } else if (s->symvalue.rangev.upper == 0 and
			s->symvalue.rangev.lower > 0) {
		switch (s->symvalue.rangev.lower) {
		    case sizeof(float):
			prtreal(pop(float));
			break;

		    case sizeof(double):
			prtreal(pop(double));
			break;

		    default:
			panic("bad real size %d", s->symvalue.rangev.lower);
			break;
		}
	    } else if (s->symvalue.rangev.lower >= 0) {
		printf("%lu", popsmall(s));
	    } else {
		printf("%ld", popsmall(s));
	    }
	    break;

	case FILET:
	case PTR: {
	    Address addr;

	    addr = pop(Address);
	    if (addr == 0) {
		printf("0, (nil)");
	    } else {
		printf("0x%x, 0%o", addr, addr);
	    }
	    break;
	}


	case SCAL: {
	    int scalar;
	    Boolean found;

	    scalar = popsmall(s);
	    found = false;
	    for (t = s->chain; t != nil; t = t->chain) {
		if (t->symvalue.iconval == scalar) {
		    printf("%s", symname(t));
		    found = true;
		    break;
		}
	    }
	    if (not found) {
		printf("(scalar = %d)", scalar);
	    }
	    break;
	}

	case FPROC:
	case FFUNC:
	{
	    Address a;

	    a = fparamaddr(pop(long));
	    t = whatblock(a);
	    if (t == nil) {
		printf("(proc %d)", a);
	    } else {
		printf("%s", symname(t));
	    }
	    break;
	}

	default:
	    if (ord(s->class) < ord(BADUSE) or ord(s->class) > ord(TYPEREF)) {
		panic("printval: bad class %d", ord(s->class));
	    }
	    error("don't know how to print a %s", classname(s));
	    /* NOTREACHED */
    }
}

/*
 * Construct a node for subscripting.
 */

public Node pascal_buildaref (a, slist)
Node a, slist;
{
    register Symbol t;
    register Node p;
    Symbol etype, atype, eltype;
    Node esub, r;

    r = a;
    t = rtype(a->nodetype);
    eltype = t->type;
    if (t->class != ARRAY) {
	beginerrmsg();
	prtree(stderr, a);
	fprintf(stderr, " is not an array");
	enderrmsg();
    } else {
	p = slist;
	t = t->chain;
	for (; p != nil and t != nil; p = p->value.arg[1], t = t->chain) {
	    esub = p->value.arg[0];
	    etype = rtype(esub->nodetype);
	    atype = rtype(t);
	    if (not compatible(atype, etype)) {
		beginerrmsg();
		fprintf(stderr, "subscript ");
		prtree(stderr, esub);
		fprintf(stderr, " is the wrong type");
		enderrmsg();
	    }
	    r = build(O_INDEX, r, esub);
	    r->nodetype = eltype;
	}
	if (p != nil or t != nil) {
	    beginerrmsg();
	    if (p != nil) {
		fprintf(stderr, "too many subscripts for ");
	    } else {
		fprintf(stderr, "not enough subscripts for ");
	    }
	    prtree(stderr, a);
	    enderrmsg();
	}
    }
    return r;
}

/*
 * Evaluate a subscript index.
 */

public int pascal_evalaref (s, i)
Symbol s;
long i;
{
    long lb, ub;

    s = rtype(rtype(s)->chain);
    lb = s->symvalue.rangev.lower;
    ub = s->symvalue.rangev.upper;
    if (i < lb or i > ub) {
	error("subscript %d out of range [%d..%d]", i, lb, ub);
    }
    return (i - lb);
}

/*
 * Initial Pascal type information.
 */

#define NTYPES 4

private Symbol inittype[NTYPES];
private integer count;

private addType (s, lower, upper)
String s;
long lower, upper;
{
    register Symbol t;

    if (count > NTYPES) {
	panic("too many initial types");
    }
    t = maketype(s, lower, upper);
    t->language = pasc;
    inittype[count] = t;
    ++count;
}

private initTypes ()
{
    count = 1;
    addType("integer", 0x80000000L, 0x7fffffffL);
    addType("char", 0L, 255L);
    addType("boolean", 0L, 1L);
    addType("real", 4L, 0L);
}

/*
 * Initialize typetable.
 */

public pascal_modinit (typetable)
Symbol typetable[];
{
    register integer i;

    for (i = 1; i < NTYPES; i++) {
	typetable[i] = inittype[i];
    }
}

public boolean pascal_hasmodules ()
{
    return false;
}

public boolean pascal_passaddr (param, exprtype)
Symbol param, exprtype;
{
    return false;
}
