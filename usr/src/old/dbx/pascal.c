/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)@(#)pascal.c 1.1 %G%";

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

/*
 * Initialize Pascal information.
 */

public pascal_init()
{
    Language lang;

    lang = language_define("pascal", ".p");
    language_setop(lang, L_PRINTDECL, pascal_printdecl);
    language_setop(lang, L_PRINTVAL, pascal_printval);
    language_setop(lang, L_TYPEMATCH, pascal_typematch);
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
	    if (t == t_char) {
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
	    t = t->type->chain;
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

    if (s->class == REF) {
	s = s->type;
    }
    switch (s->class) {
	case TYPE:
	    pascal_printval(s->type);
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if (t==t_char or (t->class==RANGE and t->type==t_char)) {
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
	    } else if (s == t_char) {
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

	case FIELD:
	    error("missing record specification");
	    break;

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
