/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pascal.c	5.1 (Berkeley) %G%";
#endif not lint

static char rcsid[] = "$Header: pascal.c,v 1.5 84/12/26 10:41:18 linton Exp $";

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
private boolean initialized;

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
    initialized = false;
}

/*
 * Typematch tests if two types are compatible.  The issue
 * is a bit complicated, so several subfunctions are used for
 * various kinds of compatibility.
 */

private boolean builtinmatch (t1, t2)
register Symbol t1, t2;
{
    boolean b;

    b = (boolean) (
	(
	    t2 == t_int->type and
	    t1->class == RANGE and istypename(t1->type, "integer")
	) or (
	    t2 == t_char->type and
	    t1->class == RANGE and istypename(t1->type, "char")
	) or (
	    t2 == t_real->type and
	    t1->class == RANGE and istypename(t1->type, "real")
	) or (
	    t2 == t_boolean->type and
	    t1->class == RANGE and istypename(t1->type, "boolean")
	)
    );
    return b;
}

private boolean rangematch (t1, t2)
register Symbol t1, t2;
{
    boolean b;
    register Symbol rt1, rt2;

    if (t1->class == RANGE and t2->class == RANGE) {
	rt1 = rtype(t1->type);
	rt2 = rtype(t2->type);
	b = (boolean) (rt1->type == rt2->type);
    } else {
	b = false;
    }
    return b;
}

private boolean nilMatch (t1, t2)
register Symbol t1, t2;
{
    boolean b;

    b = (boolean) (
	(t1 == t_nil and t2->class == PTR) or
	(t1->class == PTR and t2 == t_nil)
    );
    return b;
}

private boolean enumMatch (t1, t2)
register Symbol t1, t2;
{
    boolean b;

    b = (boolean) (
	(t1->class == SCAL and t2->class == CONST and t2->type == t1) or
	(t1->class == CONST and t2->class == SCAL and t1->type == t2)
    );
    return b;
}

private boolean isConstString (t)
register Symbol t;
{
    boolean b;

    b = (boolean) (
	t->language == primlang and t->class == ARRAY and t->type == t_char
    );
    return b;
}

private boolean stringArrayMatch (t1, t2)
register Symbol t1, t2;
{
    boolean b;

    b = (boolean) (
	(
	    isConstString(t1) and
	    t2->class == ARRAY and compatible(t2->type, t_char->type)
	) or (
	    isConstString(t2) and
	    t1->class == ARRAY and compatible(t1->type, t_char->type)
	)
    );
    return b;
}

public boolean pascal_typematch (type1, type2)
Symbol type1, type2;
{
    boolean b;
    Symbol t1, t2, tmp;

    t1 = rtype(type1);
    t2 = rtype(type2);
    if (t1 == t2) {
	b = true;
    } else {
	if (t1 == t_char->type or t1 == t_int->type or
	    t1 == t_real->type or t1 == t_boolean->type
	) {
	    tmp = t1;
	    t1 = t2;
	    t2 = tmp;
	}
	b = (Boolean) (
	    builtinmatch(t1, t2) or rangematch(t1, t2) or
	    nilMatch(t1, t2) or enumMatch(t1, t2) or
	    stringArrayMatch(t1, t2)
	);
    }
    return b;
}

/*
 * Indent n spaces.
 */

private indent (n)
int n;
{
    if (n > 0) {
	printf("%*c", n, ' ');
    }
}

public pascal_printdecl (s)
Symbol s;
{
    register Symbol t;
    Boolean semicolon;

    semicolon = true;
    if (s->class == TYPEREF) {
	resolveRef(t);
    }
    switch (s->class) {
	case CONST:
	    if (s->type->class == SCAL) {
		semicolon = false;
		printf("enum constant, ord ");
		eval(s->symvalue.constval);
		pascal_printval(s);
	    } else {
		printf("const %s = ", symname(s));
		eval(s->symvalue.constval);
		pascal_printval(s);
	    }
	    break;

	case TYPE:
	    printf("type %s = ", symname(s));
	    printtype(s, s->type, 0);
	    break;

	case TYPEREF:
	    printf("type %s", symname(s));
	    break;

	case VAR:
	    if (isparam(s)) {
		printf("(parameter) %s : ", symname(s));
	    } else {
		printf("var %s : ", symname(s));
	    }
	    printtype(s, s->type, 0);
	    break;

	case REF:
	    printf("(var parameter) %s : ", symname(s));
	    printtype(s, s->type, 0);
	    break;

	case RANGE:
	case ARRAY:
	case RECORD:
	case VARNT:
	case PTR:
	case FILET:
	    printtype(s, s, 0);
	    semicolon = false;
	    break;

	case FVAR:
	    printf("(function variable) %s : ", symname(s));
	    printtype(s, s->type, 0);
	    break;

	case FIELD:
	    printf("(field) %s : ", symname(s));
	    printtype(s, s->type, 0);
	    break;

	case PROC:
	    printf("procedure %s", symname(s));
	    listparams(s);
	    break;

	case PROG:
	    printf("program %s", symname(s));
	    listparams(s);
	    break;

	case FUNC:
	    printf("function %s", symname(s));
	    listparams(s);
	    printf(" : ");
	    printtype(s, s->type, 0);
	    break;

	case MODULE:
	    printf("module %s", symname(s));
	    break;

	  /*
	   * the parameter list of the following should be printed
	   * eventually
	   */
	case  FPROC:
	    printf("procedure %s()", symname(s));
	    break;
	
	case FFUNC:
	    printf("function %s()", symname(s));
	    break;

	default:
	    printf("%s : (class %s)", symname(s), classname(s));
	    break;
    }
    if (semicolon) {
	putchar(';');
    }
    putchar('\n');
}

/*
 * Recursive whiz-bang procedure to print the type portion
 * of a declaration.
 *
 * The symbol associated with the type is passed to allow
 * searching for type names without getting "type blah = blah".
 */

private printtype (s, t, n)
Symbol s;
Symbol t;
int n;
{
    register Symbol tmp;

    if (t->class == TYPEREF) {
	resolveRef(t);
    }
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
		    printtype(tmp, tmp, n);
		    tmp = tmp->chain;
		    if (tmp == nil) {
			break;
		    }
		    printf(", ");
		}
	    }
	    printf("] of ");
	    printtype(t, t->type, n);
	    break;

	case RECORD:
	    printRecordDecl(t, n);
	    break;

	case FIELD:
	    if (t->chain != nil) {
		printtype(t->chain, t->chain, n);
	    }
	    printf("\t%s : ", symname(t));
	    printtype(t, t->type, n);
	    printf(";\n");
	    break;

	case RANGE:
	    printRangeDecl(t);
	    break;

	case PTR:
	    printf("^");
	    printtype(t, t->type, n);
	    break;

	case TYPE:
	    if (t->name != nil and ident(t->name)[0] != '\0') {
		printname(stdout, t);
	    } else {
		printtype(t, t->type, n);
	    }
	    break;

	case SCAL:
	    printEnumDecl(t, n);
	    break;

	case SET:
	    printf("set of ");
	    printtype(t, t->type, n);
	    break;

	case FILET:
	    printf("file of ");
	    printtype(t, t->type, n);
	    break;

	case TYPEREF:
	    break;
	
	case FPROC:
	    printf("procedure");
	    break;
	    
	case FFUNC:
	    printf("function");
	    break;

	default:
	    printf("(class %d)", t->class);
	    break;
    }
}

/*
 * Print out a record declaration.
 */

private printRecordDecl (t, n)
Symbol t;
int n;
{
    register Symbol f;

    if (t->chain == nil) {
	printf("record end");
    } else {
	printf("record\n");
	for (f = t->chain; f != nil; f = f->chain) {
	    indent(n+4);
	    printf("%s : ", symname(f));
	    printtype(f->type, f->type, n+4);
	    printf(";\n");
	}
	indent(n);
	printf("end");
    }
}

/*
 * Print out the declaration of a range type.
 */

private printRangeDecl (t)
Symbol t;
{
    long r0, r1;

    r0 = t->symvalue.rangev.lower;
    r1 = t->symvalue.rangev.upper;
    if (t == t_char or istypename(t, "char")) {
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
}

/*
 * Print out an enumeration declaration.
 */

private printEnumDecl (e, n)
Symbol e;
int n;
{
    Symbol t;

    printf("(");
    t = e->chain;
    if (t != nil) {
	printf("%s", symname(t));
	t = t->chain;
	while (t != nil) {
	    printf(", %s", symname(t));
	    t = t->chain;
	}
    }
    printf(")");
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

public pascal_printval (s)
Symbol s;
{
    prval(s, size(s));
}

private prval (s, n)
Symbol s;
integer n;
{
    Symbol t;
    Address a;
    integer len;
    double r;
    integer i;

    if (s->class == TYPEREF) {
	resolveRef(s);
    }
    switch (s->class) {
	case CONST:
	case TYPE:
	case REF:
	case VAR:
	case FVAR:
	case TAG:
	    prval(s->type, n);
	    break;

	case FIELD:
		prval(s->type, n);
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if (t == t_char->type or
		(t->class == RANGE and istypename(t->type, "char"))
	    ) {
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
	    printf("[variant]");
	    break;

	case RANGE:
	    printrange(s, n);
	    break;

	case FILET:
	    a = pop(Address);
	    if (a == 0) {
		printf("nil");
	    } else {
		printf("0x%x", a);
	    }
	    break;

	case PTR:
	    a = pop(Address);
	    if (a == 0) {
		printf("nil");
	    } else {
		printf("0x%x", a);
	    }
	    break;

	case SCAL:
	    i = 0;
	    popn(n, &i);
	    if (s->symvalue.iconval < 256) {
		i &= 0xff;
	    } else if (s->symvalue.iconval < 65536) {
		i &= 0xffff;
	    }
	    printEnum(i, s);
	    break;

	case FPROC:
	case FFUNC:
	    a = pop(long);
	    t = whatblock(a);
	    if (t == nil) {
		printf("(proc 0x%x)", a);
	    } else {
		printf("%s", symname(t));
	    }
	    break;

	case SET:
	    printSet(s);
	    break;

	default:
	    if (ord(s->class) < ord(BADUSE) or ord(s->class) > ord(TYPEREF)) {
		panic("printval: bad class %d", ord(s->class));
	    }
	    printf("[%s]", classname(s));
	    break;
    }
}

/*
 * Print out the value of a scalar (non-enumeration) type.
 */

private printrange (s, n)
Symbol s;
integer n;
{
    double d;
    float f;
    integer i;

    if (s->symvalue.rangev.upper == 0 and s->symvalue.rangev.lower > 0) {
	if (n == sizeof(float)) {
	    popn(n, &f);
	    d = f;
	} else {
	    popn(n, &d);
	}
	prtreal(d);
    } else {
	i = 0;
	popn(n, &i);
	printRangeVal(i, s);
    }
}

/*
 * Print out a set.
 */

private printSet (s)
Symbol s;
{
    Symbol t;
    integer nbytes;

    nbytes = size(s);
    t = rtype(s->type);
    printf("[");
    sp -= nbytes;
    if (t->class == SCAL) {
	printSetOfEnum(t);
    } else if (t->class == RANGE) {
	printSetOfRange(t);
    } else {
	error("internal error: expected range or enumerated base type for set");
    }
    printf("]");
}

/*
 * Print out a set of an enumeration.
 */

private printSetOfEnum (t)
Symbol t;
{
    register Symbol e;
    register integer i, j, *p;
    boolean first;

    p = (int *) sp;
    i = *p;
    j = 0;
    e = t->chain;
    first = true;
    while (e != nil) {
	if ((i&1) == 1) {
	    if (first) {
		first = false;
		printf("%s", symname(e));
	    } else {
		printf(", %s", symname(e));
	    }
	}
	i >>= 1;
	++j;
	if (j >= sizeof(integer)*BITSPERBYTE) {
	    j = 0;
	    ++p;
	    i = *p;
	}
	e = e->chain;
    }
}

/*
 * Print out a set of a subrange type.
 */

private printSetOfRange (t)
Symbol t;
{
    register integer i, j, *p;
    long v;
    boolean first;

    p = (int *) sp;
    i = *p;
    j = 0;
    v = t->symvalue.rangev.lower;
    first = true;
    while (v <= t->symvalue.rangev.upper) {
	if ((i&1) == 1) {
	    if (first) {
		first = false;
		printf("%ld", v);
	    } else {
		printf(", %ld", v);
	    }
	}
	i >>= 1;
	++j;
	if (j >= sizeof(integer)*BITSPERBYTE) {
	    j = 0;
	    ++p;
	    i = *p;
	}
	++v;
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

    t = rtype(a->nodetype);
    if (t->class != ARRAY) {
	beginerrmsg();
	prtree(stderr, a);
	fprintf(stderr, " is not an array");
	enderrmsg();
    } else {
	r = a;
	eltype = t->type;
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

public pascal_evalaref (s, base, i)
Symbol s;
Address base;
long i;
{
    Symbol t;
    long lb, ub;

    t = rtype(s);
    s = rtype(t->chain);
    findbounds(s, &lb, &ub);
    if (i < lb or i > ub) {
	error("subscript %d out of range [%d..%d]", i, lb, ub);
    }
    push(long, base + (i - lb) * size(t->type));
}

/*
 * Initial Pascal type information.
 */

#define NTYPES 4

private Symbol inittype[NTYPES + 1];

private addType (n, s, lower, upper)
integer n;
String s;
long lower, upper;
{
    register Symbol t;

    if (n > NTYPES) {
	panic("initial Pascal type number too large for '%s'", s);
    }
    t = insert(identname(s, true));
    t->language = pasc;
    t->class = TYPE;
    t->type = newSymbol(nil, 0, RANGE, t, nil);
    t->type->symvalue.rangev.lower = lower;
    t->type->symvalue.rangev.upper = upper;
    t->type->language = pasc;
    inittype[n] = t;
}

private initTypes ()
{
    addType(1, "boolean", 0L, 1L);
    addType(2, "char", 0L, 255L);
    addType(3, "integer", 0x80000000L, 0x7fffffffL);
    addType(4, "real", 8L, 0L);
    initialized = true;
}

/*
 * Initialize typetable.
 */

public pascal_modinit (typetable)
Symbol typetable[];
{
    register integer i;

    if (not initialized) {
	initTypes();
	initialized = true;
    }
    for (i = 1; i <= NTYPES; i++) {
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
