/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)modula-2.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Modula-2 specific symbol routines.
 */

#include "defs.h"
#include "symbols.h"
#include "modula-2.h"
#include "languages.h"
#include "tree.h"
#include "eval.h"
#include "mappings.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"

#ifndef public
#endif

private Language mod2;
private boolean initialized;


#define ischar(t) ( \
    (t) == t_char->type or \
    ((t)->class == RANGE and istypename((t)->type, "char")) \
)

/*
 * Initialize Modula-2 information.
 */

public modula2_init ()
{
    mod2 = language_define("modula-2", ".mod");
    language_setop(mod2, L_PRINTDECL, modula2_printdecl);
    language_setop(mod2, L_PRINTVAL, modula2_printval);
    language_setop(mod2, L_TYPEMATCH, modula2_typematch);
    language_setop(mod2, L_BUILDAREF, modula2_buildaref);
    language_setop(mod2, L_EVALAREF, modula2_evalaref);
    language_setop(mod2, L_MODINIT, modula2_modinit);
    language_setop(mod2, L_HASMODULES, modula2_hasmodules);
    language_setop(mod2, L_PASSADDR, modula2_passaddr);
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
	    t2 == t_int->type and t1->class == RANGE and
	    (
		istypename(t1->type, "integer") or
		istypename(t1->type, "cardinal")
	    )
	) or (
	    t2 == t_char->type and
	    t1->class == RANGE and istypename(t1->type, "char")
	) or (
	    t2 == t_real->type and
	    t1->class == RANGE and (
		istypename(t1->type, "real") or
		istypename(t1->type, "longreal")
	    )
	) or (
	    t2 == t_boolean->type and
	    t1->class == RANGE and istypename(t1->type, "boolean")
	)
    );
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

private boolean openArrayMatch (t1, t2)
register Symbol t1, t2;
{
    boolean b;

    b = (boolean) (
	(
	    t1->class == OPENARRAY and t1->symvalue.ndims == 1 and
	    t2->class == ARRAY and
	    compatible(rtype(t2->chain)->type, t_int) and
	    compatible(t1->type, t2->type)
	) or (
	    t2->class == OPENARRAY and t2->symvalue.ndims == 1 and
	    t1->class == ARRAY and
	    compatible(rtype(t1->chain)->type, t_int) and
	    compatible(t1->type, t2->type)
	)
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

public boolean modula2_typematch (type1, type2)
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
	    builtinmatch(t1, t2) or
	    nilMatch(t1, t2) or enumMatch(t1, t2) or
	    openArrayMatch(t1, t2) or stringArrayMatch(t1, t2)
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

public modula2_printdecl (s)
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
		printf("enumeration constant with value ");
		eval(s->symvalue.constval);
		modula2_printval(s);
	    } else {
		printf("const %s = ", symname(s));
		eval(s->symvalue.constval);
		modula2_printval(s);
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
	case OPENARRAY:
	case DYNARRAY:
	case SUBARRAY:
	case RECORD:
	case VARNT:
	case PTR:
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
	    printf("procedure %s", symname(s));
	    listparams(s);
	    printf(" : ");
	    printtype(s, s->type, 0);
	    break;

	case MODULE:
	    printf("module %s", symname(s));
	    break;

	default:
	    printf("[%s]", classname(s));
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
    Symbol tmp;
    int i;

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

	case OPENARRAY:
	    printf("array of ");
	    for (i = 1; i < t->symvalue.ndims; i++) {
		printf("array of ");
	    }
	    printtype(t, t->type, n);
	    break;

	case DYNARRAY:
	    printf("dynarray of ");
	    for (i = 1; i < t->symvalue.ndims; i++) {
		printf("array of ");
	    }
	    printtype(t, t->type, n);
	    break;

	case SUBARRAY:
	    printf("subarray of ");
	    for (i = 1; i < t->symvalue.ndims; i++) {
		printf("array of ");
	    }
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
	    printf("pointer to ");
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

	case TYPEREF:
	    break;

	case FPROC:
	case FFUNC:
	    printf("procedure");
	    break;

	default:
	    printf("[%s]", classname(t));
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
    if (ischar(t)) {
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

private listparams (s)
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
		case FFUNC:
		    printf("procedure ");
		    break;

		case VAR:
		    break;

		default:
		    panic("unexpected class %d for parameter", t->class);
	    }
	    printf("%s", symname(t));
	    if (s->class == PROG) {
		printf(", ");
	    } else {
		printf(" : ");
		printtype(t, t->type, 0);
		if (t->chain != nil) {
		    printf("; ");
		}
	    }
	}
	putchar(')');
    }
}

/*
 * Test if a pointer type should be treated as a null-terminated string.
 * The type given is the type that is pointed to.
 */

private boolean isCstring (type)
Symbol type;
{
    boolean b;
    register Symbol a, t;

    a = rtype(type);
    if (a->class == ARRAY) {
	t = rtype(a->chain);
	b = (boolean) (
	    t->class == RANGE and istypename(a->type, "char") and
	    (t->symvalue.rangev.upper - t->symvalue.rangev.lower + 1) <= 0
	);
    } else {
	b = false;
    }
    return b;
}

/*
 * Modula 2 interface to printval.
 */

public modula2_printval (s)
Symbol s;
{
    prval(s, size(s));
}

/*
 * Print out the value on the top of the expression stack
 * in the format for the type of the given symbol, assuming
 * the size of the object is n bytes.
 */

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
	    if (isbitfield(s)) {
		i = extractField(s);
		t = rtype(s->type);
		if (t->class == SCAL) {
		    printEnum(i, t);
		} else {
		    printRangeVal(i, t);
		}
	    } else {
		prval(s->type, n);
	    }
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if (ischar(t)) {
		len = size(s);
		sp -= len;
		printf("\"%.*s\"", len, sp);
		break;
	    } else {
		printarray(s);
	    }
	    break;

	case OPENARRAY:
	case DYNARRAY:
	    printDynarray(s);
	    break;

	case SUBARRAY:
	    printSubarray(s);
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

	/*
	 * Unresolved opaque type.
	 * Probably a pointer.
	 */
	case TYPEREF:
	    a = pop(Address);
	    printf("@%x", a);
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
	    } else if (isCstring(s->type)) {
		printString(a, true);
	    } else {
		printf("0x%x", a);
	    }
	    break;

	case SCAL:
	    i = 0;
	    popn(n, &i);
	    printEnum(i, s);
	    break;

	case FPROC:
	case FFUNC:
	    a = pop(long);
	    t = whatblock(a);
	    if (t == nil) {
		printf("0x%x", a);
	    } else {
		printname(stdout, t);
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
 * Print out a dynamic array.
 */

private Address printDynSlice();

private printDynarray (t)
Symbol t;
{
    Address base;
    integer n;
    Stack *savesp, *newsp;
    Symbol eltype;

    savesp = sp;
    sp -= (t->symvalue.ndims * sizeof(Word));
    base = pop(Address);
    newsp = sp;
    sp = savesp;
    eltype = rtype(t->type);
    if (t->symvalue.ndims == 0) {
	if (ischar(eltype)) {
	    printString(base, true);
	} else {
	    printf("[dynarray @nocount]");
	}
    } else {
	n = ((long *) sp)[-(t->symvalue.ndims)];
	base = printDynSlice(base, n, t->symvalue.ndims, eltype, size(eltype));
    }
    sp = newsp;
}

/*
 * Print out one dimension of a multi-dimension dynamic array.
 *
 * Return the address of the element that follows the printed elements.
 */

private Address printDynSlice (base, count, ndims, eltype, elsize)
Address base;
integer count, ndims;
Symbol eltype;
integer elsize;
{
    Address b;
    integer i, n;
    char *slice;
    Stack *savesp;

    b = base;
    if (ndims > 1) {
	n = ((long *) sp)[-ndims + 1];
    }
    if (ndims == 1 and ischar(eltype)) {
	slice = newarr(char, count);
	dread(slice, b, count);
	printf("\"%.*s\"", count, slice);
	dispose(slice);
	b += count;
    } else {
	printf("(");
	for (i = 0; i < count; i++) {
	    if (i != 0) {
		printf(", ");
	    }
	    if (ndims == 1) {
		slice = newarr(char, elsize);
		dread(slice, b, elsize);
		savesp = sp;
		sp = slice + elsize;
		printval(eltype);
		sp = savesp;
		dispose(slice);
		b += elsize;
	    } else {
		b = printDynSlice(b, n, ndims - 1, eltype, elsize);
	    }
	}
	printf(")");
    }
    return b;
}

private printSubarray (t)
Symbol t;
{
    printf("[subarray]");
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
    printf("{");
    sp -= nbytes;
    if (t->class == SCAL) {
	printSetOfEnum(t);
    } else if (t->class == RANGE) {
	printSetOfRange(t);
    } else {
	panic("expected range or enumerated base type for set");
    }
    printf("}");
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
 * Construct a node for subscripting a dynamic or subarray.
 * The list of indices is left for processing in evalaref,
 * unlike normal subscripting in which the list is expanded
 * across individual INDEX nodes.
 */

private Node dynref (a, t, slist)
Node a;
Symbol t;
Node slist;
{
    Node p, r;
    integer n;

    p = slist;
    n = 0;
    while (p != nil) {
	if (not compatible(p->value.arg[0]->nodetype, t_int)) {
	    suberror("subscript \"", p->value.arg[0], "\" is the wrong type");
	}
	++n;
	p = p->value.arg[1];
    }
    if (n > t->symvalue.ndims and (t->symvalue.ndims != 0 or n != 1)) {
	suberror("too many subscripts for ", a, nil);
    } else if (n < t->symvalue.ndims) {
	suberror("not enough subscripts for ", a, nil);
    }
    r = build(O_INDEX, a, slist);
    r->nodetype = rtype(t->type);
    return r;
}

/*
 * Construct a node for subscripting.
 */

public Node modula2_buildaref (a, slist)
Node a, slist;
{
    register Symbol t;
    register Node p;
    Symbol eltype;
    Node esub, r;
    integer n;

    t = rtype(a->nodetype);
    switch (t->class) {
	case OPENARRAY:
	case DYNARRAY:
	case SUBARRAY:
	    r = dynref(a, t, slist);
	    break;

	case ARRAY:
	    r = a;
	    eltype = rtype(t->type);
	    p = slist;
	    t = t->chain;
	    while (p != nil and t != nil) {
		esub = p->value.arg[0];
		if (not compatible(rtype(t), rtype(esub->nodetype))) {
		    suberror("subscript \"", esub, "\" is the wrong type");
		}
		r = build(O_INDEX, r, esub);
		r->nodetype = eltype;
		p = p->value.arg[1];
		t = t->chain;
	    }
	    if (p != nil) {
		suberror("too many subscripts for ", a, nil);
	    } else if (t != nil) {
		suberror("not enough subscripts for ", a, nil);
	    }
	    break;

	default:
	    suberror("\"", a, "\" is not an array");
	    break;
    }
    return r;
}

/*
 * Subscript usage error reporting.
 */

private suberror (s1, e1, s2)
String s1, s2;
Node e1;
{
    beginerrmsg();
    if (s1 != nil) {
	fprintf(stderr, s1);
    }
    if (e1 != nil) {
	prtree(stderr, e1);
    }
    if (s2 != nil) {
	fprintf(stderr, s2);
    }
    enderrmsg();
}

/*
 * Check that a subscript value is in the appropriate range.
 */

private subchk (value, lower, upper)
long value, lower, upper;
{
    if (value < lower or value > upper) {
	error("subscript value %d out of range [%d..%d]", value, lower, upper);
    }
}

/*
 * Compute the offset for subscripting a dynamic array.
 */

private getdynoff (ndims, sub)
integer ndims;
long *sub;
{
    long k, off, *count;

    count = (long *) sp;
    off = 0;
    for (k = 0; k < ndims - 1; k++) {
	subchk(sub[k], 0, count[k] - 1);
	off += (sub[k] * count[k+1]);
    }
    subchk(sub[ndims - 1], 0, count[ndims - 1] - 1);
    return off + sub[ndims - 1];
}

/*
 * Compute the offset associated with a subarray.
 */

private getsuboff (ndims, sub)
integer ndims;
long *sub;
{
    long k, off;
    struct subarrayinfo {
	long count;
	long mult;
    } *info;

    info = (struct subarrayinfo *) sp;
    off = 0;
    for (k = 0; k < ndims; k++) {
	subchk(sub[k], 0, info[k].count - 1);
	off += sub[k] * info[k].mult;
    }
    return off;
}

/*
 * Evaluate a subscript index.
 */

public modula2_evalaref (s, base, i)
Symbol s;
Address base;
long i;
{
    Symbol t;
    long lb, ub, off;
    long *sub;
    Address b;

    t = rtype(s);
    if (t->class == ARRAY) {
	findbounds(rtype(t->chain), &lb, &ub);
	if (i < lb or i > ub) {
	    error("subscript %d out of range [%d..%d]", i, lb, ub);
	}
	push(long, base + (i - lb) * size(t->type));
    } else if ((t->class == OPENARRAY or t->class == DYNARRAY) and
	t->symvalue.ndims == 0
    ) {
	push(long, base + i * size(t->type));
    } else if (t->class == OPENARRAY or t->class == DYNARRAY or
	t->class == SUBARRAY
    ) {
	push(long, i);
	sub = (long *) (sp - (t->symvalue.ndims * sizeof(long)));
	rpush(base, size(t));
	sp -= (t->symvalue.ndims * sizeof(long));
	b = pop(Address);
	sp += sizeof(Address);
	if (t->class == SUBARRAY) {
	    off = getsuboff(t->symvalue.ndims, sub);
	} else {
	    off = getdynoff(t->symvalue.ndims, sub);
	}
	sp = (Stack *) sub;
	push(long, b + off * size(t->type));
    } else {
	error("[internal error: expected array in evalaref]");
    }
}

/*
 * Initial Modula-2 type information.
 */

#define NTYPES 12

private Symbol inittype[NTYPES + 1];

private addType (n, s, lower, upper)
integer n;
String s;
long lower, upper;
{
    register Symbol t;

    if (n > NTYPES) {
	panic("initial Modula-2 type number too large for '%s'", s);
    }
    t = insert(identname(s, true));
    t->language = mod2;
    t->class = TYPE;
    t->type = newSymbol(nil, 0, RANGE, t, nil);
    t->type->symvalue.rangev.lower = lower;
    t->type->symvalue.rangev.upper = upper;
    t->type->language = mod2;
    inittype[n] = t;
}

private initModTypes ()
{
    addType(1, "integer", 0x80000000L, 0x7fffffffL);
    addType(2, "char", 0L, 255L);
    addType(3, "boolean", 0L, 1L);
    addType(4, "unsigned", 0L, 0xffffffffL);
    addType(5, "real", 4L, 0L);
    addType(6, "longreal", 8L, 0L);
    addType(7, "word", 0L, 0xffffffffL);
    addType(8, "byte", 0L, 255L);
    addType(9, "address", 0L, 0xffffffffL);
    addType(10, "file", 0L, 0xffffffffL);
    addType(11, "process", 0L, 0xffffffffL);
    addType(12, "cardinal", 0L, 0x7fffffffL);
}

/*
 * Initialize typetable.
 */

public modula2_modinit (typetable)
Symbol typetable[];
{
    register integer i;

    if (not initialized) {
	initModTypes();
	initialized = true;
    }
    for (i = 1; i <= NTYPES; i++) {
	typetable[i] = inittype[i];
    }
}

public boolean modula2_hasmodules ()
{
    return true;
}

public boolean modula2_passaddr (param, exprtype)
Symbol param, exprtype;
{
    return false;
}
