/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)c.c 1.2 %G%";

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

#define isdouble(range) ( \
    range->symvalue.rangev.upper == 0 and range->symvalue.rangev.lower > 0 \
)

#define isrange(t, name) (t->class == RANGE and istypename(t->type, name))

/*
 * Initialize C language information.
 */

public c_init()
{
    Language lang;

    lang = language_define("c", ".c");
    language_setop(lang, L_PRINTDECL, c_printdecl);
    language_setop(lang, L_PRINTVAL, c_printval);
    language_setop(lang, L_TYPEMATCH, c_typematch);
}

/*
 * Test if two types are compatible.
 *
 * Integers and reals are not compatible since they cannot always be mixed.
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
	if (t1->type == t_int or t1->type == t_char) {
	    tmp = t1;
	    t1 = t2;
	    t2 = tmp;
	}
	b = (Boolean) (
	    (
		isrange(t1, "int") and
		(t2->type == t_int or t2->type == t_char)
	    ) or (
		isrange(t1, "char") and
		(t2->type == t_char or t2->type == t_int)
	    ) or (
		t1->type == t2->type and (
		    (t1->class == t2->class) or
		    (t1->class == SCAL and t2->class == CONST) or
		    (t1->class == CONST and t2->class == SCAL)
		)
	    )
	);
    }
    return b;
}

/*
 * Decide if a field is a bit field.
 */

private Boolean isbitfield(s)
register Symbol s;
{
    Boolean b;
    register Integer off, len;
    register Symbol t;

    off = s->symvalue.field.offset;
    len = s->symvalue.field.length;
    if ((off mod BITSPERBYTE) != 0 or (len mod BITSPERBYTE) != 0) {
	b = true;
    } else {
	t = rtype(s->type);
	b = (Boolean)
	    (t->class == SCAL and len != (sizeof(int)*BITSPERBYTE) or
	    len != (size(t)*BITSPERBYTE)
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
		printf("(enumeration constant, ord %ld)",
		    s->symvalue.iconval);
	    } else {
		printf("const %s = ", symname(s));
		printval(s);
	    }
	    break;

	case TYPE:
	case VAR:
	    if (s->class != TYPE) {
		if (s->level == 2) {
		    printf("static ");
		} else if (s->level < 0) {
		    printf("register ");
		}
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
	    semicolon = false;
	    printtype(s, s, indent);
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
	    error("class %s in c_printdecl", classname(s));
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
	    printf("{\n", t->class == RECORD ? "struct" : "union");
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
    register int i, len;

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
		len = s->symvalue.field.length;
		if (len <= BITSPERBYTE) {
		    i = pop(char);
		} else if (len <= sizeof(short)*BITSPERBYTE) {
		    i = pop(short);
		} else {
		    i = pop(long);
		}
		i >>= (s->symvalue.field.offset mod BITSPERBYTE);
		i &= ((1 << len) - 1);
		t = rtype(s->type);
		if (t->class == SCAL) {
		    printenum(i, t);
		} else {
		    printrange(i, t);
		}
	    } else {
		c_printval(s->type);
	    }
	    break;

	case ARRAY:
	    t = rtype(s->type);
	    if (t->class == RANGE and istypename(t->type, "char")) {
		len = size(s);
		sp -= len;
		printf("\"%.*s\"", len, sp);
	    } else {
		printarray(s);
	    }
	    break;

	case RECORD:
	case VARNT:
	    c_printstruct(s);
	    break;

	case RANGE:
	    if (istypename(s->type, "boolean")) {
		printrange(popsmall(s), s);
	    } else if (istypename(s->type, "char")) {
		printrange(pop(char), s);
	    } else if (isdouble(s)) {
		switch (s->symvalue.rangev.lower) {
		    case sizeof(float):
			prtreal(pop(float));
			break;

		    case sizeof(double):
			prtreal(pop(double));
			break;

		    default:
			panic("bad real size %d", t->symvalue.rangev.lower);
			break;
		}
	    } else {
		printrange(popsmall(s), s);
	    }
	    break;

	case PTR:
	    t = rtype(s->type);
	    a = pop(Address);
	    if (a == 0) {
		printf("(nil)");
	    } else if (t->class == RANGE and istypename(t->type, "char")) {
		printstring(a);
	    } else {
		printf("0x%x", a);
	    }
	    break;

	case SCAL:
	    i = pop(Integer);
	    printenum(i, s);
	    break;

	default:
	    if (ord(s->class) > ord(TYPEREF)) {
		panic("printval: bad class %d", ord(s->class));
	    }
	    error("don't know how to print a %s", c_classname(s));
	    /* NOTREACHED */
    }
}

/*
 * Print out a C structure.
 */

private c_printstruct(s)
Symbol s;
{
    register Symbol f;
    register Stack *savesp;
    register Integer n, off, len;

    sp -= size(s);
    savesp = sp;
    printf("(");
    f = s->chain;
    for (;;) {
	off = f->symvalue.field.offset;
	len = f->symvalue.field.length;
	n = (off + len + 7) div BITSPERBYTE;
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
 * Print out a range type (integer, char, or boolean).
 */

private printrange(i, t)
Integer i;
register Symbol t;
{
    if (istypename(t->type, "boolean")) {
	printf(((Boolean) i) == true ? "true" : "false");
    } else if (istypename(t->type, "char")) {
	putchar('\'');
	printchar(i);
	putchar('\'');
    } else if (t->symvalue.rangev.lower >= 0) {
	printf("%lu", i);
    } else {
	printf("%ld", i);
    }
}

/*
 * Print out a null-terminated string (pointer to char)
 * starting at the given address.
 */

private printstring(addr)
Address addr;
{
    register Address a;
    register Integer i, len;
    register Boolean endofstring;
    union {
	char ch[sizeof(Word)];
	int word;
    } u;

    putchar('"');
    a = addr;
    endofstring = false;
    while (not endofstring) {
	dread(&u, a, sizeof(u));
	i = 0;
	do {
	    if (u.ch[i] == '\0') {
		endofstring = true;
	    } else {
		printchar(u.ch[i]);
	    }
	    ++i;
	} while (i < sizeof(Word) and not endofstring);
	a += sizeof(Word);
    }
    putchar('"');
}

/*
 * Print out an enumerated value by finding the corresponding
 * name in the enumeration list.
 */

private printenum(i, t)
Integer i;
Symbol t;
{
    register Symbol e;

    e = t->chain;
    while (e != nil and e->symvalue.iconval != i) {
	e = e->chain;
    }
    if (e != nil) {
	printf("%s", symname(e));
    } else {
	printf("%d", i);
    }
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
