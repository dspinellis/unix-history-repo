/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)asm.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * Assembly language dependent symbol routines.
 */

#include "defs.h"
#include "symbols.h"
#include "asm.h"
#include "languages.h"
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

/*
 * Initialize assembly language information.
 */

public asm_init()
{
    Language lang;

    lang = language_define("assembler", ".s");
    language_setop(lang, L_PRINTDECL, asm_printdecl);
    language_setop(lang, L_PRINTVAL, asm_printval);
    language_setop(lang, L_TYPEMATCH, asm_typematch);
    language_setop(lang, L_BUILDAREF, asm_buildaref);
    language_setop(lang, L_EVALAREF, asm_evalaref);
    language_setop(lang, L_MODINIT, asm_modinit);
    language_setop(lang, L_HASMODULES, asm_hasmodules);
    language_setop(lang, L_PASSADDR, asm_passaddr);
}

/*
 * Test if two types are compatible.
 */

public Boolean asm_typematch(type1, type2)
Symbol type1, type2;
{
    Boolean b;

    b = false;
    return b;
}

public asm_printdecl(s)
Symbol s;
{
    switch (s->class) {
	case CONST:
	    printf("%s = %d", symname(s), s->symvalue.constval->value.lcon);
	    break;

	case VAR:
	case REF:
	    printf("&%s = 0x%x", symname(s), s->symvalue.offset);
	    break;

	case PROC:
	case FUNC:
	    printf("%s (0x%x):", symname(s), codeloc(s));
	    break;

	case TYPE:
	    printf("%s", symname(s));
	    break;

	case ARRAY:
	    printf("$string");
	    break;

	default:
	    printf("[%s]", classname(s));
	    break;
    }
    putchar('\n');
}

/*
 * Print out the value on the top of the expression stack
 * in the format for the type of the given symbol.
 */

public asm_printval(s)
register Symbol s;
{
    register Symbol t;
    register Integer len;

    switch (s->class) {
	case ARRAY:
	    t = rtype(s->type);
	    if (t->class == RANGE and istypename(t->type, "$char")) {
		len = size(s);
		sp -= len;
		printf("\"%.*s\"", len, sp);
	    } else {
		printarray(s);
	    }
	    break;

	default:
	    printf("0x%x", pop(Integer));
	    break;
    }
}

/*
 * Treat subscripting as indirection through pointer to integer.
 */

public Node asm_buildaref(a, slist)
Node a, slist;
{
    Symbol t, eltype;
    Node p, r;

    t = rtype(a->nodetype);
    eltype = t->type;
    p = slist->value.arg[0];
    r = build(O_MUL, p, build(O_LCON, (long) size(eltype)));
    r = build(O_ADD, build(O_RVAL, a), r);
    r->nodetype = eltype;
    return r;
}

/*
 * Evaluate a subscript index.  Assumes dimension is [0..n].
 */

public asm_evalaref(s, base, i)
Symbol s;
Address base;
long i;
{
    Symbol t;

    t = rtype(s);
    push(long, base + i * size(t->type));
}

public asm_modinit (typetable)
Symbol typetable[];
{
    /* nothing for right now */
}

public boolean asm_hasmodules ()
{
    return false;
}

public boolean asm_passaddr (param, exprtype)
Symbol param, exprtype;
{
    return false;
}
