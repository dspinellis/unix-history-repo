/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printval.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * Print out the value at the top of the stack using the given type.
 */

#include "defs.h"
#include "sym.h"
#include "btypes.h"
#include "classes.h"
#include "tree.h"
#include "process.h"
#include "mappings.h"
#include "sym.rep"

printval(s)
SYM *s;
{
    SYM *t;
    ADDRESS a;
    int len;
    double r;

    if (s->class == REF) {
	s = s->type;
    }
    switch (s->class) {
	case ARRAY:
	    t = rtype(s->type);
	    if (t == t_char || (t->class == RANGE && t->type == t_char)) {
		len = size(s);
		sp -= len;
#ifdef tahoe
		downalignstack();
#endif
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
	    if (s == t_real) {
		prtreal(pop(double));
	    } else {
		printordinal(popsmall(s), rtype(s->type));
	    }
	    break;

	case FILET:
	case PTR:
	    a = pop(ADDRESS);
	    if (a == 0) {
		printf("nil");
	    } else {
		printf("0%o", a);
	    }
	    break;

	case FIELD:
	    error("missing record specification");
	    break;

	case SCAL:
	    printordinal(popsmall(s), s);
	    break;

	case FPROC:
	case FFUNC:
	    a = fparamaddr(pop(long));
	    t = whatblock(a);
	    if (t == NIL) {
		printf("(proc %d)", a);
	    } else {
		printf("%s", t->symbol);
	    }
	    break;

	default:
	    if (s->class < BADUSE || s->class > VARNT) {
		panic("printval: bad class %d", s->class);
	    }
	    error("don't know how to print a %s", classname(s));
	    /* NOTREACHED */
    }
}

/*
 * Print out an ordinal value (either an integer, character, or
 * an enumeration constant).
 */

printordinal(v, t)
long v;
SYM *t;
{
    SYM *c;
    int iv;

    iv = v;
    if (t->class == SCAL) {
	c = t->chain;
	while (c != NIL && c->symvalue.iconval != iv) {
	    c = c->chain;
	}
	if (c == NIL) {
	    printf("(scalar = %d)", iv);
	} else {
	    printf("%s", c->symbol);
	}
    } else if (t == t_char) {
	printf("'%c'", iv);
    } else if (t == t_boolean) {
	printf("%s", (iv == TRUE) ? "true" : "false");
    } else {
	printf("%ld", v);
    }
}

/*
 * Print out the value of a record, field by field.
 */

LOCAL printrecord(s)
SYM *s;
{
    SYM *t;

    if ((t = s->chain) == NIL) {
	error("record has no fields");
    }
    printf("(");
    sp -= size(s);
#ifdef tahoe
    downalignstack();
#endif
    printfield(t);
    printf(")");
}

/*
 * Print out a field, first printing out other fields.
 * This is done because the fields are chained together backwards.
 */

LOCAL printfield(s)
SYM *s;
{
    STACK *savesp;

    if (s->chain != NIL) {
	printfield(s->chain);
	printf(", ");
    }
    printf("%s = ", s->symbol);
    savesp = sp;
    sp += (s->symvalue.offset + size(s->type));
#ifdef tahoe
    alignstack();
#endif
    printval(s->type);
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

#ifdef tahoe
LOCAL printarray(a)
SYM *a;
{
    STACK *savesp, *newsp;
    SYM *eltype;
    long elsize;

    savesp = (STACK *)(((int)sp + 3) & ~3);
    eltype = a->type;
    printf("(");
    elsize = size(eltype);
    if (eltype->class == ARRAY)
	savesp += elsize;
    if (elsize < sizeof(int)) {
	register char *cp = sp - ((size(a) + 3) & ~3);
	int psh;
	register char *cp1, *end = cp + size(a);
	register int savestack;

	while (cp < end) {
	    psh = 0;
	    cp1 = (char *)&psh + sizeof(int) - elsize;
	    while (cp1 < (char *)&psh + sizeof psh)
		*cp1++ = *cp++;
	    if (end - size(a) != cp - elsize) {
		printf(", ");
	    }
	    switch (elsize) {
		case sizeof(char):
		    savestack = *(char *)sp;
		    push(char, psh);
	    	    printval(eltype);
		    *(char *)sp = savestack;
		    break;
		case sizeof(short):
		    savestack = *(short *)sp;
		    push(short, psh);
	    	    printval(eltype);
		    *(short *)sp = savestack;
		    break;
		default:
		   panic("bad size on runtime stack");
	    }
	}
    } else {
	sp -= size(a);
	downalignstack();
	newsp = sp;
	for (sp += elsize, alignstack(); sp <= savesp; sp += 2*elsize) {
	    if (sp - 2*elsize >= newsp) {
		printf(", ");
	    }
	    printval(eltype);
	    if (eltype->class == ARRAY) {
		sp -= elsize;
	    }
	}
	sp = newsp;
    }
    printf(")");
}
#else

LOCAL printarray(a)
SYM *a;
{
    STACK *savesp, *newsp;
    SYM *eltype;
    long elsize;

    savesp = sp;
    eltype = a->type;
    elsize = size(eltype);
    sp -= size(a);
    newsp = sp;
    printf("(");
    for (sp += elsize; sp <= savesp; sp += 2*elsize) {
	if (sp - elsize != newsp) {
	    printf(", ");
	}
	printval(eltype);
    }
    sp = newsp;
    printf(")");
}
#endif tahoe

/*
 * Print out the value of a real number.
 * Pascal notation is somewhat different that what one gets
 * from "%g" in printf.
 */

LOCAL prtreal(r)
double r;
{
    extern char *index();
    char buf[256];

    sprintf(buf, "%g", r);
    if (buf[0] == '.') {
	printf("0%s", buf);
    } else if (buf[0] == '-' && buf[1] == '.') {
	printf("-0%s", &buf[1]);
    } else {
	printf("%s", buf);
    }
    if (index(buf, '.') == NIL) {
	printf(".0");
    }
}
