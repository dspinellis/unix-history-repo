/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)printval.c 1.5 %G%";

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
    switch(s->class) {
	case ARRAY:
	    t = rtype(s->type);
	    if (t==t_char || (t->class==RANGE && t->type==t_char)) {
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
	    if (s == t_real) {
		printreal(pop(double));
	    } else if (s == t_char) {
		printf("'%c'", pop(char));
	    } else if (s == t_boolean) {
		printf(popsmall(s) == TRUE ? "true" : "false");
	    } else {
		printf("%ld", popsmall(s));
	    }
	    break;

	case FILET:
	case PTR: {
	    ADDRESS addr;

	    addr = pop(ADDRESS);
	    if (addr == 0) {
		printf("nil");
	    } else {
		printf("0%o", addr);
	    }
	    break;
	}

	case FIELD:
	    error("missing record specification");
	    break;

	case SCAL: {
	    int scalar;
	    BOOLEAN found;

	    scalar = popsmall(s);
	    found = FALSE;
	    for (t = s->chain; t != NIL; t = t->chain) {
		if (t->symvalue.iconval == scalar) {
		    printf("%s", t->symbol);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		printf("(scalar = %d)", scalar);
	    }
	    break;
	}

	case FPROC:
	case FFUNC:
	{
	    ADDRESS a;

	    a = fparamaddr(pop(long));
	    t = whatblock(a);
	    if (t == NIL) {
		printf("(proc %d)", a);
	    } else {
		printf("%s", t->symbol);
	    }
	    break;
	}

	default:
	    if (s->class < BADUSE || s->class > VARNT) {
		panic("printval: bad class %d", s->class);
	    }
	    error("don't know how to print a %s", classname(s));
	    /* NOTREACHED */
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

LOCAL printarray(a)
SYM *a;
{
    STACK *savesp, *newsp;
    SYM *eltype;
    long elsize;

    savesp = sp;
    sp -= size(a);
    newsp = sp;
    eltype = a->type;
    elsize = size(eltype);
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

/*
 * Print out the value of a real number.
 * Pascal notation is somewhat different that what one gets
 * from "%g" in printf.
 */

LOCAL printreal(r)
double r;
{
    extern char *index();
    char *p, buf[256];

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
