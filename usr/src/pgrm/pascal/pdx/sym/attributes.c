/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)attributes.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * Functions to return the attributes of a symbol.
 */

#include "defs.h"
#include "sym.h"
#include "process.h"
#include "btypes.h"
#include "classes.h"
#include "sym.rep"

char *name(s)
SYM *s;
{
	return s->symbol;
}

int toknum(s)
SYM *s;
{
	return s->symvalue.token.toknum;
}

int tokval(s)
SYM *s;
{
	return s->symvalue.token.tokval;
}

ADDRESS codeloc(f)
SYM *f;
{
	if (f == NIL) {
		panic("codeloc: nil symbol");
	}
	if (!isblock(f)) {
		panic("codeloc: %s is not a block", f->symbol);
	}
	return f->symvalue.funcv.codeloc;
}

/*
 * Rtype returns the "reduced type" given a variable.
 * The idea is to remove type names so we can check the class.
 */

SYM *rtype(t)
register SYM *t;
{
	while (t->class == TYPE) {
		t = t->type;
	}
	return t;
}

/*
 * Return the SYM that contains the given SYM.
 */

SYM *container(s)
SYM *s;
{
	return s->func;
}

/*
 * Return a pointer to the string for the name of the class that
 * the given symbol belongs to.
 */

LOCAL char *clname[] = {
	"bad use", "constant", "type", "variable", "array", "fileptr",
	"record", "field", "procedure", "function", "funcvar",
	"ref", "pointer", "file", "set", "range", "label", "withptr",
	"scalar", "string", "program", "improper", "variant",
	"procparam", "funcparam",
};

char *classname(s)
SYM *s;
{
	return clname[s->class];
}

/*
 * size finds the size in bytes of the given type
 */

#define MINCHAR -128
#define MAXCHAR 127
#define MINSHORT -32768
#define MAXSHORT 32767

int size(t)
register SYM *t;
{
	long lower, upper;

	t = rtype(t);
	if (t == t_real) {
		return sizeof(double);
	}
	switch(t->class) {
		case RANGE:
			lower = t->symvalue.rangev.lower;
			upper = t->symvalue.rangev.upper;
			if (lower >= MINCHAR && upper <= MAXCHAR) {
				return sizeof(char);
			} else if (lower >= MINSHORT && upper <= MAXSHORT) {
				return sizeof(short);
			} else {
				return sizeof(long);
			}
			/* NOTREACHED */

		case ARRAY: {
			register int nel, elsize;
			register SYM *s;

			elsize = size(t->type);
			nel = 1;
			for (t = t->chain; t != NIL; t = t->chain) {
				s = rtype(t);
				lower = s->symvalue.rangev.lower;
				upper = s->symvalue.rangev.upper;
				nel *= (upper-lower+1);
			}
			return nel*elsize;
		}

		case CONST:
		case VAR:
		case FVAR:
		case FIELD:
			return size(t->type);

		case RECORD:
			return t->symvalue.offset;

		case PTR:
		case REF:
		case FILET:
			return sizeof(int);

		case SCAL:
			if (t->symvalue.iconval > 255) {
				return sizeof(short);
			} else {
				return sizeof(char);
			}
			/* NOTREACHED */

		case FPROC:
		case FFUNC:
			return sizeof(ADDRESS *);

		default:
			if (t->class < BADUSE || t->class > FFUNC) {
				panic("size: bad class (%d)", t->class);
			} else {
				error("improper operation on a %s", classname(t));
			}
			/* NOTREACHED */
	}
}
