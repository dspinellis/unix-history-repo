/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)symtab.c	5.2 (Berkeley) 4/7/87";
#endif not lint

/*
 * Symbol table implementation.
 */

#include "defs.h"
#include "symtab.h"
#include "sym.h"
#include "sym/classes.h"
#include "sym/sym.rep"

/*
 * The symbol table structure is currently assumes no deletions.
 */

#define MAXHASHSIZE 1009    /* largest allowable hash table */

struct symtab {
    int size;
    int hshsize;
    SYM **symhsh;
    SYM *symarray;
    int symindex;
};

/*
 * Macro to hash a string.
 *
 * The hash value is returned through the "h" parameter which should
 * an unsigned integer.  The other parameters are the symbol table, "st",
 * and a pointer to the string to be hashed, "name".
 */

#define hash(h, st, name) \
{ \
    register char *cp; \
\
    h = 0; \
    for (cp = name; *cp != '\0'; cp++) { \
	h = (h << 1) | (*cp); \
    } \
    h %= st->hshsize; \
}

/*
 * To create a symbol table, we allocate space for the symbols and
 * for a hash table that's twice as big (+1 to make it odd).
 */

SYMTAB *st_creat(size)
int size;
{
    register SYMTAB *st;
    register int i;

    st = alloc(1, SYMTAB);
    st->size = size;
    st->hshsize = 2*size + 1;
    if (st->hshsize > MAXHASHSIZE) {
	st->hshsize = MAXHASHSIZE;
    }
    st->symhsh = alloc(st->hshsize, SYM *);
    st->symarray = alloc(st->size, SYM);
    st->symindex = 0;
    for (i = 0; i < st->hshsize; i++) {
	st->symhsh[i] = NIL;
    }
    return(st);
}

st_destroy(st)
SYMTAB *st;
{
    dispose(st->symhsh);
    dispose(st->symarray);
    dispose(st);
}

/*
 * insert a symbol into a table
 */

SYM *st_insert(st, name)
register SYMTAB *st;
char *name;
{
    register SYM *s;
    register unsigned int h;
    static SYM zerosym;

    if (st == NIL) {
	panic("tried to insert into NIL table");
    }
    if (st->symindex >= st->size) {
	panic("too many symbols");
    }
    hash(h, st, name);
    s = &(st->symarray[st->symindex++]);
    *s = zerosym;
    s->symbol = name;
    s->next_sym = st->symhsh[h];
    st->symhsh[h] = s;
    return(s);
}

/*
 * symbol lookup
 */

SYM *st_lookup(st, name)
SYMTAB *st;
char *name;
{
    register SYM *s;
    register unsigned int h;

    if (st == NIL) {
	panic("tried to lookup in NIL table");
    }
    hash(h, st, name);
    for (s = st->symhsh[h]; s != NIL; s = s->next_sym) {
	if (strcmp(s->symbol, name) == 0) {
	    break;
	}
    }
    return(s);
}

/*
 * Dump out all the variables associated with the given
 * procedure, function, or program at the given recursive level.
 *
 * This is quite inefficient.  We traverse the entire symbol table
 * each time we're called.  The assumption is that this routine
 * won't be called frequently enough to merit improved performance.
 */

dumpvars(f, frame)
SYM *f;
FRAME *frame;
{
    register SYM *s;
    SYM *first, *last;

    first = symtab->symarray;
    last = first + symtab->symindex - 1;
    for (s = first; s <= last; s++) {
	if (should_print(s, f)) {
	    printv(s, frame);
	    putchar('\n');
	}
    }
}

/*
 * Create an alias for a command.
 *
 * We put it into the given table with block 1, which is how it
 * is distinguished for printing purposes.
 */

enter_alias(table, new, old)
SYMTAB *table;
char *new, *old;
{
    SYM *s, *t;

    if ((s = st_lookup(table, old)) == NIL) {
	error("%s is not a known command", old);
    }
    if (st_lookup(table, new) != NIL) {
	error("cannot alias command names");
    }
    make_keyword(table, new, s->symvalue.token.toknum);
    t = st_insert(table, new);
    t->blkno = 1;
    t->symvalue.token.toknum = s->symvalue.token.toknum;
    t->type = s;
}

/*
 * Print out the currently active aliases.
 * The kludge is that the type pointer for an alias points to the
 * symbol it is aliased to.
 */

print_alias(table, name)
SYMTAB *table;
char *name;
{
    SYM *s;
    SYM *first, *last;

    if (name != NIL) {
	s = st_lookup(table, name);
	if (s == NIL) {
	    error("\"%s\" is not an alias", name);
	}
	printf("%s\n", s->type->symbol);
    } else {
	first = table->symarray;
	last = first + table->symindex - 1;
	for (s = first; s <= last; s++) {
	    if (s->blkno == 1) {
		printf("%s\t%s\n", s->symbol, s->type->symbol);
	    }
	}
    }
}

/*
 * Find a named type that points to t; return NIL if there is none.
 * This is necessary because of the way pi keeps symbols.
 */

#define NSYMS_BACK 20       /* size of local context to try */

LOCAL SYM *search();

SYM *findtype(t)
SYM *t;
{
    SYM *s;
    SYM *first, *last;
    SYM *lower;

    first = symtab->symarray;
    last = first + symtab->symindex - 1;
    if ((lower = t - NSYMS_BACK) < first) {
	lower = first;
    }
    if ((s = search(t, lower, last)) == NIL) {
	s = search(t, first, last);
    }
    return(s);
}

/*
 * Search the symbol table from first to last, looking for a
 * named type pointing to the given type symbol.
 */

LOCAL SYM *search(t, first, last)
SYM *t;
register SYM *first, *last;
{
    register SYM *s;

    for (s = first; s <= last; s++) {
	if (s->class == TYPE && s->type == t && s->symbol != NIL) {
	    return(s);
	}
    }
    return(NIL);
}
