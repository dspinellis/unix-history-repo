/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)@(#)names.c 1.1 %G%";

/*
 * Name are the internal representation for identifiers.
 *
 * A hash table is used to map identifiers to names.
 */

#include "defs.h"
#include "names.h"

#ifndef public
typedef struct Name *Name;

/*
 * Inline (for speed) function to return the identifier (string)
 * associated with a name.  Because C cannot support both inlines
 * and data hiding at the same time, the Name structure must be
 * publicly visible.  It is not used explicitly, however, outside of this file.
 */

struct Name {
    char *identifier;
    Name chain;
};

#define ident(n) ((n == nil) ? "(noname)" : n->identifier)
#endif

#define HASHTABLESIZE 2997

private Name nametable[HASHTABLESIZE];

/*
 * Names are allocated in large chunks to avoid calls to malloc
 * and to cluster names in memory so that tracing hash chains
 * doesn't cause many a page fault.
 */

#define CHUNKSIZE 1000

typedef struct Namepool {
    struct Name name[CHUNKSIZE];
    struct Namepool *prevpool;
} *Namepool;

private Namepool namepool = nil;
private Integer nleft = 0;
private struct Namepool zeropool;

/*
 * Given an identifier, convert it to a name.
 * If it's not in the hash table, then put it there.
 *
 * The second argument specifies whether the string should be copied
 * into newly allocated space if not found.
 *
 * Pardon my use of goto's, but it seemed more efficient and less convoluted
 * than adding a collection of boolean variables.  This routine is time
 * critical when starting up the debugger on large programs.
 */

public Name identname(s, isallocated)
String s;
Boolean isallocated;
{
    register unsigned h;
    register Char *p, *q;
    register Name n;
    register Integer len;
    Namepool newpool;

    h = 0;
    for (p = s; *p != '\0'; p++) {
	h = (h << 1) ^ (*p);
    }
    h = h mod HASHTABLESIZE;
    len = p - s;
    n = nametable[h];
    while (n != nil) {
	p = s;
	q = n->identifier;
	for (;;) {
	    if (*p != *q) {
		goto nomatch;
	    } else if (*p == '\0') {
		goto match;
	    }
	    ++p;
	    ++q;
	}
    nomatch:
	n = n->chain;
    }

    /*
     * Now we know that name hasn't been found (otherwise we'd have jumped
     * down to match), so we allocate a name, store the identifier, and
     * enter it in the hash table.
     */
    if (nleft <= 0) {
	newpool = new(Namepool);
	*newpool = zeropool;
	newpool->prevpool = namepool;
	namepool = newpool;
	nleft = CHUNKSIZE;
    }
    --nleft;
    n = &(namepool->name[nleft]);
    if (isallocated) {
	n->identifier = s;
    } else {
	n->identifier = newarr(char, len + 1);
	p = s;
	q = n->identifier;
	while (*p != '\0') {
	    *q++ = *p++;
	}
	*q = '\0';
    }
    n->chain = nametable[h];
    nametable[h] = n;

    /*
     * The two possibilities (name known versus unknown) rejoin.
     */
match:
    return n;
}

/*
 * Return the identifier associated with a name.
 *
 * Currently compiled inline.
 *
 *
 * public String ident(n)
Name n;
{
    return (n == nil) ? "(noname)" : n->identifier;
}
 *
 */

/*
 * Deallocate the name table.
 */

public names_free()
{
    register int i;
    register Name n, next;

    for (i = 0; i < HASHTABLESIZE; i++) {
	n = nametable[i];
	while (n != nil) {
	    next = n->chain;
	    dispose(n);
	    n = next;
	}
	nametable[i] = nil;
    }
    namepool = nil;
    nleft = 0;
}
