/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)names.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

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

/*
 * The hash table is a power of two, in order to make hashing faster.
 * Using a non-prime is ok since we use chaining instead of re-hashing.
 */

#define HASHTABLESIZE 8192

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

/*
 * Given an identifier, convert it to a name.
 * If it's not in the hash table, then put it there.
 *
 * The second argument specifies whether the string should be copied
 * into newly allocated space if not found.
 *
 * This routine is time critical when starting up the debugger
 * on large programs.
 */

public Name identname(s, isallocated)
String s;
Boolean isallocated;
{
    register unsigned h;
    register char *p, *q;
    register Name n, *np;
    Namepool newpool;

    h = 0;
    for (p = s; *p != '\0'; p++) {
	h = (h << 1) ^ (*p);
    }
    h &= (HASHTABLESIZE-1);
    np = &nametable[h];
    n = *np;
    while (n != nil) {
	p = s;
	q = n->identifier;
	while (*p == *q) {
	    if (*p == '\0') {
		return n;
	    }
	    ++p;
	    ++q;
	}
	n = n->chain;
    }

    /*
     * Now we know that name hasn't been found,
     * so we allocate a name, store the identifier, and
     * enter it in the hash table.
     */
    if (nleft <= 0) {
	newpool = new(Namepool);
	newpool->prevpool = namepool;
	namepool = newpool;
	nleft = CHUNKSIZE;
    }
    --nleft;
    n = &(namepool->name[nleft]);
    if (isallocated) {
	n->identifier = s;
    } else {
	/* this case doesn't happen very often */
	n->identifier = newarr(char, strlen(s) + 1);
	p = s;
	q = n->identifier;
	while (*p != '\0') {
	    *q++ = *p++;
	}
	*q = '\0';
    }
    n->chain = *np;
    *np = n;
    return n;
}

/*
 * Deallocate the name table.
 */

public names_free()
{
    Namepool n, m;
    register integer i;

    n = namepool;
    while (n != nil) {
	m = n->prevpool;
	dispose(n);
	n = m;
    }
    for (i = 0; i < HASHTABLESIZE; i++) {
	nametable[i] = nil;
    }
    namepool = nil;
    nleft = 0;
}
