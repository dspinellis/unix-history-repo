/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)mappings.c 1.4 8/10/83";

/*
 * Source-to-object and vice versa mappings.
 */

#include "defs.h"
#include "mappings.h"
#include "symbols.h"
#include "source.h"
#include "object.h"
#include "machine.h"

#ifndef public
#include "machine.h"
#include "source.h"
#include "symbols.h"

typedef struct {
    Address addr;
    String filename;
    Lineno lineindex;		/* index to first linetab entry */
} Filetab;

typedef struct {
    Lineno line;
    Address addr;
} Linetab;

Filetab *filetab;
Linetab *linetab;

#define NOADDR ((Address) -1)	/* no address for line or procedure */

#endif

/*
 * Get the source file name associated with a given address.
 */

public String srcfilename(addr)
Address addr;
{
    register Address i, j, k;
    Address a;
    Filetab *ftp;
    String s;

    s = nil;
    if (nlhdr.nfiles != 0 and addr >= filetab[0].addr) {
	i = 0;
	j = nlhdr.nfiles - 1;
	while (i < j) {
	    k = (i + j) / 2;
	    ftp = &filetab[k];
	    a = ftp->addr;
	    if (a == addr) {
		s = ftp->filename;
		break;
	    } else if (addr > a) {
		i = k + 1;
	    } else {
		j = k - 1;
	    }
	}
	if (s == nil) {
	    if (addr >= filetab[i].addr) {
		s = filetab[i].filename;
	    } else {
		s = filetab[i-1].filename;
	    }
	}
    }
    return s;
}

/*
 * Find the line associated with the given address.
 * If the second parameter is true, then the address must match
 * a source line exactly.  Otherwise the nearest source line
 * below the given address is returned.  In any case, if no suitable
 * line exists, 0 is returned.
 */

private Lineno findline(addr, exact)
Address addr;
Boolean exact;
{
    register Address i, j, k;
    register Lineno r;
    register Address a;

    if (nlhdr.nlines == 0 or addr < linetab[0].addr) {
	r = 0;
    } else {
	i = 0;
	j = nlhdr.nlines - 1;
	if (addr == linetab[i].addr) {
	    r = linetab[i].line;
	} else if (addr == linetab[j].addr) {
	    r = linetab[j].line;
	} else if (addr > linetab[j].addr) {
	    r = exact ? 0 : linetab[j].line;
	} else {
	    do {
		k = (i + j) div 2;
		a = linetab[k].addr;
	    if (a == addr) break;
		if (addr > a) {
		    i = k + 1;
		} else {
		    j = k - 1;
		}
	    } while (i <= j);
	    if (a == addr) {
		r = linetab[k].line;
	    } else if (exact) {
		r = 0;
	    } else if (addr > linetab[i].addr) {
		r = linetab[i].line;
	    } else {
		r = linetab[i-1].line;
	    }
	}
    }
    return r;
}

/*
 * Lookup the source line number nearest from below to an address.
 */

public Lineno srcline(addr)
Address addr;
{
    return findline(addr, false);
}

/*
 * Look for a line exactly corresponding to the given address.
 */

public Lineno linelookup(addr)
Address addr;
{
    return findline(addr, true);
}

/*
 * Lookup the object address of a given line from the named file.
 *
 * Potentially all files in the file table need to be checked
 * until the line is found since a particular file name may appear
 * more than once in the file table (caused by includes).
 */

public Address objaddr(line, name)
Lineno line;
String name;
{
    register Filetab *ftp;
    register Lineno i, j;
    Boolean foundfile;

    if (nlhdr.nlines == 0) {
	return NOADDR;
    }
    if (name == nil) {
	name = cursource;
    }
    foundfile = false;
    for (ftp = &filetab[0]; ftp < &filetab[nlhdr.nfiles]; ftp++) {
	if (streq(ftp->filename, name)) {
	    foundfile = true;
	    i = ftp->lineindex;
	    if (ftp == &filetab[nlhdr.nfiles-1]) {
		j = nlhdr.nlines;
	    } else {
		j = (ftp + 1)->lineindex;
	    }
	    while (i < j) {
		if (linetab[i].line == line) {
		    return linetab[i].addr;
		}
		i++;
	    }
	}
    }
    if (not foundfile) {
	error("unknown source file \"%s\"", name);
    }
    return NOADDR;
}

/*
 * Table for going from object addresses to the functions in which they belong.
 */

#define MAXNFUNCS 1001      /* maximum number of functions allowed */

typedef struct {
    Symbol func;
    Address addr;
} AddrOfFunc;

private AddrOfFunc functab[MAXNFUNCS];
private int nfuncs;

/*
 * Insert a new function into the table.
 * The table is ordered by object address.
 */

public newfunc(f, addr)
Symbol f;
Address addr;
{
    register AddrOfFunc *af;

    if (nfuncs >= MAXNFUNCS) {
	panic("too many procedures/functions");
    }
    af = &functab[nfuncs];
    af->func = f;
    af->addr = addr;
    ++nfuncs;
}

/*
 * Return the function that begins at the given address.
 */

public Symbol whatblock(addr)
Address addr;
{
    register int i, j, k;
    Address a;

    i = 0;
    j = nfuncs - 1;
    if (addr < functab[i].addr) {
	return program;
    } else if (addr == functab[i].addr) {
	return functab[i].func;
    } else if (addr >= functab[j].addr) {
	return functab[j].func;
    }
    while (i <= j) {
	k = (i + j) / 2;
	a = functab[k].addr;
	if (a == addr) {
	    return functab[k].func;
	} else if (addr > a) {
	    i = k+1;
	} else {
	    j = k-1;
	}
    }
    if (addr > functab[i].addr) {
	return functab[i].func;
    } else {
	return functab[i-1].func;
    }
    /* NOTREACHED */
}

/*
 * Order the functab.
 */

private int cmpfunc(f1, f2)
AddrOfFunc *f1, *f2;
{
    register Address a1, a2;

    a1 = (*f1).addr;
    a2 = (*f2).addr;
    return ( (a1 < a2) ? -1 : ( (a1 == a2) ? 0 : 1 ) );
}

public ordfunctab()
{
    qsort(functab, nfuncs, sizeof(AddrOfFunc), cmpfunc);
}

/*
 * Clear out the functab, used when re-reading the object information.
 */

public clrfunctab()
{
    nfuncs = 0;
}

public dumpfunctab()
{
    int i;

    for (i = 0; i < nfuncs; i++) { 
	psym(functab[i].func);
    }
}
