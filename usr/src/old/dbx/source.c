/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)source.c 1.9 %G%";

/*
 * Source file management.
 */

#include "defs.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "machine.h"

#ifndef public
typedef int Lineno;

String cursource;
Lineno curline;
Lineno cursrcline;

#define LASTLINE 0		/* recognized by printlines */

#include "lists.h"

List sourcepath;
#endif

private Lineno lastlinenum;
private String prevsource = nil;

/*
 * Data structure for indexing source seek addresses by line number.
 *
 * The constraints are:
 *
 *  we want an array so indexing is fast and easy
 *  we don't want to waste space for small files
 *  we don't want an upper bound on # of lines in a file
 *  we don't know how many lines there are
 *
 * The solution is a "dirty" hash table.  We have NSLOTS pointers to
 * arrays of NLINESPERSLOT addresses.  To find the source address of
 * a particular line we find the slot, allocate space if necessary,
 * and then find its location within the pointed to array.
 */

typedef long Seekaddr;

#define NSLOTS 20
#define NLINESPERSLOT 500

#define slotno(line)    ((line) div NLINESPERSLOT)
#define index(line)	((line) mod NLINESPERSLOT)
#define slot_alloc()    newarr(Seekaddr, NLINESPERSLOT)
#define srcaddr(line)	seektab[slotno(line)][index(line)]

private File srcfp;
private Seekaddr *seektab[NSLOTS];

/*
 * Print out the given lines from the source.
 */

public printlines(l1, l2)
Lineno l1, l2;
{
    register int c;
    register Lineno i, lb, ub;
    register File f;

    if (cursource == nil) {
	beginerrmsg();
	fprintf(stderr, "no source file\n");
    } else {
	if (cursource != prevsource) {
	    skimsource();
	}
	if (lastlinenum == 0) {
	    beginerrmsg();
	    fprintf(stderr, "couldn't read \"%s\"\n", cursource);
	} else {
	    lb = (l1 == 0) ? lastlinenum : l1;
	    ub = (l2 == 0) ? lastlinenum : l2;
	    if (lb < 1) {
		beginerrmsg();
		fprintf(stderr, "line number must be positive\n");
	    } else if (lb > lastlinenum) {
		beginerrmsg();
		if (lastlinenum == 1) {
		    fprintf(stderr, "\"%s\" has only 1 line\n", cursource);
		} else {
		    fprintf(stderr, "\"%s\" has only %d lines\n",
			cursource, lastlinenum);
		}
	    } else if (ub < lb) {
		beginerrmsg();
		fprintf(stderr, "second number must be greater than first\n");
	    } else {
		if (ub > lastlinenum) {
		    ub = lastlinenum;
		}
		f = srcfp;
		fseek(f, srcaddr(lb), 0);
		for (i = lb; i <= ub; i++) {
		    printf("%5d   ", i);
		    while ((c = getc(f)) != '\n') {
			putchar(c);
		    }
		    putchar('\n');
		}
		cursrcline = ub + 1;
	    }
	}
    }
}

/*
 * Search the sourcepath for a file.
 */

static char fileNameBuf[1024];

public String findsource(filename)
String filename;
{
    register File f;
    register String src, dir;

    if (filename[0] == '/') {
	src = filename;
    } else {
	src = nil;
	foreach (String, dir, sourcepath)
	    sprintf(fileNameBuf, "%s/%s", dir, filename);
	    f = fopen(fileNameBuf, "r");
	    if (f != nil) {
		fclose(f);
		src = fileNameBuf;
		break;
	    }
	endfor
    }
    return src;
}

/*
 * Open a source file looking in the appropriate places.
 */

public File opensource(filename)
String filename;
{
    String s;
    File f;

    s = findsource(filename);
    if (s == nil) {
	f = nil;
    } else {
	f = fopen(s, "r");
    }
    return f;
}

/*
 * Set the current source file.
 */

public setsource(filename)
String filename;
{
    if (filename != nil and filename != cursource) {
	prevsource = cursource;
	cursource = filename;
	cursrcline = 1;
    }
}

/*
 * Read the source file getting seek pointers for each line.
 */

private skimsource()
{
    register int c;
    register Seekaddr count;
    register File f;
    register Lineno linenum;
    register Seekaddr lastaddr;
    register int slot;

    f = opensource(cursource);
    if (f == nil) {
	lastlinenum = 0;
    } else {
	if (prevsource != nil) {
	    free_seektab();
	    if (srcfp != nil) {
		fclose(srcfp);
	    }
	}
	prevsource = cursource;
	linenum = 0;
	count = 0;
	lastaddr = 0;
	while ((c = getc(f)) != EOF) {
	    ++count;
	    if (c == '\n') {
		slot = slotno(++linenum);
		if (slot >= NSLOTS) {
		    panic("skimsource: too many lines");
		}
		if (seektab[slot] == nil) {
		    seektab[slot] = slot_alloc();
		}
		seektab[slot][index(linenum)] = lastaddr;
		lastaddr = count;
	    }
	}
	lastlinenum = linenum;
	srcfp = f;
    }
}

/*
 * Erase information and release space in the current seektab.
 * This is in preparation for reading in seek pointers for a
 * new file.  It is possible that seek pointers for all files
 * should be kept around, but the current concern is space.
 */

private free_seektab()
{
    register int slot;

    for (slot = 0; slot < NSLOTS; slot++) {
	if (seektab[slot] != nil) {
	    dispose(seektab[slot]);
	}
    }
}

/*
 * Figure out current source position.
 */

public getsrcpos()
{
    String filename;

    curline = srcline(pc);
    filename = srcfilename(pc);
    setsource(filename);
    if (curline != 0) {
	cursrcline = curline;
    }
}

/*
 * Print out the current source position.
 */

public printsrcpos()
{
    printf("at line %d", curline);
    if (nlhdr.nfiles > 1) {
	printf(" in file \"%s\"", cursource);
    }
}

#define DEF_EDITOR  "vi"

/*
 * Invoke an editor on the given file.  Which editor to use might change
 * installation to installation.  For now, we use "vi".  In any event,
 * the environment variable "EDITOR" overrides any default.
 */

public edit(filename)
String filename;
{
    extern String getenv();
    String ed, src, s;
    Symbol f;
    Address addr;
    char lineno[10];

    ed = getenv("EDITOR");
    if (ed == nil) {
	ed = DEF_EDITOR;
    }
    src = findsource((filename != nil) ? filename : cursource);
    if (src == nil) {
	f = which(identname(filename, true));
	if (not isblock(f)) {
	    error("can't read \"%s\"", filename);
	}
	addr = firstline(f);
	if (addr == NOADDR) {
	    error("no source for \"%s\"", filename);
	}
	src = srcfilename(addr);
	s = findsource(src);
	if (s != nil) {
	    src = s;
	}
	sprintf(lineno, "+%d", srcline(addr));
    } else {
	sprintf(lineno, "+1");
    }
    call(ed, stdin, stdout, lineno, src, nil);
}
