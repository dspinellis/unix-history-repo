/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)readobj.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * Read in the namelist from the obj file.
 */

#include "defs.h"
#include "sym.h"
#include "symtab.h"
#include "object.h"
#include "objfmt.h"
#include "main.h"
#include "mappings.h"
#include "mappings/filetab.h"
#include "mappings/linetab.h"
#include "objsym.rep"

#define MAXSYMNO 6000

char *objname = "obj";

LOCAL SYM *sym[MAXSYMNO];

readobj(file)
char *file;
{
    register FILE *fp;
    struct pxhdr hdr;

    if ((fp = fopen(file, "r")) == NIL) {
	panic("can't open %s", file);
    }
    get(fp, hdr);
    if (hdr.magicnum != MAGICNUM) {
	fseek(fp, (long) (HEADER_BYTES - sizeof(struct pxhdr)), 0);
	get(fp, hdr);
	if (hdr.magicnum != MAGICNUM) {
	    fatal("%s is not a Pascal object file", file);
	}
    }
    if (hdr.symtabsize == 0) {
	fatal("%s doesn't have symbolic information", file);
    }
    objsize = hdr.objsize;
    fseek(fp, (long) objsize, 1);
    if (get(fp, nlhdr) != 1) {
	panic("can't read nlhdr");
    }
    if (option('h')) {
	printf("\nHeader information:\n");
	printf("\tobject size %d\n", objsize);
	printf("\tsymtab size %d\n", hdr.symtabsize);
	printf("\tstringsize  %d\n", nlhdr.stringsize);
	printf("\tnsyms       %d\n", nlhdr.nsyms);
	printf("\tnfiles      %d\n", nlhdr.nfiles);
	printf("\tnlines      %d\n", nlhdr.nlines);
    }
    stringtab = alloc(nlhdr.stringsize, char);
    fread(stringtab, sizeof(char), nlhdr.stringsize, fp);
    readsyms(fp);
    readfiles(fp);
    readlines(fp);
    fclose(fp);
}

/*
 * Allocate and read in file name information table.
 */

LOCAL readfiles(fp)
register FILE *fp;
{
    register int i;
    register FILETAB *ftp;
    FILETAB temp;
    ADDRESS prevaddr;

    filetab = alloc(nlhdr.nfiles, FILETAB);
    ftp = &filetab[0];
    prevaddr = 0;
    for (i = 0; i < nlhdr.nfiles; i++) {
	fread(&temp, sizeof(FILETAB), 1, fp);
	if (temp.addr != prevaddr) {
	    ftp++;
	}
	*ftp = temp;
	ftp->filename += (int) stringtab;
	prevaddr = ftp->addr;
    }
    nlhdr.nfiles = (ftp - &filetab[0]) + 1;
    skimsource(filetab[0].filename);
    dotpfile = filetab[0].filename;
}

/*
 * Allocate and read in line number information table.
 */

LOCAL readlines(fp)
FILE *fp;
{
    register LINENO oline;
    register ADDRESS oaddr;
    register LINETAB *lp;
    FILETAB *ftp;
    OBJLINE info;

    if (nlhdr.nlines == 0) {
	return;
    }
    linetab = alloc(nlhdr.nlines, LINETAB);
    for (lp = &linetab[0]; lp < &linetab[nlhdr.nlines]; lp++) {
	lp->line = 0;
    }
    for (ftp = &filetab[0]; ftp < &filetab[nlhdr.nfiles]; ftp++) {
	if (ftp->lineindex < nlhdr.nlines) {
	    linetab[ftp->lineindex].line = ftp->line;
	}
    }
    oline = 0;
    oaddr = 0;
    for (lp = &linetab[0]; lp < &linetab[nlhdr.nlines]; lp++) {
	if (lp->line != 0) {
	    oline = lp->line;
	}
	info.together = getw(fp);
	oline += info.separate.lineincr;
	oaddr += info.separate.addrincr;
	lp->line = oline;
	lp->addr = oaddr;
    }
}

/*
 * Read in the symbols.
 */

readsyms(fp)
FILE *fp;
{
    register int i;
    int symno;

    symtab = st_creat(nlhdr.nsyms);
    for (i = 0; i < nlhdr.nsyms; i++) {
	symno = getw(fp);
	if (symno >= MAXSYMNO) {
	    panic("symbol number too large (%d)", symno);
	}
	sym[symno] = readsym(fp);
    }
    if (backpatch() != 0) {
	panic("patchlist not empty after reading namelist");
    }
    if (program == NIL) {
	panic("no program");
    }
    maketypes();
}

typedef struct patchinfo {
    SYM **patchsym;
    struct patchinfo *next_patch;
} PATCH;

LOCAL PATCH *phead;

/*
 * Go through patchlist looking for symbol numbers for which the
 * sym array now has a non-NIL entry.
 *
 * Afterwards, zap the sym array.
 */

int backpatch()
{
    register PATCH *p, *last, *next;
    register SYM *s, **t;
    int count;

    last = NIL;
    count = 0;
    for (p = phead; p != NIL; p = next) {
	next = p->next_patch;
	t = p->patchsym;
	if ((s = sym[(int) *t]) != NIL) {
	    *t = s;
	    if (last == NIL) {
		phead = next;
	    } else {
		last->next_patch = next;
	    }
	    dispose(p);
	} else {
	    last = p;
	    count++;
	}
    }
    for (t = &sym[0]; t < &sym[MAXSYMNO]; t++) {
	*t = NIL;
    }
    return(count);
}

/*
 * Check to see if the given pointer (really symbol number) should
 * be added to the patch list.  The argument is double indirect
 * to do call by reference passing.
 */

chkpatch(p)
SYM **p;
{
    register SYM *s, *t;
    register PATCH *patch;

    if ((s = *p) != NIL) {
	if ((t = sym[(int) s]) != NIL) {
	    *p = t;
	} else {
	    patch = alloc(1, PATCH);
	    patch->patchsym = p;
	    patch->next_patch = phead;
	    phead = patch;
	}
    }
}

/*
 * Free all the object information.
 */

objfree()
{
    register int i;

    st_destroy(symtab);
    dispose(stringtab);
    dispose(filetab);
    dispose(linetab);
    clrfunctab();
    for (i = 0; i < MAXSYMNO; i++) {
	sym[i] = NIL;
    }
}
