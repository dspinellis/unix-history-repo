/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)object.c	5.2 (Berkeley) %G%";
#endif not lint

static char rcsid[] = "$Header: object.c,v 1.5 87/03/26 20:24:58 donn Exp $";

/*
 * Object code interface, mainly for extraction of symbolic information.
 */

#include "defs.h"
#include "object.h"
#include "stabstring.h"
#include "main.h"
#include "symbols.h"
#include "names.h"
#include "languages.h"
#include "mappings.h"
#include "lists.h"
#include <a.out.h>
#include <stab.h>
#include <ctype.h>

#ifndef public

struct {
    unsigned int stringsize;	/* size of the dumped string table */
    unsigned int nsyms;		/* number of symbols */
    unsigned int nfiles;	/* number of files */
    unsigned int nlines;	/* number of lines */
} nlhdr;

#include "languages.h"
#include "symbols.h"

#endif

#ifndef N_MOD2
#    define N_MOD2 0x50
#endif

public String objname = "a.out";
public integer objsize;

public Language curlang;
public Symbol curmodule;
public Symbol curparam;
public Symbol curcomm;
public Symbol commchain;

private char *stringtab;
private struct nlist *curnp;
private Boolean warned;
private Boolean strip_ = false;

private Filetab *filep;
private Linetab *linep, *prevlinep;

public String curfilename ()
{
    return ((filep-1)->filename);
}

/*
 * Blocks are figured out on the fly while reading the symbol table.
 */

#define MAXBLKDEPTH 25

public Symbol curblock;

private Symbol blkstack[MAXBLKDEPTH];
private integer curlevel;
private integer bnum, nesting;
private Address addrstk[MAXBLKDEPTH];

public pushBlock (b)
Symbol b;
{
    if (curlevel >= MAXBLKDEPTH) {
	fatal("nesting depth too large (%d)", curlevel);
    }
    blkstack[curlevel] = curblock;
    ++curlevel;
    curblock = b;
    if (traceblocks) {
	printf("entering block %s\n", symname(b));
    }
}

/*
 * Change the current block with saving the previous one,
 * since it is assumed that the symbol for the current one is to be deleted.
 */

public changeBlock (b)
Symbol b;
{
    curblock = b;
}

public enterblock (b)
Symbol b;
{
    if (curblock == nil) {
	b->level = 1;
    } else {
	b->level = curblock->level + 1;
    }
    b->block = curblock;
    pushBlock(b);
}

public exitblock ()
{
    if (curblock->class == FUNC or curblock->class == PROC) {
	if (prevlinep != linep) {
	    curblock->symvalue.funcv.src = true;
	}
    }
    if (curlevel <= 0) {
	panic("nesting depth underflow (%d)", curlevel);
    }
    --curlevel;
    if (traceblocks) {
	printf("exiting block %s\n", symname(curblock));
    }
    curblock = blkstack[curlevel];
}

/*
 * Enter a source line or file name reference into the appropriate table.
 * Expanded inline to reduce procedure calls.
 *
 * private enterline (linenumber, address)
 * Lineno linenumber;
 * Address address;
 *  ...
 */

#define enterline(linenumber, address) \
{ \
    register Linetab *lp; \
 \
    lp = linep - 1; \
    if (linenumber != lp->line) { \
	if (address != lp->addr) { \
	    ++lp; \
	} \
	lp->line = linenumber; \
	lp->addr = address; \
	linep = lp + 1; \
    } \
}

/*
 * Read in the namelist from the obj file.
 *
 * Reads and seeks are used instead of fread's and fseek's
 * for efficiency sake; there's a lot of data being read here.
 */

public readobj (file)
String file;
{
    Fileid f;
    struct exec hdr;
    struct nlist nlist;

    f = open(file, 0);
    if (f < 0) {
	fatal("can't open %s", file);
    }
    read(f, &hdr, sizeof(hdr));
    if (N_BADMAG(hdr)) {
	objsize = 0;
	nlhdr.nsyms = 0;
	nlhdr.nfiles = 0;
	nlhdr.nlines = 0;
    } else {
	objsize = hdr.a_text;
	nlhdr.nsyms = hdr.a_syms / sizeof(nlist);
	nlhdr.nfiles = nlhdr.nsyms;
	nlhdr.nlines = nlhdr.nsyms;
    }
    if (nlhdr.nsyms > 0) {
	lseek(f, (long) N_STROFF(hdr), 0);
	read(f, &(nlhdr.stringsize), sizeof(nlhdr.stringsize));
	nlhdr.stringsize -= 4;
	stringtab = newarr(char, nlhdr.stringsize);
	read(f, stringtab, nlhdr.stringsize);
	allocmaps(nlhdr.nfiles, nlhdr.nlines);
	lseek(f, (long) N_SYMOFF(hdr), 0);
	readsyms(f);
	ordfunctab();
	setnlines();
	setnfiles();
    } else {
	initsyms();
    }
    close(f);
}

/*
 * Found the beginning of the externals in the object file
 * (signified by the "-lg" or find an external), close the
 * block for the last procedure.
 */

private foundglobals ()
{
    if (curblock->class != PROG) {
	exitblock();
	if (curblock->class != PROG) {
	    exitblock();
	}
    }
    enterline(0, (linep-1)->addr + 1);
}

/*
 * Read in symbols from object file.
 */

private readsyms (f)
Fileid f;
{
    struct nlist *namelist;
    register struct nlist *np, *ub;
    register String name;
    boolean afterlg, foundstab;
    integer index;
    char *lastchar;

    initsyms();
    namelist = newarr(struct nlist, nlhdr.nsyms);
    read(f, namelist, nlhdr.nsyms * sizeof(struct nlist));
    afterlg = false;
    foundstab = false;
    ub = &namelist[nlhdr.nsyms];
    curnp = &namelist[0];
    np = curnp;
    while (np < ub) {
	index = np->n_un.n_strx;
	if (index != 0) {
	    name = &stringtab[index - 4];
	    /*
             *  If the program contains any .f files a trailing _ is stripped
       	     *  from the name on the assumption it was added by the compiler.
	     *  This only affects names that follow the sdb N_SO entry with
             *  the .f name. 
             */
            if (strip_ and name[0] != '\0' ) {
		lastchar = &name[strlen(name) - 1];
		if (*lastchar == '_') {
		    *lastchar = '\0';
		}
            }
	} else {
	    name = nil;
	} 

	/*
	 * Assumptions:
	 *	not an N_STAB	==> name != nil
	 *	name[0] == '-'	==> name == "-lg"
	 *	name[0] != '_'	==> filename or invisible
	 *
	 * The "-lg" signals the beginning of global loader symbols.
         *
	 */
	if ((np->n_type&N_STAB) != 0) {
	    foundstab = true;
	    enter_nl(name, np);
	} else if (name[0] == '-') {
	    afterlg = true;
	    foundglobals();
	} else if (afterlg) {
	    check_global(name, np);
	} else if ((np->n_type&N_EXT) == N_EXT) {
	    afterlg = true;
	    foundglobals();
	    check_global(name, np);
	} else if (name[0] == '_') {
	    check_local(&name[1], np);
	} else if ((np->n_type&N_TEXT) == N_TEXT) {
	    check_filename(name);
	}
	++curnp;
	np = curnp;
    }
    if (not foundstab) {
	warning("no source compiled with -g");
    }
    dispose(namelist);
}

/*
 * Get a continuation entry from the name list.
 * Return the beginning of the name.
 */

public String getcont ()
{
    register integer index;
    register String name;

    ++curnp;
    index = curnp->n_un.n_strx;
    if (index == 0) {
	name = "";
    } else {
	name = &stringtab[index - 4];
    }
    return name;
}

/*
 * Initialize symbol information.
 */

private initsyms ()
{
    curblock = nil;
    curlevel = 0;
    nesting = 0;
    program = insert(identname("", true));
    program->class = PROG;
    program->language = primlang;
    program->symvalue.funcv.beginaddr = CODESTART;
    program->symvalue.funcv.inline = false;
    newfunc(program, codeloc(program));
    findbeginning(program);
    enterblock(program);
    curmodule = program;
}

/*
 * Free all the object file information that's being stored.
 */

public objfree ()
{
    symbol_free();
    /* keywords_free(); */
    /* names_free(); */
    /* dispose(stringtab); */
    clrfunctab();
}

/*
 * Enter a namelist entry.
 */

private enter_nl (name, np)
String name;
register struct nlist *np;
{
    register Symbol s;
    register Name n;

    s = nil;
    switch (np->n_type) {
	/*
	 * Build a symbol for the FORTRAN common area.  All GSYMS that follow
	 * will be chained in a list with the head kept in common.offset, and
	 * the tail in common.chain.
	 */
	case N_BCOMM:
 	    if (curcomm) {
		curcomm->symvalue.common.chain = commchain;
	    }
	    n = identname(name, true);
	    curcomm = lookup(n);
	    if (curcomm == nil) {
		curcomm = insert(n);
		curcomm->class = COMMON;
		curcomm->block = curblock;
		curcomm->level = program->level;
		curcomm->symvalue.common.chain = nil;
	    }
	    commchain = curcomm->symvalue.common.chain;
	    break;

	case N_ECOMM:
	    if (curcomm) {
		curcomm->symvalue.common.chain = commchain;
		curcomm = nil;
	    }
	    break;

	case N_LBRAC:
	    ++nesting;
	    addrstk[nesting] = (linep - 1)->addr;
	    break;

	case N_RBRAC:
	    --nesting;
	    if (addrstk[nesting] == NOADDR) {
		exitblock();
		newfunc(curblock, (linep - 1)->addr);
		addrstk[nesting] = (linep - 1)->addr;
	    }
	    break;

	case N_SLINE:
	    enterline((Lineno) np->n_desc, (Address) np->n_value);
	    break;

	/*
	 * Source files.
	 */
	case N_SO:
	    n = identname(name, true);
	    enterSourceModule(n, (Address) np->n_value);
	    break;

	/*
	 * Textually included files.
	 */
	case N_SOL:
	    enterfile(name, (Address) np->n_value);
	    break;

	/*
	 * These symbols are assumed to have non-nil names.
	 */
	case N_GSYM:
	case N_FUN:
	case N_STSYM:
	case N_LCSYM:
	case N_RSYM:
	case N_PSYM:
	case N_LSYM:
	case N_SSYM:
	case N_LENG:
	    if (index(name, ':') == nil) {
		if (not warned) {
		    warned = true;
		    printf("warning: old style symbol information ");
		    printf("found in \"%s\"\n", curfilename());
		}
	    } else {
		entersym(name, np);
	    }
	    break;

	case N_PC:
	case N_MOD2:
	    break;

	default:
	    printf("warning:  stab entry unrecognized: ");
	    if (name != nil) {
		printf("name %s,", name);
	    }
	    printf("ntype %2x, desc %x, value %x'\n",
		np->n_type, np->n_desc, np->n_value);
	    break;
    }
}

/*
 * Try to find the symbol that is referred to by the given name.  Since it's
 * an external, we need to follow a level or two of indirection.
 */

private Symbol findsym (n, var_isextref)
Name n;
boolean *var_isextref;
{
    register Symbol r, s;

    *var_isextref = false;
    find(s, n) where
	(
	    s->level == program->level and (
		s->class == EXTREF or s->class == VAR or
		s->class == PROC or s->class == FUNC
	    )
	) or (
	    s->block == program and s->class == MODULE
	)
    endfind(s);
    if (s == nil) {
	r = nil;
    } else if (s->class == EXTREF) {
	*var_isextref = true;
	r = s->symvalue.extref;
	delete(s);

	/*
	 * Now check for another level of indirection that could come from
	 * a forward reference in procedure nesting information.  In this case
	 * the symbol has already been deleted.
	 */
	if (r != nil and r->class == EXTREF) {
	    r = r->symvalue.extref;
	}
/*
    } else if (s->class == MODULE) {
	s->class = FUNC;
	s->level = program->level;
	r = s;
 */
    } else {
	r = s;
    }
    return r;
}

/*
 * Create a symbol for a text symbol with no source information.
 * We treat it as an assembly language function.
 */

private Symbol deffunc (n)
Name n;
{
    Symbol f;

    f = insert(n);
    f->language = findlanguage(".s");
    f->class = FUNC;
    f->type = t_int;
    f->block = curblock;
    f->level = program->level;
    f->symvalue.funcv.src = false;
    f->symvalue.funcv.inline = false;
    if (f->chain != nil) {
	panic("chain not nil in deffunc");
    }
    return f;
}

/*
 * Create a symbol for a data or bss symbol with no source information.
 * We treat it as an assembly language variable.
 */

private Symbol defvar (n)
Name n;
{
    Symbol v;

    v = insert(n);
    v->language = findlanguage(".s");
    v->storage = EXT;
    v->class = VAR;
    v->type = t_int;
    v->level = program->level;
    v->block = curblock;
    return v;
}

/*
 * Update a symbol entry with a text address.
 */

private updateTextSym (s, name, addr)
Symbol s;
char *name;
Address addr;
{
    if (s->class == VAR) {
	s->symvalue.offset = addr;
    } else {
	s->symvalue.funcv.beginaddr = addr;
	if (name[0] == '_') {
	    newfunc(s, codeloc(s));
	    findbeginning(s);
	}
    }
}

/*
 * Avoid seeing Pascal labels as text symbols.
 */

private boolean PascalLabel (n)
Name n;
{
    boolean b;
    register char *p;

    b = false;
    if (curlang == findlanguage(".p")) {
	p = ident(n);
	while (*p != '\0') {
	    if (*p == '_' and *(p+1) == '$') {
		b = true;
		break;
	    }
	    ++p;
	}
    }
    return b;
}

/*
 * Check to see if a global _name is already in the symbol table,
 * if not then insert it.
 */

private check_global (name, np)
String name;
register struct nlist *np;
{
    register Name n;
    register Symbol t, u;
    char buf[4096];
    boolean isextref;
    integer count;

    if (not streq(name, "_end")) {
	if (name[0] == '_') {
	    n = identname(&name[1], true);
	} else {
	    n = identname(name, true);
	    if (lookup(n) != nil) {
		sprintf(buf, "$%s", name);
		n = identname(buf, false);
	    }
	}
	if ((np->n_type&N_TYPE) == N_TEXT) {
	    count = 0;
	    t = findsym(n, &isextref);
	    while (isextref) {
		++count;
		updateTextSym(t, name, np->n_value);
		t = findsym(n, &isextref);
	    }
	    if (count == 0) {
		if (t == nil) {
		    if (not PascalLabel(n)) {
			t = deffunc(n);
			updateTextSym(t, name, np->n_value);
			if (tracesyms) {
			    printdecl(t);
			}
		    }
		} else {
		    if (t->class == MODULE) {
			u = t;
			t = deffunc(n);
			t->block = u;
			if (tracesyms) {
			    printdecl(t);
			}
		    }
		    updateTextSym(t, name, np->n_value);
		}
	    }
	} else if ((np->n_type&N_TYPE) == N_BSS or (np->n_type&N_TYPE) == N_DATA) {
	    find(t, n) where
		t->class == COMMON
	    endfind(t);
	    if (t != nil) {
		u = (Symbol) t->symvalue.common.offset;
		while (u != nil) {
		    u->symvalue.offset = u->symvalue.common.offset+np->n_value;
		    u = u->symvalue.common.chain;
		}
            } else {
		check_var(np, n);
	    }
        } else {
	    check_var(np, n);
	}
    }
}

/*
 * Check to see if a namelist entry refers to a variable.
 * If not, create a variable for the entry.  In any case,
 * set the offset of the variable according to the value field
 * in the entry.
 *
 * If the external name has been referred to by several other symbols,
 * we must update each of them.
 */

private check_var (np, n)
struct nlist *np;
register Name n;
{
    register Symbol t, u, next;
    Symbol conflict;

    t = lookup(n);
    if (t == nil) {
	t = defvar(n);
	t->symvalue.offset = np->n_value;
	if (tracesyms) {
	    printdecl(t);
	}
    } else {
	conflict = nil;
	do {
	    next = t->next_sym;
	    if (t->name == n) {
		if (t->class == MODULE and t->block == program) {
		    conflict = t;
		} else if (t->class == EXTREF and t->level == program->level) {
		    u = t->symvalue.extref;
		    while (u != nil and u->class == EXTREF) {
			u = u->symvalue.extref;
		    }
		    u->symvalue.offset = np->n_value;
		    delete(t);
		} else if (t->level == program->level and
		    (t->class == VAR or t->class == PROC or t->class == FUNC)
		) {
		    conflict = nil;
		    t->symvalue.offset = np->n_value;
		}
	    }
	    t = next;
	} while (t != nil);
	if (conflict != nil) {
	    u = defvar(n);
	    u->block = conflict;
	    u->symvalue.offset = np->n_value;
	}
    }
}

/*
 * Check to see if a local _name is known in the current scope.
 * If not then enter it.
 */

private check_local (name, np)
String name;
register struct nlist *np;
{
    register Name n;
    register Symbol t, cur;

    n = identname(name, true);
    cur = ((np->n_type&N_TYPE) == N_TEXT) ? curmodule : curblock;
    find(t, n) where t->block == cur endfind(t);
    if (t == nil) {
	t = insert(n);
	t->language = findlanguage(".s");
	t->type = t_int;
	t->block = cur;
	t->storage = EXT;
	t->level = cur->level;
	if ((np->n_type&N_TYPE) == N_TEXT) {
	    t->class = FUNC;
	    t->symvalue.funcv.src = false;
	    t->symvalue.funcv.inline = false;
	    t->symvalue.funcv.beginaddr = np->n_value;
	    newfunc(t, codeloc(t));
	    findbeginning(t);
	} else {
	    t->class = VAR;
	    t->symvalue.offset = np->n_value;
	}
    }
}

/*
 * Check to see if a symbol corresponds to a object file name.
 * For some reason these are listed as in the text segment.
 */

private check_filename (name)
String name;
{
    register String mname;
    register integer i;
    Name n;
    Symbol s;

    mname = strdup(name);
    i = strlen(mname) - 2;
    if (i >= 0 and mname[i] == '.' and mname[i+1] == 'o') {
	mname[i] = '\0';
	--i;
	while (mname[i] != '/' and i >= 0) {
	    --i;
	}
	n = identname(&mname[i+1], true);
	find(s, n) where s->block == program and s->class == MODULE endfind(s);
	if (s == nil) {
	    s = insert(n);
	    s->language = findlanguage(".s");
	    s->class = MODULE;
	    s->symvalue.funcv.beginaddr = 0;
	    findbeginning(s);
	}
	if (curblock->class != PROG) {
	    exitblock();
	    if (curblock->class != PROG) {
		exitblock();
	    }
	}
	enterblock(s);
	curmodule = s;
    }
}

/*
 * Check to see if a symbol is about to be defined within an unnamed block.
 * If this happens, we create a procedure for the unnamed block, make it
 * "inline" so that tracebacks don't associate an activation record with it,
 * and enter it into the function table so that it will be detected
 * by "whatblock".
 */

public chkUnnamedBlock ()
{
    register Symbol s;
    static int bnum = 0;
    char buf[100];
    Address startaddr;

    if (nesting > 0 and addrstk[nesting] != NOADDR) {
	startaddr = (linep - 1)->addr;
	++bnum;
	sprintf(buf, "$b%d", bnum);
	s = insert(identname(buf, false));
	s->language = curlang;
	s->class = PROC;
	s->symvalue.funcv.src = false;
	s->symvalue.funcv.inline = true;
	s->symvalue.funcv.beginaddr = startaddr;
	enterblock(s);
	newfunc(s, startaddr);
	addrstk[nesting] = NOADDR;
    }
}

/*
 * Compilation unit.  C associates scope with filenames
 * so we treat them as "modules".  The filename without
 * the suffix is used for the module name.
 *
 * Because there is no explicit "end-of-block" mark in
 * the object file, we must exit blocks for the current
 * procedure and module.
 */

private enterSourceModule (n, addr)
Name n;
Address addr;
{
    register Symbol s;
    Name nn;
    String mname, suffix;

    mname = strdup(ident(n));
    if (rindex(mname, '/') != nil) {
	mname = rindex(mname, '/') + 1;
    }
    suffix = rindex(mname, '.');
    if (suffix > mname && *(suffix-1) == '.') {
	/* special hack for C++ */
	--suffix;
    }
    curlang = findlanguage(suffix);
    if (curlang == findlanguage(".f")) {
	strip_ = true;
    } 
    if (suffix != nil) {
	*suffix = '\0';
    }
    if (not (*language_op(curlang, L_HASMODULES))()) {
	if (curblock->class != PROG) {
	    exitblock();
	    if (curblock->class != PROG) {
		exitblock();
	    }
	}
	nn = identname(mname, true);
	if (curmodule == nil or curmodule->name != nn) {
	    s = insert(nn);
	    s->class = MODULE;
	    s->symvalue.funcv.beginaddr = 0;
	    findbeginning(s);
	} else {
	    s = curmodule;
	}
	s->language = curlang;
	enterblock(s);
	curmodule = s;
    }
    if (program->language == nil) {
	program->language = curlang;
    }
    warned = false;
    enterfile(ident(n), addr);
    initTypeTable();
}

/*
 * Allocate file and line tables and initialize indices.
 */

private allocmaps (nf, nl)
integer nf, nl;
{
    if (filetab != nil) {
	dispose(filetab);
    }
    if (linetab != nil) {
	dispose(linetab);
    }
    filetab = newarr(Filetab, nf);
    linetab = newarr(Linetab, nl);
    filep = filetab;
    linep = linetab;
}

/*
 * Add a file to the file table.
 *
 * If the new address is the same as the previous file address
 * this routine used to not enter the file, but this caused some
 * problems so it has been removed.  It's not clear that this in
 * turn may not also cause a problem.
 */

private enterfile (filename, addr)
String filename;
Address addr;
{
    filep->addr = addr;
    filep->filename = filename;
    filep->lineindex = linep - linetab;
    ++filep;
}

/*
 * Since we only estimated the number of lines (and it was a poor
 * estimation) and since we need to know the exact number of lines
 * to do a binary search, we set it when we're done.
 */

private setnlines ()
{
    nlhdr.nlines = linep - linetab;
}

/*
 * Similarly for nfiles ...
 */

private setnfiles ()
{
    nlhdr.nfiles = filep - filetab;
    setsource(filetab[0].filename);
}
