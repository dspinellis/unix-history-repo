/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)object.c 1.2 %G%";

/*
 * Object code interface, mainly for extraction of symbolic information.
 */

#include "defs.h"
#include "object.h"
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

#endif

public String objname = "a.out";
public Integer objsize;
public char *stringtab;

private String progname = nil;
private Language curlang;
private Symbol curmodule;
private Symbol curparam;
private Boolean warned;

private Filetab *filep;
private Linetab *linep;
private Address curfaddr;

#define curfilename() (filep-1)->filename

/*
 * Blocks are figured out on the fly while reading the symbol table.
 */

#define MAXBLKDEPTH 25

private Symbol curblock;
private Symbol blkstack[MAXBLKDEPTH];
private Integer curlevel;

#define enterblock(b) { \
    blkstack[curlevel] = curblock; \
    ++curlevel; \
    b->level = curlevel; \
    b->block = curblock; \
    curblock = b; \
}

#define exitblock() { \
    --curlevel; \
    curblock = blkstack[curlevel]; \
}

/*
 * Enter a source line or file name reference into the appropriate table.
 * Expanded inline to reduce procedure calls.
 *
 * private enterline(linenumber, address)
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

#define NTYPES 1000

private Symbol typetable[NTYPES];

/*
 * Read in the namelist from the obj file.
 *
 * Reads and seeks are used instead of fread's and fseek's
 * for efficiency sake; there's a lot of data being read here.
 */

public readobj(file)
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
    objsize = hdr.a_text;
    nlhdr.nsyms = hdr.a_syms / sizeof(nlist);
    nlhdr.nfiles = nlhdr.nsyms;
    nlhdr.nlines = nlhdr.nsyms;
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
    close(f);
}

/*
 * Read in symbols from object file.
 */

private readsyms(f)
Fileid f;
{
    struct nlist *namelist;
    register struct nlist *np, *ub;
    register int index;
    register String name;
    register Boolean afterlg;

    initsyms();
    namelist = newarr(struct nlist, nlhdr.nsyms);
    read(f, namelist, nlhdr.nsyms * sizeof(struct nlist));
    afterlg = false;
    ub = &namelist[nlhdr.nsyms];
    for (np = &namelist[0]; np < ub; np++) {
	index = np->n_un.n_strx;
	if (index != 0) {
	    name = &stringtab[index - 4];
	} else {
	    name = nil;
	}
	/*
	 * assumptions:
	 *	not an N_STAB	==> name != nil
	 *	name[0] == '-'	==> name == "-lg"
	 *	name[0] != '_'	==> filename or invisible
	 *
	 * The "-lg" signals the beginning of global loader symbols.
	 */
	if ((np->n_type&N_STAB) != 0) {
	    enter_nl(name, np);
	} else if (name[0] == '-') {
	    afterlg = true;
	    if (curblock->class != PROG) {
		exitblock();
		if (curblock->class != PROG) {
		    exitblock();
		}
	    }
	    enterline(0, (linep-1)->addr + 1);
	} else if (name[0] == '_') {
	    if (afterlg) {
		check_global(&name[1], np);
	    } else if (curblock->name != nil) {
		check_local(&name[1], np);
	    }
	} else if ((np->n_type&N_TEXT) == N_TEXT) {
	    check_filename(name);
	}
    }
    dispose(namelist);
}

/*
 * Initialize symbol information.
 */

private initsyms()
{
    curblock = nil;
    curlevel = 0;
    if (progname == nil) {
	progname = strdup(objname);
	if (rindex(progname, '/') != nil) {
	    progname = rindex(progname, '/') + 1;
	}
	if (index(progname, '.') != nil) {
	    *(index(progname, '.')) = '\0';
	}
    }
    program = insert(identname(progname, true));
    program->class = PROG;
    newfunc(program);
    findbeginning(program);
    enterblock(program);
    curmodule = program;
    t_boolean = maketype("$boolean", 0L, 1L);
    t_int = maketype("$integer", 0x80000000L, 0x7fffffffL);
    t_char = maketype("$char", 0L, 127L);
    t_real = maketype("$real", 4L, 0L);
    t_nil = maketype("$nil", 0L, 0L);
}

/*
 * Free all the object file information that's being stored.
 */

public objfree()
{
    symbol_free();
    keywords_free();
    names_free();
    dispose(stringtab);
    clrfunctab();
}

/*
 * Enter a namelist entry.
 */

private enter_nl(name, np)
String name;
register struct nlist *np;
{
    register Symbol s;
    String mname, suffix;
    register Name n;
    register Symbol *tt;

    s = nil;
    if (name == nil) {
	n = nil;
    } else {
	n = identname(name, true);
    }
    switch (np->n_type) {
	case N_LBRAC:
	    s = symbol_alloc();
	    s->class = PROC;
	    enterblock(s);
	    break;

	case N_RBRAC:
	    exitblock();
	    break;

	case N_SLINE:
	    enterline((Lineno) np->n_desc, (Address) np->n_value);
	    break;

	/*
	 * Compilation unit.  C associates scope with filenames
	 * so we treat them as "modules".  The filename without
	 * the suffix is used for the module name.
	 *
	 * Because there is no explicit "end-of-block" mark in
	 * the object file, we must exit blocks for the current
	 * procedure and module.
	 */
	case N_SO:
	    mname = strdup(ident(n));
	    if (rindex(mname, '/') != nil) {
		mname = rindex(mname, '/') + 1;
	    }
	    suffix = rindex(mname, '.');
	    curlang = findlanguage(suffix);
	    if (suffix != nil) {
		*suffix = '\0';
	    }
	    if (curblock->class != PROG) {
		exitblock();
		if (curblock->class != PROG) {
		    exitblock();
		}
	    }
	    s = insert(identname(mname, true));
	    s->language = curlang;
	    s->class = MODULE;
	    enterblock(s);
	    curmodule = s;
	    if (program->language == nil) {
		program->language = curlang;
	    }
	    warned = false;
	    enterfile(ident(n), (Address) np->n_value);
	    for (tt = &typetable[0]; tt < &typetable[NTYPES]; tt++) {
		*tt = nil;
	    }
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
	    if (index(name, ':') == nil) {
		if (not warned) {
		    warned = true;
		    /*
		     * Shouldn't do this if user might be typing.
		     *
		    warning("old style symbol information found in \"%s\"",
			curfilename());
		     *
		     */
		}
	    } else {
		entersym(name, np);
	    }
	    break;

	case N_PC:
	    break;

	default:
	    if (name != nil) {
		printf("%s, ", name);
	    }
	    printf("ntype %2x, desc %x, value %x\n",
		np->n_type, np->n_desc, np->n_value);
	    break;
    }
}

/*
 * Check to see if a global _name is already in the symbol table,
 * if not then insert it.
 */

private check_global(name, np)
String name;
register struct nlist *np;
{
    register Name n;
    register Symbol t;

    if (not streq(name, "end")) {
	n = identname(name, true);
	if ((np->n_type&N_TYPE) == N_TEXT) {
	    find(t, n) where
		t->level == program->level and isblock(t)
	    endfind(t);
	    if (t == nil) {
		t = insert(n);
		t->language = findlanguage(".s");
		t->class = FUNC;
		t->type = t_int;
		t->block = curblock;
		t->level = program->level;
	    }
	    t->symvalue.funcv.beginaddr = np->n_value;
	    newfunc(t);
	    findbeginning(t);
	} else {
	    find(t, n) where
		t->class == VAR and t->level == program->level
	    endfind(t);
	    if (t == nil) {
		t = insert(n);
		t->language = findlanguage(".s");
		t->class = VAR;
		t->type = t_int;
		t->block = curblock;
		t->level = program->level;
	    }
	    t->symvalue.offset = np->n_value;
	}
    }
}

/*
 * Check to see if a local _name is known in the current scope.
 * If not then enter it.
 */

private check_local(name, np)
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
	t->level = cur->level;
	if ((np->n_type&N_TYPE) == N_TEXT) {
	    t->class = FUNC;
	    t->symvalue.funcv.beginaddr = np->n_value;
	    newfunc(t);
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

private check_filename(name)
String name;
{
    register String mname;
    register Integer i;
    register Symbol s;

    mname = strdup(name);
    i = strlen(mname) - 2;
    if (i >= 0 and mname[i] == '.' and mname[i+1] == 'o') {
	mname[i] = '\0';
	--i;
	while (mname[i] != '/' and i >= 0) {
	    --i;
	}
	s = insert(identname(&mname[i+1], true));
	s->language = findlanguage(".s");
	s->class = MODULE;
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
 * Put an nlist into the symbol table.
 * If it's already there just add the associated information.
 *
 * Type information is encoded in the name following a ":".
 */

private Symbol constype();
private Char *curchar;

#define skipchar(ptr, ch) { \
    if (*ptr != ch) { \
	panic("expected char '%c', found char '%c'", ch, *ptr); \
    } \
    ++ptr; \
}

private entersym(str, np)
String str;
struct nlist *np;
{
    register Symbol s;
    register char *p;
    register int c;
    register Name n;
    register Integer i;
    Boolean knowtype, isnew;
    Symclass class;
    Integer level;

    p = index(str, ':');
    *p = '\0';
    c = *(p+1);
    n = identname(str, true);
    if (index("FfGV", c) != nil) {
	if (c == 'F' or c == 'f') {
	    class = FUNC;
	} else {
	    class = VAR;
	}
	level = (c == 'f' ? curmodule->level : program->level);
	find(s, n) where s->level == level and s->class == class endfind(s);
	if (s == nil) {
	    isnew = true;
	    s = insert(n);
	} else {
	    isnew = false;
	}
    } else {
	isnew = true;
	s = insert(n);
    }

    /*
     * Default attributes.
     */
    s->language = curlang;
    s->class = VAR;
    s->block = curblock;
    s->level = curlevel;
    s->symvalue.offset = np->n_value;
    curchar = p + 2;
    knowtype = false;
    switch (c) {
	case 't':	/* type name */
	    s->class = TYPE;
	    i = getint();
	    if (i == 0) {
		panic("bad input on type \"%s\" at \"%s\"", symname(s),
		    curchar);
	    } else if (i >= NTYPES) {
		panic("too many types in file \"%s\"", curfilename());
	    }
	    /*
	     * A hack for C typedefs that don't create new types,
	     * e.g. typedef unsigned int Hashvalue;
	     */
	    if (*curchar == '\0') {
		s->type = typetable[i];
		if (s->type == nil) {
		    panic("nil type for %d", i);
		}
		knowtype = true;
	    } else {
		typetable[i] = s;
		skipchar(curchar, '=');
	    }
	    break;

	case 'T':	/* tag */
	    s->class = TAG;
	    i = getint();
	    if (i == 0) {
		panic("bad input on tag \"%s\" at \"%s\"", symname(s),
		    curchar);
	    } else if (i >= NTYPES) {
		panic("too many types in file \"%s\"", curfilename());
	    }
	    if (typetable[i] != nil) {
		typetable[i]->language = curlang;
		typetable[i]->class = TYPE;
		typetable[i]->type = s;
	    } else {
		typetable[i] = s;
	    }
	    skipchar(curchar, '=');
	    break;

	case 'F':	/* public function */
	case 'f':	/* private function */
	    s->class = FUNC;
	    if (curblock->class == FUNC or curblock->class == PROC) {
		exitblock();
	    }
	    enterblock(s);
	    if (c == 'F') {
		s->level = program->level;
		isnew = false;
	    }
	    curparam = s;
	    if (isnew) {
		s->symvalue.funcv.beginaddr = np->n_value;
		newfunc(s);
		findbeginning(s);
	    }
	    break;

	case 'G':	/* public variable */
	    s->level = program->level;
	    break;

	case 'S':	/* private variable */
	    s->level = curmodule->level;
	    s->block = curmodule;
	    break;

	case 'V':	/* own variable */
	    s->level = 2;
	    break;

	case 'r':	/* register variable */
	    s->level = -(s->level);
	    break;

	case 'p':	/* parameter variable */
	    curparam->chain = s;
	    curparam = s;
	    break;

	case 'v':	/* varies parameter */
	    s->class = REF;
	    s->symvalue.offset = np->n_value;
	    curparam->chain = s;
	    curparam = s;
	    break;

	default:	/* local variable */
	    --curchar;
	    break;
    }
    if (not knowtype) {
	s->type = constype(nil);
	if (s->class == TAG) {
	    addtag(s);
	}
    }
    if (tracesyms) {
	printdecl(s);
	fflush(stdout);
    }
}

/*
 * Construct a type out of a string encoding.
 *
 * The forms of the string are
 *
 *	<number>
 *	<number>=<type>
 *	r<type>;<number>;<number>		$ subrange
 *	a<type>;<type>				$ array[index] of element
 *	s{<name>:<type>;<number>;<number>}	$ record
 *	*<type>					$ pointer
 */

private Symbol constype(type)
Symbol type;
{
    register Symbol t, u;
    register Char *p, *cur;
    register Integer n;
    Integer b;
    Name name;
    Char class;

    b = curlevel;
    if (isdigit(*curchar)) {
	n = getint();
	if (n == 0) {
	    panic("bad type number at \"%s\"", curchar);
	} else if (n >= NTYPES) {
	    panic("too many types in file \"%s\"", curfilename());
	}
	if (*curchar == '=') {
	    if (typetable[n] != nil) {
		t = typetable[n];
	    } else {
		t = symbol_alloc();
		typetable[n] = t;
	    }
	    ++curchar;
	    constype(t);
	} else {
	    t = typetable[n];
	    if (t == nil) {
		t = symbol_alloc();
		typetable[n] = t;
	    }
	}
    } else {
	if (type == nil) {
	    t = symbol_alloc();
	} else {
	    t = type;
	}
	t->language = curlang;
	t->level = b;
	class = *curchar++;
	switch (class) {
	    case 'r':
		t->class = RANGE;
		t->type = constype(nil);
		skipchar(curchar, ';');
		t->symvalue.rangev.lower = getint();
		skipchar(curchar, ';');
		t->symvalue.rangev.upper = getint();
		break;

	    case 'a':
		t->class = ARRAY;
		t->chain = constype(nil);
		skipchar(curchar, ';');
		t->type = constype(nil);
		break;

	    case 's':
	    case 'u':
		t->class = (class == 's') ? RECORD : VARNT;
		t->symvalue.offset = getint();
		u = t;
		cur = curchar;
		while (*cur != ';' and *cur != '\0') {
		    p = index(cur, ':');
		    if (p == nil) {
			panic("index(\"%s\", ':') failed", curchar);
		    }
		    *p = '\0';
		    name = identname(cur, true);
		    u->chain = newSymbol(name, b, FIELD, nil, nil);
		    cur = p + 1;
		    u = u->chain;
		    u->language = curlang;
		    curchar = cur;
		    u->type = constype(nil);
		    skipchar(curchar, ',');
		    u->symvalue.field.offset = getint();
		    skipchar(curchar, ',');
		    u->symvalue.field.length = getint();
		    skipchar(curchar, ';');
		    cur = curchar;
		}
		if (*cur == ';') {
		    ++cur;
		}
		curchar = cur;
		break;

	    case 'e':
		t->class = SCAL;
		u = t;
		while (*curchar != ';' and *curchar != '\0') {
		    p = index(curchar, ':');
		    assert(p != nil);
		    *p = '\0';
		    u->chain = insert(identname(curchar, true));
		    curchar = p + 1;
		    u = u->chain;
		    u->language = curlang;
		    u->class = CONST;
		    u->level = b;
		    u->block = curblock;
		    u->type = t;
		    u->symvalue.iconval = getint();
		    skipchar(curchar, ',');
		}
		break;

	    case '*':
		t->class = PTR;
		t->type = constype(nil);
		break;

	    case 'f':
		t->class = FUNC;
		t->type = constype(nil);
		break;

	    default:
		badcaseval(class);
	}
    }
    return t;
}

/*
 * Read an integer from the current position in the type string.
 */

private Integer getint()
{
    register Integer n;
    register char *p;
    register Boolean isneg;

    n = 0;
    p = curchar;
    if (*p == '-') {
	isneg = true;
	++p;
    } else {
	isneg = false;
    }
    while (isdigit(*p)) {
	n = 10*n + (*p - '0');
	++p;
    }
    curchar = p;
    return isneg ? (-n) : n;
}

/*
 * Add a tag name.  This is a kludge to be able to refer
 * to tags that have the same name as some other symbol
 * in the same block.
 */

private addtag(s)
register Symbol s;
{
    register Symbol t;
    char buf[100];

    sprintf(buf, "$$%.90s", ident(s->name));
    t = insert(identname(buf, false));
    t->language = s->language;
    t->class = TAG;
    t->type = s->type;
    t->block = s->block;
}

/*
 * Allocate file and line tables and initialize indices.
 */

private allocmaps(nf, nl)
Integer nf, nl;
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
 */

private enterfile(filename, addr)
String filename;
Address addr;
{
    if (addr != curfaddr) {
	filep->addr = addr;
	filep->filename = filename;
	filep->lineindex = linep - linetab;
	++filep;
	curfaddr = addr;
    }
}

/*
 * Since we only estimated the number of lines (and it was a poor
 * estimation) and since we need to know the exact number of lines
 * to do a binary search, we set it when we're done.
 */

private setnlines()
{
    nlhdr.nlines = linep - linetab;
}

/*
 * Similarly for nfiles ...
 */

private setnfiles()
{
    nlhdr.nfiles = filep - filetab;
    setsource(filetab[0].filename);
}
