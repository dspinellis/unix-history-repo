/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stabstring.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * String information interpretation
 *
 * The string part of a stab entry is broken up into name and type information.
 */

#include "defs.h"
#include "stabstring.h"
#include "object.h"
#include "main.h"
#include "symbols.h"
#include "names.h"
#include "languages.h"
#include "tree.h"
#include <a.out.h>
#include <ctype.h>

#ifndef public
#endif

/*
 * Special characters in symbol table information.
 */

#define CONSTNAME 'c'
#define TYPENAME 't'
#define TAGNAME 'T'
#define MODULEBEGIN 'm'
#define EXTPROCEDURE 'P'
#define PRIVPROCEDURE 'Q'
#define INTPROCEDURE 'I'
#define EXTFUNCTION 'F'
#define PRIVFUNCTION 'f'
#define INTFUNCTION 'J'
#define EXTVAR 'G'
#define MODULEVAR 'S'
#define OWNVAR 'V'
#define REGVAR 'r'
#define VALUEPARAM 'p'
#define VARIABLEPARAM 'v'
#define LOCALVAR /* default */

/*
 * Type information special characters.
 */

#define T_SUBRANGE 'r'
#define T_ARRAY 'a'
#define T_OLDOPENARRAY 'A'
#define T_OPENARRAY 'O'
#define T_DYNARRAY 'D'
#define T_SUBARRAY 'E'
#define T_RECORD 's'
#define T_UNION 'u'
#define T_ENUM 'e'
#define T_PTR '*'
#define T_FUNCVAR 'f'
#define T_PROCVAR 'p'
#define T_IMPORTED 'i'
#define T_SET 'S'
#define T_OPAQUE 'o'
#define T_FILE 'd'

/*
 * Table of types indexed by per-file unique identification number.
 */

#define NTYPES 1000

private Symbol typetable[NTYPES];

public initTypeTable ()
{
    bzero(typetable, sizeof(typetable));
    (*language_op(curlang, L_MODINIT))(typetable);
}

/*
 * Put an nlist entry into the symbol table.
 * If it's already there just add the associated information.
 *
 * Type information is encoded in the name following a ":".
 */

private Symbol constype();
private Char *curchar;

#define skipchar(ptr, ch) \
{ \
    if (*ptr != ch) { \
	panic("expected char '%c', found '%s'", ch, ptr); \
    } \
    ++ptr; \
}

#define optchar(ptr, ch) \
{ \
    if (*ptr == ch) { \
	++ptr; \
    } \
}

#ifdef sun
#    define chkcont(ptr) \
{ \
    if (*ptr == '\\' or *ptr == '?') { \
	ptr = getcont(); \
    } \
}
#else if notsun
#    define chkcont(ptr) \
{ \
    if (*ptr == '?') { \
	ptr = getcont(); \
    } \
}
#endif

#define newSym(s, n) \
{ \
    s = insert(n); \
    s->level = curblock->level + 1; \
    s->language = curlang; \
    s->block = curblock; \
}

#define makeVariable(s, n, off) \
{ \
    newSym(s, n); \
    s->class = VAR; \
    s->symvalue.offset = off; \
    getType(s); \
}

#define makeParameter(s, n, cl, off) \
{ \
    if ((s = lookup(n)) == nil or s->block != curblock) { \
	newSym(s, n); \
	s->storage = STK; \
	s->class = cl; \
	s->symvalue.offset = off; \
	getType(s); \
    } \
    curparam->chain = s; \
    curparam = s; \
}

public entersym (name, np)
String name;
struct nlist *np;
{
    Symbol s;
    register char *p, *q, *r;
    register Name n;
    char c;

    p = index(name, ':');
    *p = '\0';
    c = *(p+1);
    if (autostrip and streq(language_name(curlang), "c++")) {
	/*
	 * Strip off redundant prefixes from C++ names.
	 * Static variables are prefixed with _static_.
	 * Formal arguments of functions are prefixed with _au0_.
	 * Automatic variables are prefixed with _au[1-9][0-9]*_.
	 * Class members are prefixed with _T_, where T is a class tag.
	 */
	if (strncmp("_static_", name, 8) == 0 and name[8] != '\0') {
	    name += 8;
	}
	q = name;
	if (*q++ == '_' and *q++ == 'a' and *q++ == 'u' and isdigit(*q++)) {
	    while (isdigit(*q))
		++q;
	    if (*q++ == '_' and *q != '\0')
		name = q;
	}
	q = name;
	if (*q++ == '_' and c == EXTFUNCTION) {
	    /*
	     * Punt on static class members, for speed.
	     */
	    for (r = q; (r = index(r, '_')) != nil; ++r) {
		if (r == q) {
		    continue;
		}
		*r = '\0';
		s = lookup(identname(q, true));
		if (s != nil and s->class == TYPE) {
		    char *newname = r + 1;
		    if (*newname != '\0') {
			name = newname;
			break;
		    }
		}
		*r = '_';
	    }
	}
    }
    n = identname(name, true);
    chkUnnamedBlock();
    curchar = p + 2;
    switch (c) {
	case CONSTNAME:
	    newSym(s, n);
	    constName(s);
	    break;

	case TYPENAME:
	    newSym(s, n);
	    typeName(s);
	    break;

	case TAGNAME:
	    s = symbol_alloc();
	    s->name = n;
	    s->level = curblock->level + 1;
	    s->language = curlang;
	    s->block = curblock;
	    tagName(s);
	    break;

	case MODULEBEGIN:
	    publicRoutine(&s, n, MODULE, np->n_value, false);
	    curmodule = s;
	    break;

	case EXTPROCEDURE:
	    publicRoutine(&s, n, PROC, np->n_value, false);
	    break;

	case PRIVPROCEDURE:
	    privateRoutine(&s, n, PROC, np->n_value);
	    break;

	case INTPROCEDURE:
	    publicRoutine(&s, n, PROC, np->n_value, true);
	    break;

	case EXTFUNCTION:
	    publicRoutine(&s, n, FUNC, np->n_value, false);
	    break;

	case PRIVFUNCTION:
	    privateRoutine(&s, n, FUNC, np->n_value);
	    break;

	case INTFUNCTION:
	    publicRoutine(&s, n, FUNC, np->n_value, true);
	    break;

	case EXTVAR:
	    extVar(&s, n, np->n_value);
	    break;

	case MODULEVAR:
	    if (curblock->class != MODULE) {
		exitblock();
	    }
	    makeVariable(s, n, np->n_value);
	    s->storage = EXT;
	    s->level = program->level;
	    s->block = curmodule;
	    getExtRef(s);
	    break;

	case OWNVAR:
	    makeVariable(s, n, np->n_value);
	    ownVariable(s, np->n_value);
	    getExtRef(s);
	    break;

	case REGVAR:
	    makeVariable(s, n, np->n_value);
	    s->storage = INREG;
	    break;

	case VALUEPARAM:
	    makeParameter(s, n, VAR, np->n_value);
#	    ifdef IRIS
		/*
		 * Bug in SGI C compiler -- generates stab offset
		 * for parameters with size added in.
		 */
		if (s->storage == STK and curlang == findlanguage(".c")) {
		    s->symvalue.offset -= size(s);
		}
#	    endif
	    break;

	case VARIABLEPARAM:
	    makeParameter(s, n, REF, np->n_value);
	    break;

	default:	/* local variable */
	    --curchar;
	    makeVariable(s, n, np->n_value);
	    s->storage = STK;
	    break;
    }
    if (tracesyms) {
	printdecl(s);
	fflush(stdout);
    }
}

/*
 * Enter a named constant.
 */

private constName (s)
Symbol s;
{
    integer i;
    double d;
    char *p, buf[1000];

    s->class = CONST;
    skipchar(curchar, '=');
    p = curchar;
    ++curchar;
    switch (*p) {
	case 'b':
	    s->type = t_boolean;
	    s->symvalue.constval = build(O_LCON, getint());
	    break;

	case 'c':
	    s->type = t_char;
	    s->symvalue.constval = build(O_LCON, getint());
	    break;

	case 'i':
	    s->type = t_int;
	    s->symvalue.constval = build(O_LCON, getint());
	    break;

	case 'r':
	    sscanf(curchar, "%lf", &d);
	    while (*curchar != '\0' and *curchar != ';') {
		++curchar;
	    }
	    --curchar;
	    s->type = t_real;
	    s->symvalue.constval = build(O_FCON, d);
	    break;

	case 's':
	    p = &buf[0];
	    skipchar(curchar, '\'');
	    while (*curchar != '\'') {
		*p = *curchar;
		++p;
		++curchar;
	    }
	    *p = '\0';
	    s->symvalue.constval = build(O_SCON, strdup(buf));
	    s->type = s->symvalue.constval->nodetype;
	    break;

	case 'e':
	    getType(s);
	    skipchar(curchar, ',');
	    s->symvalue.constval = build(O_LCON, getint());
	    break;

	case 'S':
	    getType(s);
	    skipchar(curchar, ',');
	    i = getint(); /* set size */
	    skipchar(curchar, ',');
	    i = getint(); /* number of bits in constant */
	    s->symvalue.constval = build(O_LCON, 0);
	    break;

	default:
	    s->type = t_int;
	    s->symvalue.constval = build(O_LCON, 0);
	    printf("[internal error: unknown constant type '%c']", *p);
	    break;
    }
    s->symvalue.constval->nodetype = s->type;
}

/*
 * Enter a type name.
 */

private typeName (s)
Symbol s;
{
    register integer i;

    s->class = TYPE;
    s->language = curlang;
    s->block = curblock;
    s->level = curblock->level + 1;
    i = getint();
    if (i == 0) {
	panic("bad input on type \"%s\" at \"%s\"", symname(s), curchar);
    } else if (i >= NTYPES) {
	panic("too many types in file \"%s\"", curfilename());
    }
    /*
     * A hack for C typedefs that don't create new types,
     * e.g. typedef unsigned int Hashvalue;
     *  or  typedef struct blah BLAH;
     */
    if (*curchar != '=') {
	s->type = typetable[i];
	if (s->type == nil) {
	    s->type = symbol_alloc();
	    typetable[i] = s->type;
	}
    } else {
	if (typetable[i] != nil) {
	    typetable[i]->language = curlang;
	    typetable[i]->class = TYPE;
	    typetable[i]->type = s;
	} else {
	    typetable[i] = s;
	}
	skipchar(curchar, '=');
	getType(s);
    }
}

/*
 * Enter a tag name.
 */

private tagName (s)
Symbol s;
{
    register integer i;

    s->class = TAG;
    i = getint();
    if (i == 0) {
	panic("bad input on tag \"%s\" at \"%s\"", symname(s), curchar);
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
    getType(s);
}

/*
 * Setup a symbol entry for a public procedure or function.
 *
 * If it contains nested procedures, then it may already be defined
 * in the current block as a MODULE.
 */

private publicRoutine (s, n, class, addr, isinternal)
Symbol *s;
Name n;
Symclass class;
Address addr;
boolean isinternal;
{
    Symbol nt, t;

    newSym(nt, n);
    if (isinternal) {
	markInternal(nt);
    }
    enterRoutine(nt, class);
    find(t, n) where
	t != nt and t->class == MODULE and t->block == nt->block
    endfind(t);
    if (t == nil) {
	t = nt;
    } else {
	t->language = nt->language;
	t->class = nt->class;
	t->type = nt->type;
	t->chain = nt->chain;
	t->symvalue = nt->symvalue;
	nt->class = EXTREF;
	nt->symvalue.extref = t;
	delete(nt);
	curparam = t;
	changeBlock(t);
    }
    if (t->block == program) {
	t->level = program->level;
    } else if (t->class == MODULE) {
	t->level = t->block->level;
    } else if (t->block->class == MODULE) {
	t->level = t->block->block->level;
    } else {
	t->level = t->block->level + 1;
    }
    *s = t;
}

/*
 * Setup a symbol entry for a private procedure or function.
 */

private privateRoutine (s, n, class, addr)
Symbol *s;
Name n;
Symclass class;
Address addr;
{
    Symbol t;
    boolean isnew;

    find(t, n) where
	t->level == curmodule->level and t->class == class
    endfind(t);
    if (t == nil) {
	isnew = true;
	t = insert(n);
    } else {
	isnew = false;
    }
    t->language = curlang;
    enterRoutine(t, class);
    if (isnew) {
	t->symvalue.funcv.src = false;
	t->symvalue.funcv.inline = false;
	t->symvalue.funcv.beginaddr = addr;
	newfunc(t, codeloc(t));
	findbeginning(t);
    }
    *s = t;
}

/*
 * Set up for beginning a new procedure, function, or module.
 * If it's a function, then read the type.
 *
 * If the next character is a ",", then read the name of the enclosing block.
 * Otherwise assume the previous function, if any, is over, and the current
 * routine is at the same level.
 */

private enterRoutine (s, class)
Symbol s;
Symclass class;
{
    s->class = class;
    if (class == FUNC) {
	getType(s);
    }
    if (s->class != MODULE) {
	getExtRef(s);
    } else if (*curchar == ',') {
	++curchar;
    }
    if (*curchar != '\0') {
	exitblock();
	enterNestedBlock(s);
    } else {
	if (curblock->class == FUNC or curblock->class == PROC) {
	    exitblock();
	}
	if (class == MODULE) {
	    exitblock();
	}
	enterblock(s);
    }
    curparam = s;
}

/*
 * Handling an external variable is tricky, since we might already
 * know it but need to define it's type for other type information
 * in the file.  So just in case we read the type information anyway.
 */

private extVar (symp, n, off)
Symbol *symp;
Name n;
integer off;
{
    Symbol s, t;

    find(s, n) where
	s->level == program->level and s->class == VAR
    endfind(s);
    if (s == nil) {
	makeVariable(s, n, off);
	s->storage = EXT;
	s->level = program->level;
	s->block = curmodule;
	getExtRef(s);
    } else {
	t = constype(nil);
    }
    *symp = s;
}

/*
 * Check to see if the stab string contains the name of the external
 * reference.  If so, we create a symbol with that name and class EXTREF, and
 * connect it to the given symbol.  This link is created so that when
 * we see the linker symbol we can resolve it to the given symbol.
 */

private getExtRef (s)
Symbol s;
{
    char *p;
    Name n;
    Symbol t;

    if (*curchar == ',' and *(curchar + 1) != '\0') {
	p = index(curchar + 1, ',');
	*curchar = '\0';
	if (p != nil) {
	    *p = '\0';
	    n = identname(curchar + 1, false);
	    curchar = p + 1;
	} else {
	    n = identname(curchar + 1, true);
	}
	t = insert(n);
	t->language = s->language;
	t->class = EXTREF;
	t->block = program;
	t->level = program->level;
	t->symvalue.extref = s;
    }
}

/*
 * Find a block with the given identifier in the given outer block.
 * If not there, then create it.
 */

private Symbol findBlock (id, m)
String id;
Symbol m;
{
    Name n;
    Symbol s;

    n = identname(id, true);
    find(s, n) where s->block == m and isblock(s) endfind(s);
    if (s == nil) {
	s = insert(n);
	s->block = m;
	s->language = curlang;
	s->class = MODULE;
	s->level = m->level + 1;
    }
    return s;
}

/*
 * Enter a nested block.
 * The block within which it is nested is described
 * by "module{:module}[:proc]".
 */

private enterNestedBlock (b)
Symbol b;
{
    register char *p, *q;
    Symbol m, s;
    Name n;

    q = curchar;
    p = index(q, ':');
    m = program;
    while (p != nil) {
	*p = '\0';
	m = findBlock(q, m);
	q = p + 1;
	p = index(q, ':');
    }
    if (*q != '\0') {
	m = findBlock(q, m);
    }
    b->level = m->level + 1;
    b->block = m;
    pushBlock(b);
}

/*
 * Enter a statically-allocated variable defined within a routine.
 *
 * Global BSS variables are chained together so we can resolve them
 * when the start of common is determined.  The list is kept in order
 * so that f77 can display all vars in a COMMON.
 */

private ownVariable (s, addr)
Symbol s;
Address addr;
{
    s->storage = EXT;
    /* s->level = 1; */
    if (curcomm) {
	if (commchain != nil) {
	    commchain->symvalue.common.chain = s;
	} else {
	    curcomm->symvalue.common.offset = (integer) s;
	}			  
	commchain = s;
	s->symvalue.common.offset = addr;
	s->symvalue.common.chain = nil;
    }
}

/*
 * Get a type from the current stab string for the given symbol.
 */

private getType (s)
Symbol s;
{
    Symbol t, addtag();

    if (s->class == TAG) {
	t = addtag(s);
	t->type = constype(nil);
	s->type = t->type;
    } else {
	s->type = constype(nil);
    }
}

/*
 * Construct a type out of a string encoding.
 */

private Rangetype getRangeBoundType();

private Symbol constype (type)
Symbol type;
{
    register Symbol t;
    register integer n;
    char class;
    char *p;

    while (*curchar == '@') {
	p = index(curchar, ';');
	if (p == nil) {
	    fflush(stdout);
	    fprintf(stderr, "missing ';' after type attributes");
	} else {
	    curchar = p + 1;
	}
    }
    if (isdigit(*curchar)) {
	n = getint();
	if (n >= NTYPES) {
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
	t->level = curblock->level + 1;
	t->block = curblock;
	class = *curchar++;
	switch (class) {
	    case T_SUBRANGE:
		consSubrange(t);
		break;

	    case T_ARRAY:
		t->class = ARRAY;
		t->chain = constype(nil);
		skipchar(curchar, ';');
		chkcont(curchar);
		t->type = constype(nil);
		break;

	    case T_OLDOPENARRAY:
		t->class = DYNARRAY;
		t->symvalue.ndims = 1;
		t->type = constype(nil);
		t->chain = t_int;
		break;

	    case T_OPENARRAY:
		consDynarray(t, OPENARRAY);
		break;

	    case T_DYNARRAY:
		consDynarray(t, DYNARRAY);
		break;

	    case T_SUBARRAY:
		t->class = SUBARRAY;
		t->symvalue.ndims = getint();
		skipchar(curchar, ',');
		t->type = constype(nil);
		t->chain = t_int;
		break;

	    case T_RECORD:
		consRecord(t, RECORD);
		break;

	    case T_UNION:
		consRecord(t, VARNT);
		break;

	    case T_ENUM:
		consEnum(t);
		break;

	    case T_PTR:
		t->class = PTR;
		t->type = constype(nil);
		break;

	    /*
	     * C function variables are different from Modula-2's.
	     */
	    case T_FUNCVAR:
		t->class = FFUNC;
		t->type = constype(nil);
		if (streq(language_name(curlang), "modula-2")) {
		    skipchar(curchar, ',');
		    consParamlist(t);
		}
		break;

	    case T_PROCVAR:
		t->class = FPROC;
		consParamlist(t);
		break;

	    case T_IMPORTED:
		consImpType(t);
		break;

	    case T_SET:
		t->class = SET;
		t->type = constype(nil);
		break;

	    case T_OPAQUE:
		consOpaqType(t);
		break;

	    case T_FILE:
		t->class = FILET;
		t->type = constype(nil);
		break;

	    default:
		badcaseval(class);
	}
    }
    return t;
}

/*
 * Construct a subrange type.
 */

private consSubrange (t)
Symbol t;
{
    t->class = RANGE;
    t->type = constype(nil);
    skipchar(curchar, ';');
    chkcont(curchar);
    t->symvalue.rangev.lowertype = getRangeBoundType();
    t->symvalue.rangev.lower = getint();
    skipchar(curchar, ';');
    chkcont(curchar);
    t->symvalue.rangev.uppertype = getRangeBoundType();
    t->symvalue.rangev.upper = getint();
}

/*
 * Figure out the bound type of a range.
 *
 * Some letters indicate a dynamic bound, ie what follows
 * is the offset from the fp which contains the bound; this will
 * need a different encoding when pc a['A'..'Z'] is
 * added; J is a special flag to handle fortran a(*) bounds
 */

private Rangetype getRangeBoundType ()
{
    Rangetype r;

    switch (*curchar) {
	case 'A':
	    r = R_ARG;
	    curchar++;
	    break;

	case 'T':
	    r = R_TEMP;
	    curchar++;
	    break;

	case 'J': 
	    r = R_ADJUST;
	    curchar++;
	    break;

	default:
	    r = R_CONST;
	    break;
    }
    return r;
}

/*
 * Construct a dynamic array descriptor.
 */

private consDynarray (t, c)
register Symbol t;
Symclass c;
{
    t->class = c;
    t->symvalue.ndims = getint();
    skipchar(curchar, ',');
    t->type = constype(nil);
    t->chain = t_int;
}

/*
 * Construct a record or union type.
 */

private consRecord (t, class)
Symbol t;
Symclass class;
{
    register Symbol u;
    register char *cur, *p;
    Name name;
    integer d;

    t->class = class;
    t->symvalue.offset = getint();
    d = curblock->level + 1;
    u = t;
    chkcont(curchar);
    cur = curchar;
    while (*cur != ';' and *cur != '\0') {
	p = index(cur, ':');
	if (p == nil) {
	    panic("index(\"%s\", ':') failed", curchar);
	}
	*p = '\0';
	if (
	    autostrip and
	    *cur == '_' and
	    streq(language_name(curlang), "c++")
	) {
	    /*
	     * Strip off redundant prefixes from C++ names.
	     * Class members are prefixed with _T_, where T is a class tag.
	     */
	    register char *q, *r;
	    Symbol s;

	    /*
	     * The slow way...  Check for members defined in the base class.
	     */
	    for (q = cur + 1, r = q; (r = index(r, '_')) != nil; ++r) {
		if (r == q) {
		    continue;
		}
		*r = '\0';
		s = lookup(identname(q, true));
		if (s != nil and s->class == TYPE) {
		    char *newcur = r + 1;
		    if (*newcur != '\0') {
			cur = newcur;
			break;
		    }
		}
		*r = '_';
	    }
	}
	name = identname(cur, true);
	u->chain = newSymbol(name, d, FIELD, nil, nil);
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
	chkcont(curchar);
	cur = curchar;
    }
    if (*cur == ';') {
	++cur;
    }
    curchar = cur;
}

/*
 * Construct an enumeration type.
 */

private consEnum (t)
Symbol t;
{
    register Symbol u;
    register char *p;
    register integer count;

    t->class = SCAL;
    count = 0;
    u = t;
    while (*curchar != ';' and *curchar != '\0' and *curchar != ',') {
	p = index(curchar, ':');
	assert(p != nil);
	*p = '\0';
	u->chain = insert(identname(curchar, true));
	curchar = p + 1;
	u = u->chain;
	u->language = curlang;
	u->class = CONST;
	u->level = curblock->level + 1;
	u->block = curblock;
	u->type = t;
	u->symvalue.constval = build(O_LCON, (long) getint());
	++count;
	skipchar(curchar, ',');
	chkcont(curchar);
    }
    if (*curchar == ';') {
	++curchar;
    }
    t->symvalue.iconval = count;
}

/*
 * Construct a parameter list for a function or procedure variable.
 */

private consParamlist (t)
Symbol t;
{
    Symbol p;
    integer i, d, n, paramclass;

    n = getint();
    skipchar(curchar, ';');
    p = t;
    d = curblock->level + 1;
    for (i = 0; i < n; i++) {
	p->chain = newSymbol(nil, d, VAR, nil, nil);
	p = p->chain;
	p->type = constype(nil);
	skipchar(curchar, ',');
	paramclass = getint();
	if (paramclass == 0) {
	    p->class = REF;
	}
	skipchar(curchar, ';');
	chkcont(curchar);
    }
}

/*
 * Construct an imported type.
 * Add it to a list of symbols to get fixed up.
 */

private consImpType (t)
Symbol t;
{
    register char *p;
    Symbol tmp;

    p = curchar;
    while (*p != ',' and *p != ';' and *p != '\0') {
	++p;
    }
    if (*p == '\0') {
	panic("bad import symbol entry '%s'", curchar);
    }
    t->class = TYPEREF;
    t->symvalue.typeref = curchar;
    if (*p == ',') {
	curchar = p + 1;
	tmp = constype(nil);
    } else {
	curchar = p;
    }
    skipchar(curchar, ';');
    *p = '\0';
}

/*
 * Construct an opaque type entry.
 */

private consOpaqType (t)
Symbol t;
{
    register char *p;
    register Symbol s;
    register Name n;
    boolean def;

    p = curchar;
    while (*p != ';' and *p != ',') {
	if (*p == '\0') {
	    panic("bad opaque symbol entry '%s'", curchar);
	}
	++p;
    }
    def = (Boolean) (*p == ',');
    *p = '\0';
    n = identname(curchar, true);
    find(s, n) where s->class == TYPEREF endfind(s);
    if (s == nil) {
	s = insert(n);
	s->class = TYPEREF;
	s->type = nil;
    }
    curchar = p + 1;
    if (def) {
	s->type = constype(nil);
	skipchar(curchar, ';');
    }
    t->class = TYPE;
    t->type = s;
}

/*
 * Read an integer from the current position in the type string.
 */

private integer getint ()
{
    register integer n;
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

private Symbol addtag (s)
register Symbol s;
{
    register Symbol t;
    char buf[100];

    if (streq(language_name(curlang), "c++")) {
	t = insert(s->name);
	t->class = TYPE;
    } else {
	sprintf(buf, "$$%.90s", ident(s->name));
	t = insert(identname(buf, false));
	t->class = TAG;
    }
    t->language = s->language;
    t->block = s->block;
    return t;
}
