/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)symbols.c	5.4 (Berkeley) 1/12/88";
#endif not lint

static char rcsid[] = "$Header: symbols.c,v 1.3 87/03/26 23:17:35 donn Exp $";

/*
 * Symbol management.
 */

#include "defs.h"
#include "symbols.h"
#include "languages.h"
#include "printsym.h"
#include "tree.h"
#include "operators.h"
#include "eval.h"
#include "mappings.h"
#include "events.h"
#include "process.h"
#include "runtime.h"
#include "machine.h"
#include "names.h"

#ifndef public
typedef struct Symbol *Symbol;

#include "machine.h"
#include "names.h"
#include "languages.h"
#include "tree.h"

/*
 * Symbol classes
 */

typedef enum {
    BADUSE, CONST, TYPE, VAR, ARRAY, OPENARRAY, DYNARRAY, SUBARRAY,
    PTRFILE, RECORD, FIELD,
    PROC, FUNC, FVAR, REF, PTR, FILET, SET, RANGE, 
    LABEL, WITHPTR, SCAL, STR, PROG, IMPROPER, VARNT,
    FPROC, FFUNC, MODULE, TAG, COMMON, EXTREF, TYPEREF
} Symclass;

typedef enum { R_CONST, R_TEMP, R_ARG, R_ADJUST } Rangetype; 

#define INREG 0
#define STK 1
#define EXT 2

typedef unsigned integer Storage;

struct Symbol {
    Name name;
    Language language;
    Symclass class : 8;
    Storage storage : 2;
    unsigned int level : 6;	/* for variables stored on stack only */
    Symbol type;
    Symbol chain;
    union {
	Node constval;		/* value of constant symbol */
	int offset;		/* variable address */
	long iconval;		/* integer constant value */
	double fconval;		/* floating constant value */
	int ndims;		/* no. of dimensions for dynamic/sub-arrays */
	struct {		/* field offset and size (both in bits) */
	    int offset;
	    int length;
	} field;
	struct {		/* common offset and chain; used to relocate */
	    int offset;         /* vars in global BSS */
	    Symbol chain;
	} common;
	struct {		/* range bounds */
            Rangetype lowertype : 16; 
            Rangetype uppertype : 16;  
	    long lower;
	    long upper;
	} rangev;
	struct {
	    int offset : 16;	/* offset for of function value */
	    Boolean src : 1;	/* true if there is source line info */
	    Boolean inline : 1;	/* true if no separate act. rec. */
	    Boolean intern : 1; /* internal calling sequence */
	    int unused : 13;
	    Address beginaddr;	/* address of function code */
	} funcv;
	struct {		/* variant record info */
	    int size;
	    Symbol vtorec;
	    Symbol vtag;
	} varnt;
	String typeref;		/* type defined by "<module>:<type>" */
	Symbol extref;		/* indirect symbol for external reference */
    } symvalue;
    Symbol block;		/* symbol containing this symbol */
    Symbol next_sym;		/* hash chain */
};

/*
 * Basic types.
 */

Symbol t_boolean;
Symbol t_char;
Symbol t_int;
Symbol t_real;
Symbol t_nil;
Symbol t_addr;

Symbol program;
Symbol curfunc;

boolean showaggrs;

#define symname(s) ident(s->name)
#define codeloc(f) ((f)->symvalue.funcv.beginaddr)
#define isblock(s) (Boolean) ( \
    s->class == FUNC or s->class == PROC or \
    s->class == MODULE or s->class == PROG \
)
#define isroutine(s) (Boolean) ( \
    s->class == FUNC or s->class == PROC \
)

#define nosource(f) (not (f)->symvalue.funcv.src)
#define isinline(f) ((f)->symvalue.funcv.inline)

#define isreg(s)		(s->storage == INREG)

#include "tree.h"

/*
 * Some macros to make finding a symbol with certain attributes.
 */

#define find(s, withname) \
{ \
    s = lookup(withname); \
    while (s != nil and not (s->name == (withname) and

#define where /* qualification */

#define endfind(s) )) { \
	s = s->next_sym; \
    } \
}

#endif

/*
 * Symbol table structure currently does not support deletions.
 * Hash table size is a power of two to make hashing faster.
 * Using a non-prime is ok since we aren't doing rehashing.
 */

#define HASHTABLESIZE 8192

private Symbol hashtab[HASHTABLESIZE];

#define hash(name) ((((unsigned) name) >> 2) & (HASHTABLESIZE - 1))

/*
 * Allocate a new symbol.
 */

#define SYMBLOCKSIZE 1000

typedef struct Sympool {
    struct Symbol sym[SYMBLOCKSIZE];
    struct Sympool *prevpool;
} *Sympool;

private Sympool sympool = nil;
private Integer nleft = 0;

public Symbol symbol_alloc()
{
    register Sympool newpool;

    if (nleft <= 0) {
	newpool = new(Sympool);
	bzero(newpool, sizeof(*newpool));
	newpool->prevpool = sympool;
	sympool = newpool;
	nleft = SYMBLOCKSIZE;
    }
    --nleft;
    return &(sympool->sym[nleft]);
}

public symbol_dump (func)
Symbol func;
{
    register Symbol s;
    register integer i;

    printf(" symbols in %s \n",symname(func));
    for (i = 0; i < HASHTABLESIZE; i++) {
	for (s = hashtab[i]; s != nil; s = s->next_sym) {
	    if (s->block == func) {
		psym(s);
	    }
	}
    }
}

/*
 * Free all the symbols currently allocated.
 */

public symbol_free()
{
    Sympool s, t;
    register Integer i;

    s = sympool;
    while (s != nil) {
	t = s->prevpool;
	dispose(s);
	s = t;
    }
    for (i = 0; i < HASHTABLESIZE; i++) {
	hashtab[i] = nil;
    }
    sympool = nil;
    nleft = 0;
}

/*
 * Create a new symbol with the given attributes.
 */

public Symbol newSymbol(name, blevel, class, type, chain)
Name name;
Integer blevel;
Symclass class;
Symbol type;
Symbol chain;
{
    register Symbol s;

    s = symbol_alloc();
    s->name = name;
    s->language = primlang;
    s->storage = EXT;
    s->level = blevel;
    s->class = class;
    s->type = type;
    s->chain = chain;
    return s;
}

/*
 * Insert a symbol into the hash table.
 */

public Symbol insert(name)
Name name;
{
    register Symbol s;
    register unsigned int h;

    h = hash(name);
    s = symbol_alloc();
    s->name = name;
    s->next_sym = hashtab[h];
    hashtab[h] = s;
    return s;
}

/*
 * Symbol lookup.
 */

public Symbol lookup(name)
Name name;
{
    register Symbol s;
    register unsigned int h;

    h = hash(name);
    s = hashtab[h];
    while (s != nil and s->name != name) {
	s = s->next_sym;
    }
    return s;
}

/*
 * Delete a symbol from the symbol table.
 */

public delete (s)
Symbol s;
{
    register Symbol t;
    register unsigned int h;

    h = hash(s->name);
    t = hashtab[h];
    if (t == nil) {
	panic("delete of non-symbol '%s'", symname(s));
    } else if (t == s) {
	hashtab[h] = s->next_sym;
    } else {
	while (t->next_sym != s) {
	    t = t->next_sym;
	    if (t == nil) {
		panic("delete of non-symbol '%s'", symname(s));
	    }
	}
	t->next_sym = s->next_sym;
    }
}

/*
 * Dump out all the variables associated with the given
 * procedure, function, or program associated with the given stack frame.
 *
 * This is quite inefficient.  We traverse the entire symbol table
 * each time we're called.  The assumption is that this routine
 * won't be called frequently enough to merit improved performance.
 */

public dumpvars(f, frame)
Symbol f;
Frame frame;
{
    register Integer i;
    register Symbol s;

    for (i = 0; i < HASHTABLESIZE; i++) {
	for (s = hashtab[i]; s != nil; s = s->next_sym) {
	    if (container(s) == f) {
		if (should_print(s)) {
		    printv(s, frame);
		    putchar('\n');
		} else if (s->class == MODULE) {
		    dumpvars(s, frame);
		}
	    }
	}
    }
}

/*
 * Create a builtin type.
 * Builtin types are circular in that btype->type->type = btype.
 */

private Symbol maketype(name, lower, upper)
String name;
long lower;
long upper;
{
    register Symbol s;
    Name n;

    if (name == nil) {
	n = nil;
    } else {
	n = identname(name, true);
    }
    s = insert(n);
    s->language = primlang;
    s->level = 0;
    s->class = TYPE;
    s->type = nil;
    s->chain = nil;
    s->type = newSymbol(nil, 0, RANGE, s, nil);
    s->type->symvalue.rangev.lower = lower;
    s->type->symvalue.rangev.upper = upper;
    return s;
}

/*
 * Create the builtin symbols.
 */

public symbols_init ()
{
    Symbol s;

    t_boolean = maketype("$boolean", 0L, 1L);
    t_int = maketype("$integer", 0x80000000L, 0x7fffffffL);
    t_char = maketype("$char", 0L, 255L);
    t_real = maketype("$real", 8L, 0L);
    t_nil = maketype("$nil", 0L, 0L);
    t_addr = insert(identname("$address", true));
    t_addr->language = primlang;
    t_addr->level = 0;
    t_addr->class = TYPE;
    t_addr->type = newSymbol(nil, 1, PTR, t_int, nil);
    s = insert(identname("true", true));
    s->class = CONST;
    s->type = t_boolean;
    s->symvalue.constval = build(O_LCON, 1L);
    s->symvalue.constval->nodetype = t_boolean;
    s = insert(identname("false", true));
    s->class = CONST;
    s->type = t_boolean;
    s->symvalue.constval = build(O_LCON, 0L);
    s->symvalue.constval->nodetype = t_boolean;
}

/*
 * Reduce type to avoid worrying about type names.
 */

public Symbol rtype(type)
Symbol type;
{
    register Symbol t;

    t = type;
    if (t != nil) {
	if (t->class == VAR or t->class == CONST or
	    t->class == FIELD or t->class == REF
	) {
	    t = t->type;
	}
	if (t->class == TYPEREF) {
	    resolveRef(t);
	}
	while (t->class == TYPE or t->class == TAG) {
	    t = t->type;
	    if (t->class == TYPEREF) {
		resolveRef(t);
	    }
	}
    }
    return t;
}

/*
 * Find the end of a module name.  Return nil if there is none
 * in the given string.
 */

private String findModuleMark (s)
String s;
{
    register char *p, *r;
    register boolean done;

    p = s;
    done = false;
    do {
	if (*p == ':') {
	    done = true;
	    r = p;
	} else if (*p == '\0') {
	    done = true;
	    r = nil;
	} else {
	    ++p;
	}
    } while (not done);
    return r;
}

/*
 * Resolve a type reference by modifying to be the appropriate type.
 *
 * If the reference has a name, then it refers to an opaque type and
 * the actual type is directly accessible.  Otherwise, we must use
 * the type reference string, which is of the form "module:{module:}name".
 */

public resolveRef (t)
Symbol t;
{
    register char *p;
    char *start;
    Symbol s, m, outer;
    Name n;

    if (t->name != nil) {
	s = t;
    } else {
	start = t->symvalue.typeref;
	outer = program;
	p = findModuleMark(start);
	while (p != nil) {
	    *p = '\0';
	    n = identname(start, true);
	    find(m, n) where m->block == outer endfind(m);
	    if (m == nil) {
		p = nil;
		outer = nil;
		s = nil;
	    } else {
		outer = m;
		start = p + 1;
		p = findModuleMark(start);
	    }
	}
	if (outer != nil) {
	    n = identname(start, true);
	    find(s, n) where s->block == outer endfind(s);
	}
    }
    if (s != nil and s->type != nil) {
	t->name = s->type->name;
	t->class = s->type->class;
	t->type = s->type->type;
	t->chain = s->type->chain;
	t->symvalue = s->type->symvalue;
	t->block = s->type->block;
    }
}

public integer regnum (s)
Symbol s;
{
    integer r;

    checkref(s);
    if (s->storage == INREG) {
	r = s->symvalue.offset;
    } else {
	r = -1;
    }
    return r;
}

public Symbol container(s)
Symbol s;
{
    checkref(s);
    return s->block;
}

public Node constval(s)
Symbol s;
{
    checkref(s);
    if (s->class != CONST) {
	error("[internal error: constval(non-CONST)]");
    }
    return s->symvalue.constval;
}

/*
 * Return the object address of the given symbol.
 *
 * There are the following possibilities:
 *
 *	globals		- just take offset
 *	locals		- take offset from locals base
 *	arguments	- take offset from argument base
 *	register	- offset is register number
 */

#define isglobal(s)		(s->storage == EXT)
#define islocaloff(s)		(s->storage == STK and s->symvalue.offset < 0)
#define isparamoff(s)		(s->storage == STK and s->symvalue.offset >= 0)

public Address address (s, frame)
Symbol s;
Frame frame;
{
    register Frame frp;
    register Address addr;
    register Symbol cur;

    checkref(s);
    if (not isactive(s->block)) {
	error("\"%s\" is not currently defined", symname(s));
    } else if (isglobal(s)) {
	addr = s->symvalue.offset;
    } else {
	frp = frame;
	if (frp == nil) {
	    cur = s->block;
	    while (cur != nil and cur->class == MODULE) {
		cur = cur->block;
	    }
	    if (cur == nil) {
		frp = nil;
	    } else {
		frp = findframe(cur);
		if (frp == nil) {
		    error("[internal error: unexpected nil frame for \"%s\"]",
			symname(s)
		    );
		}
	    }
	}
	if (islocaloff(s)) {
	    addr = locals_base(frp) + s->symvalue.offset;
	} else if (isparamoff(s)) {
	    addr = args_base(frp) + s->symvalue.offset;
	} else if (isreg(s)) {
	    addr = savereg(s->symvalue.offset, frp);
	} else {
	    panic("address: bad symbol \"%s\"", symname(s));
	}
    }
    return addr;
}

/*
 * Define a symbol used to access register values.
 */

public defregname (n, r)
Name n;
integer r;
{
    Symbol s;

    s = insert(n);
    s->language = t_addr->language;
    s->class = VAR;
    s->storage = INREG;
    s->level = 3;
    s->type = t_addr;
    s->symvalue.offset = r;
}

/*
 * Resolve an "abstract" type reference.
 *
 * It is possible in C to define a pointer to a type, but never define
 * the type in a particular source file.  Here we try to resolve
 * the type definition.  This is problematic, it is possible to
 * have multiple, different definitions for the same name type.
 */

public findtype(s)
Symbol s;
{
    register Symbol t, u, prev;

    u = s;
    prev = nil;
    while (u != nil and u->class != BADUSE) {
	if (u->name != nil) {
	    prev = u;
	}
	u = u->type;
    }
    if (prev == nil) {
	error("couldn't find link to type reference");
    }
    t = lookup(prev->name);
    while (t != nil and
	not (
	    t != prev and t->name == prev->name and
	    t->block->class == MODULE and t->class == prev->class and
	    t->type != nil and t->type->type != nil and
	    t->type->type->class != BADUSE
	)
    ) {
	t = t->next_sym;
    }
    if (t == nil) {
	error("couldn't resolve reference");
    } else {
	prev->type = t->type;
    }
}

/*
 * Find the size in bytes of the given type.
 *
 * This is probably the WRONG thing to do.  The size should be kept
 * as an attribute in the symbol information as is done for structures
 * and fields.  I haven't gotten around to cleaning this up yet.
 */

#define MAXUCHAR 255
#define MAXUSHORT 65535L
#define MINCHAR -128
#define MAXCHAR 127
#define MINSHORT -32768
#define MAXSHORT 32767

public findbounds (u, lower, upper)
Symbol u;
long *lower, *upper;
{
    Rangetype lbt, ubt;
    long lb, ub;

    if (u->class == RANGE) {
	lbt = u->symvalue.rangev.lowertype;
	ubt = u->symvalue.rangev.uppertype;
	lb = u->symvalue.rangev.lower;
	ub = u->symvalue.rangev.upper;
	if (lbt == R_ARG or lbt == R_TEMP) {
	    if (not getbound(u, lb, lbt, lower)) {
		error("dynamic bounds not currently available");
	    }
	} else {
	    *lower = lb;
	}
	if (ubt == R_ARG or ubt == R_TEMP) {
	    if (not getbound(u, ub, ubt, upper)) {
		error("dynamic bounds not currently available");
	    }
	} else {
	    *upper = ub;
	}
    } else if (u->class == SCAL) {
	*lower = 0;
	*upper = u->symvalue.iconval - 1;
    } else {
	error("[internal error: unexpected array bound type]");
    }
}

public integer size(sym)
Symbol sym;
{
    register Symbol s, t, u;
    register integer nel, elsize;
    long lower, upper;
    integer r, off, len;

    t = sym;
    checkref(t);
    if (t->class == TYPEREF) {
	resolveRef(t);
    }
    switch (t->class) {
	case RANGE:
	    lower = t->symvalue.rangev.lower;
	    upper = t->symvalue.rangev.upper;
	    if (upper == 0 and lower > 0) {
		/* real */
		r = lower;
	    } else if (lower > upper) {
		/* unsigned long */
		r = sizeof(long);
	    } else if (
  		(lower >= MINCHAR and upper <= MAXCHAR) or
  		(lower >= 0 and upper <= MAXUCHAR)
  	      ) {
		r = sizeof(char);
  	    } else if (
  		(lower >= MINSHORT and upper <= MAXSHORT) or
  		(lower >= 0 and upper <= MAXUSHORT)
  	      ) {
		r = sizeof(short);
	    } else {
		r = sizeof(long);
	    }
	    break;

	case ARRAY:
	    elsize = size(t->type);
	    nel = 1;
	    for (t = t->chain; t != nil; t = t->chain) {
		u = rtype(t);
		findbounds(u, &lower, &upper);
		nel *= (upper-lower+1);
	    }
	    r = nel*elsize;
	    break;

	case OPENARRAY:
	case DYNARRAY:
	    r = (t->symvalue.ndims + 1) * sizeof(Word);
	    break;

	case SUBARRAY:
	    r = (2 * t->symvalue.ndims + 1) * sizeof(Word);
	    break;

	case REF:
	case VAR:
	    r = size(t->type);
	    /*
	     *
	    if (r < sizeof(Word) and isparam(t)) {
		r = sizeof(Word);
	    }
	    */
	    break;

	case FVAR:
	case CONST:
	case TAG:
	    r = size(t->type);
	    break;

	case TYPE:
	    /*
	     * This causes problems on the IRIS because of the compiler bug
	     * with stab offsets for parameters.  Not sure it's really
	     * necessary anyway.
	     */
#	    ifndef IRIS
	    if (t->type->class == PTR and t->type->type->class == BADUSE) {
		findtype(t);
	    }
#	    endif
	    r = size(t->type);
	    break;

	case FIELD:
	    off = t->symvalue.field.offset;
	    len = t->symvalue.field.length;
	    r = (off + len + 7) div 8 - (off div 8);
	    break;

	case RECORD:
	case VARNT:
	    r = t->symvalue.offset;
	    if (r == 0 and t->chain != nil) {
		panic("missing size information for record");
	    }
	    break;

	case PTR:
	case TYPEREF:
	case FILET:
	    r = sizeof(Word);
	    break;

	case SCAL:
	    r = sizeof(Word);
	    /*
	     *
	    if (t->symvalue.iconval > 255) {
		r = sizeof(short);
	    } else {
		r = sizeof(char);
	    }
	     *
	     */
	    break;

	case FPROC:
	case FFUNC:
	    r = sizeof(Word);
	    break;

	case PROC:
	case FUNC:
	case MODULE:
	case PROG:
	    r = sizeof(Symbol);
	    break;

	case SET:
	    u = rtype(t->type);
	    switch (u->class) {
		case RANGE:
		    r = u->symvalue.rangev.upper - u->symvalue.rangev.lower + 1;
		    break;

		case SCAL:
		    r = u->symvalue.iconval;
		    break;

		default:
		    error("expected range for set base type");
		    break;
	    }
	    r = (r + BITSPERBYTE - 1) div BITSPERBYTE;
	    break;

	/*
	 * These can happen in C (unfortunately) for unresolved type references
	 * Assume they are pointers.
	 */
	case BADUSE:
	    r = sizeof(Address);
	    break;

	default:
	    if (ord(t->class) > ord(TYPEREF)) {
		panic("size: bad class (%d)", ord(t->class));
	    } else {
		fprintf(stderr, "can't compute size of a %s\n", classname(t));
	    }
	    r = 0;
	    break;
    }
    return r;
}

/*
 * Return the size associated with a symbol that takes into account
 * reference parameters.  This might be better as the normal size function, but
 * too many places already depend on it working the way it does.
 */

public integer psize (s)
Symbol s;
{
    integer r;
    Symbol t;

    if (s->class == REF) {
	t = rtype(s->type);
	if (t->class == OPENARRAY) {
	    r = (t->symvalue.ndims + 1) * sizeof(Word);
	} else if (t->class == SUBARRAY) {
	    r = (2 * t->symvalue.ndims + 1) * sizeof(Word);
	} else {
	    r = sizeof(Word);
	}
    } else {
	r = size(s);
    }
    return r;
}

/*
 * Test if a symbol is a parameter.  This is true if there
 * is a cycle from s->block to s via chain pointers.
 */

public Boolean isparam(s)
Symbol s;
{
    register Symbol t;

    t = s->block;
    while (t != nil and t != s) {
	t = t->chain;
    }
    return (Boolean) (t != nil);
}

/*
 * Test if a type is an open array parameter type.
 */

public boolean isopenarray (type)
Symbol type;
{
    Symbol t;

    t = rtype(type);
    return (boolean) (t->class == OPENARRAY);
}

/*
 * Test if a symbol is a var parameter, i.e. has class REF.
 */

public Boolean isvarparam(s)
Symbol s;
{
    return (Boolean) (s->class == REF);
}

/*
 * Test if a symbol is a variable (actually any addressible quantity
 * with do).
 */

public Boolean isvariable(s)
Symbol s;
{
    return (Boolean) (s->class == VAR or s->class == FVAR or s->class == REF);
}

/*
 * Test if a symbol is a constant.
 */

public Boolean isconst(s)
Symbol s;
{
    return (Boolean) (s->class == CONST);
}

/*
 * Test if a symbol is a module.
 */

public Boolean ismodule(s)
register Symbol s;
{
    return (Boolean) (s->class == MODULE);
}

/*
 * Mark a procedure or function as internal, meaning that it is called
 * with a different calling sequence.
 */

public markInternal (s)
Symbol s;
{
    s->symvalue.funcv.intern = true;
}

public boolean isinternal (s)
Symbol s;
{
    return s->symvalue.funcv.intern;
}

/*
 * Decide if a field begins or ends on a bit rather than byte boundary.
 */

public Boolean isbitfield(s)
register Symbol s;
{
    boolean b;
    register integer off, len;
    register Symbol t;

    off = s->symvalue.field.offset;
    len = s->symvalue.field.length;
    if ((off mod BITSPERBYTE) != 0 or (len mod BITSPERBYTE) != 0) {
	b = true;
    } else {
	t = rtype(s->type);
	b = (Boolean) (
	    (t->class == SCAL and len != (sizeof(int)*BITSPERBYTE)) or
	    len != (size(t)*BITSPERBYTE)
	);
    }
    return b;
}

private boolean primlang_typematch (t1, t2)
Symbol t1, t2;
{
    return (boolean) (
	(t1 == t2) or
	(
	    t1->class == RANGE and t2->class == RANGE and
	    t1->symvalue.rangev.lower == t2->symvalue.rangev.lower and
	    t1->symvalue.rangev.upper == t2->symvalue.rangev.upper
	) or (
	    t1->class == PTR and t2->class == RANGE and
	    t2->symvalue.rangev.upper >= t2->symvalue.rangev.lower
	) or (
	    t2->class == PTR and t1->class == RANGE and
	    t1->symvalue.rangev.upper >= t1->symvalue.rangev.lower
	)
    );
}

/*
 * Test if two types match.
 * Equivalent names implies a match in any language.
 *
 * Special symbols must be handled with care.
 */

public Boolean compatible(t1, t2)
register Symbol t1, t2;
{
    Boolean b;
    Symbol rt1, rt2;

    if (t1 == t2) {
	b = true;
    } else if (t1 == nil or t2 == nil) {
	b = false;
    } else if (t1 == procsym) {
	b = isblock(t2);
    } else if (t2 == procsym) {
	b = isblock(t1);
    } else if (t1->language == nil) {
	if (t2->language == nil) {
	    b = false;
	} else if (t2->language == primlang) {
	    b = (boolean) primlang_typematch(rtype(t1), rtype(t2));
	} else {
	    b = (boolean) (*language_op(t2->language, L_TYPEMATCH))(t1, t2);
	}
    } else if (t1->language == primlang) {
	if (t2->language == primlang or t2->language == nil) {
	    b = primlang_typematch(rtype(t1), rtype(t2));
	} else {
	    b = (boolean) (*language_op(t2->language, L_TYPEMATCH))(t1, t2);
	}
    } else {
	b = (boolean) (*language_op(t1->language, L_TYPEMATCH))(t1, t2);
    }
    return b;
}

/*
 * Check for a type of the given name.
 */

public Boolean istypename(type, name)
Symbol type;
String name;
{
    register Symbol t;
    Boolean b;

    t = type;
    if (t == nil) {
	b = false;
    } else {
	b = (Boolean) (
	    t->class == TYPE and streq(ident(t->name), name)
	);
    }
    return b;
}

/*
 * Determine if a (value) parameter should actually be passed by address.
 */

public boolean passaddr (p, exprtype)
Symbol p, exprtype;
{
    boolean b;
    Language def;

    if (p == nil) {
	def = findlanguage(".c");
	b = (boolean) (*language_op(def, L_PASSADDR))(p, exprtype);
    } else if (p->language == nil or p->language == primlang) {
	b = false;
    } else if (isopenarray(p->type)) {
	b = true;
    } else {
	b = (boolean) (*language_op(p->language, L_PASSADDR))(p, exprtype);
    }
    return b;
}

/*
 * Test if the name of a symbol is uniquely defined or not.
 */

public Boolean isambiguous(s)
register Symbol s;
{
    register Symbol t;

    find(t, s->name) where t != s endfind(t);
    return (Boolean) (t != nil);
}

typedef char *Arglist;

#define nextarg(type)  ((type *) (ap += sizeof(type)))[-1]

private Symbol mkstring();

/*
 * Determine the type of a parse tree.
 *
 * Also make some symbol-dependent changes to the tree such as
 * removing indirection for constant or register symbols.
 */

public assigntypes (p)
register Node p;
{
    register Node p1;
    register Symbol s;

    switch (p->op) {
	case O_SYM:
	    p->nodetype = p->value.sym;
	    break;

	case O_LCON:
	    p->nodetype = t_int;
	    break;

	case O_CCON:
	    p->nodetype = t_char;
	    break;

	case O_FCON:
	    p->nodetype = t_real;
	    break;

	case O_SCON:
	    p->nodetype = mkstring(p->value.scon);
	    break;

	case O_INDIR:
	    p1 = p->value.arg[0];
	    s = rtype(p1->nodetype);
	    if (s->class != PTR) {
		beginerrmsg();
		fprintf(stderr, "\"");
		prtree(stderr, p1);
		fprintf(stderr, "\" is not a pointer");
		enderrmsg();
	    }
	    p->nodetype = rtype(p1->nodetype)->type;
	    break;

	case O_DOT:
	    p->nodetype = p->value.arg[1]->value.sym;
	    break;

	case O_RVAL:
	    p1 = p->value.arg[0];
	    p->nodetype = p1->nodetype;
	    if (p1->op == O_SYM) {
		if (p1->nodetype->class == PROC or p->nodetype->class == FUNC) {
		    p->op = p1->op;
		    p->value.sym = p1->value.sym;
		    p->nodetype = p1->nodetype;
		    dispose(p1);
		} else if (p1->value.sym->class == CONST) {
		    p->op = p1->op;
		    p->value = p1->value;
		    p->nodetype = p1->nodetype;
		    dispose(p1);
		} else if (isreg(p1->value.sym)) {
		    p->op = O_SYM;
		    p->value.sym = p1->value.sym;
		    dispose(p1);
		}
	    } else if (p1->op == O_INDIR and p1->value.arg[0]->op == O_SYM) {
		s = p1->value.arg[0]->value.sym;
		if (isreg(s)) {
		    p1->op = O_SYM;
		    dispose(p1->value.arg[0]);
		    p1->value.sym = s;
		    p1->nodetype = s;
		}
	    }
	    break;

	case O_COMMA:
	    p->nodetype = p->value.arg[0]->nodetype;
	    break;

	case O_CALLPROC:
	case O_CALL:
	    p1 = p->value.arg[0];
	    p->nodetype = rtype(p1->nodetype)->type;
	    break;

	case O_TYPERENAME:
	    p->nodetype = p->value.arg[1]->nodetype;
	    break;

	case O_ITOF:
	    p->nodetype = t_real;
	    break;

	case O_NEG:
	    s = p->value.arg[0]->nodetype;
	    if (not compatible(s, t_int)) {
		if (not compatible(s, t_real)) {
		    beginerrmsg();
		    fprintf(stderr, "\"");
		    prtree(stderr, p->value.arg[0]);
		    fprintf(stderr, "\" is improper type");
		    enderrmsg();
		} else {
		    p->op = O_NEGF;
		}
	    }
	    p->nodetype = s;
	    break;

	case O_ADD:
	case O_SUB:
	case O_MUL:
	    binaryop(p, nil);
	    break;

	case O_LT:
	case O_LE:
	case O_GT:
	case O_GE:
	case O_EQ:
	case O_NE:
	    binaryop(p, t_boolean);
	    break;

	case O_DIVF:
	    convert(&(p->value.arg[0]), t_real, O_ITOF);
	    convert(&(p->value.arg[1]), t_real, O_ITOF);
	    p->nodetype = t_real;
	    break;

	case O_DIV:
	case O_MOD:
	    convert(&(p->value.arg[0]), t_int, O_NOP);
	    convert(&(p->value.arg[1]), t_int, O_NOP);
	    p->nodetype = t_int;
	    break;

	case O_AND:
	case O_OR:
	    chkboolean(p->value.arg[0]);
	    chkboolean(p->value.arg[1]);
	    p->nodetype = t_boolean;
	    break;

	case O_QLINE:
	    p->nodetype = t_int;
	    break;

	default:
	    p->nodetype = nil;
	    break;
    }
}

/*
 * Process a binary arithmetic or relational operator.
 * Convert from integer to real if necessary.
 */

private binaryop (p, t)
Node p;
Symbol t;
{
    Node p1, p2;
    Boolean t1real, t2real;
    Symbol t1, t2;

    p1 = p->value.arg[0];
    p2 = p->value.arg[1];
    t1 = rtype(p1->nodetype);
    t2 = rtype(p2->nodetype);
    t1real = compatible(t1, t_real);
    t2real = compatible(t2, t_real);
    if (t1real or t2real) {
	p->op = (Operator) (ord(p->op) + 1);
	if (not t1real) {
	    p->value.arg[0] = build(O_ITOF, p1);
	} else if (not t2real) {
	    p->value.arg[1] = build(O_ITOF, p2);
	}
	p->nodetype = t_real;
    } else {
	if (size(p1->nodetype) > sizeof(integer)) {
	    beginerrmsg();
	    fprintf(stderr, "operation not defined on \"");
	    prtree(stderr, p1);
	    fprintf(stderr, "\"");
	    enderrmsg();
	} else if (size(p2->nodetype) > sizeof(integer)) {
	    beginerrmsg();
	    fprintf(stderr, "operation not defined on \"");
	    prtree(stderr, p2);
	    fprintf(stderr, "\"");
	    enderrmsg();
	}
	p->nodetype = t_int;
    }
    if (t != nil) {
	p->nodetype = t;
    }
}

/*
 * Convert a tree to a type via a conversion operator;
 * if this isn't possible generate an error.
 *
 * Note the tree is call by address, hence the #define below.
 */

private convert(tp, typeto, op)
Node *tp;
Symbol typeto;
Operator op;
{
    Node tree;
    Symbol s, t;

    tree = *tp;
    s = rtype(tree->nodetype);
    t = rtype(typeto);
    if (compatible(t, t_real) and compatible(s, t_int)) {
	tree = build(op, tree);
    } else if (not compatible(s, t)) {
	beginerrmsg();
	fprintf(stderr, "expected integer or real, found \"");
	prtree(stderr, tree);
	fprintf(stderr, "\"");
	enderrmsg();
    } else if (op != O_NOP and s != t) {
	tree = build(op, tree);
    }
    *tp = tree;
}

/*
 * Construct a node for the dot operator.
 *
 * If the left operand is not a record, but rather a procedure
 * or function, then we interpret the "." as referencing an
 * "invisible" variable; i.e. a variable within a dynamically
 * active block but not within the static scope of the current procedure.
 */

public Node dot(record, fieldname)
Node record;
Name fieldname;
{
    register Node rec, p;
    register Symbol s, t;

    rec = record;
    if (isblock(rec->nodetype)) {
	find(s, fieldname) where
	    s->block == rec->nodetype and
	    s->class != FIELD
	endfind(s);
	if (s == nil) {
	    beginerrmsg();
	    fprintf(stderr, "\"%s\" is not defined in ", ident(fieldname));
	    printname(stderr, rec->nodetype);
	    enderrmsg();
	}
	p = new(Node);
	p->op = O_SYM;
	p->value.sym = s;
	p->nodetype = s;
    } else {
	p = rec;
	t = rtype(p->nodetype);
	if (t->class == PTR) {
	    s = findfield(fieldname, t->type);
	} else {
	    s = findfield(fieldname, t);
	}
	if (s == nil) {
	    beginerrmsg();
	    fprintf(stderr, "\"%s\" is not a field in ", ident(fieldname));
	    prtree(stderr, rec);
	    enderrmsg();
	}
	if (t->class != PTR or isreg(rec->nodetype)) {
	    p = unrval(p);
	}
	p->nodetype = t_addr;
	p = build(O_DOT, p, build(O_SYM, s));
    }
    return build(O_RVAL, p);
}

/*
 * Return a tree corresponding to an array reference and do the
 * error checking.
 */

public Node subscript(a, slist)
Node a, slist;
{
    Symbol t;
    Node p;

    t = rtype(a->nodetype);
    if (t->language == nil or t->language == primlang) {
	p = (Node) (*language_op(findlanguage(".s"), L_BUILDAREF))(a, slist);
    } else {
	p = (Node) (*language_op(t->language, L_BUILDAREF))(a, slist);
    }
    return build(O_RVAL, p);
}

/*
 * Evaluate a subscript index.
 */

public int evalindex(s, base, i)
Symbol s;
Address base;
long i;
{
    Symbol t;
    int r;

    t = rtype(s);
    if (t->language == nil or t->language == primlang) {
	r = ((*language_op(findlanguage(".s"), L_EVALAREF)) (s, base, i));
    } else {
	r = ((*language_op(t->language, L_EVALAREF)) (s, base, i));
    }
    return r;
}

/*
 * Check to see if a tree is boolean-valued, if not it's an error.
 */

public chkboolean(p)
register Node p;
{
    if (p->nodetype != t_boolean) {
	beginerrmsg();
	fprintf(stderr, "found ");
	prtree(stderr, p);
	fprintf(stderr, ", expected boolean expression");
	enderrmsg();
    }
}

/*
 * Construct a node for the type of a string.
 */

private Symbol mkstring(str)
String str;
{
    register Symbol s;

    s = newSymbol(nil, 0, ARRAY, t_char, nil);
    s->chain = newSymbol(nil, 0, RANGE, t_int, nil);
    s->chain->language = s->language;
    s->chain->symvalue.rangev.lower = 1;
    s->chain->symvalue.rangev.upper = strlen(str) + 1;
    return s;
}

/*
 * Free up the space allocated for a string type.
 */

public unmkstring(s)
Symbol s;
{
    dispose(s->chain);
}

/*
 * Figure out the "current" variable or function being referred to
 * by the name n.
 */

private boolean stwhich(), dynwhich();

public Symbol which (n)
Name n;
{
    Symbol s;

    s = lookup(n);
    if (s == nil) {
	error("\"%s\" is not defined", ident(n));
    } else if (not stwhich(&s) and isambiguous(s) and not dynwhich(&s)) {
	printf("[using ");
	printname(stdout, s);
	printf("]\n");
    }
    return s;
}

/*
 * Static search.
 */

private boolean stwhich (var_s)
Symbol *var_s;
{
    Name n;		/* name of desired symbol */
    Symbol s;		/* iteration variable for symbols with name n */
    Symbol f;		/* iteration variable for blocks containing s */
    integer count;	/* number of levels from s->block to curfunc */
    Symbol t;		/* current best answer for stwhich(n) */
    integer mincount;	/* relative level for current best answer (t) */
    boolean b;		/* return value, true if symbol found */

    s = *var_s;
    n = s->name;
    t = s;
    mincount = 10000; /* force first match to set mincount */
    do {
	if (s->name == n and s->class != FIELD and s->class != TAG) {
	    f = curfunc;
	    count = 0;
	    while (f != nil and f != s->block) {
		++count;
		f = f->block;
	    }
	    if (f != nil and count < mincount) {
		t = s;
		mincount = count;
		b = true;
	    }
	}
	s = s->next_sym;
    } while (s != nil);
    if (mincount != 10000) {
	*var_s = t;
	b = true;
    } else {
	b = false;
    }
    return b;
}

/*
 * Dynamic search.
 */

private boolean dynwhich (var_s)
Symbol *var_s;
{
    Name n;		/* name of desired symbol */
    Symbol s;		/* iteration variable for possible symbols */
    Symbol f;		/* iteration variable for active functions */
    Frame frp;		/* frame associated with stack walk */
    boolean b;		/* return value */

    f = curfunc;
    frp = curfuncframe();
    n = (*var_s)->name;
    b = false;
    if (frp != nil) {
	frp = nextfunc(frp, &f);
	while (frp != nil) {
	    s = *var_s;
	    while (s != nil and
		(
		    s->name != n or s->block != f or
		    s->class == FIELD or s->class == TAG
		)
	    ) {
		s = s->next_sym;
	    }
	    if (s != nil) {
		*var_s = s;
		b = true;
		break;
	    }
	    if (f == program) {
		break;
	    }
	    frp = nextfunc(frp, &f);
	}
    }
    return b;
}

/*
 * Find the symbol that has the same name and scope as the
 * given symbol but is of the given field.  Return nil if there is none.
 */

public Symbol findfield (fieldname, record)
Name fieldname;
Symbol record;
{
    register Symbol t;

    t = rtype(record)->chain;
    while (t != nil and t->name != fieldname) {
	t = t->chain;
    }
    return t;
}

public Boolean getbound(s,off,type,valp)
Symbol s;
int off;
Rangetype type;
int *valp;
{
    Frame frp;
    Address addr;
    Symbol cur;

    if (not isactive(s->block)) {
	return(false);
    }
    cur = s->block;
    while (cur != nil and cur->class == MODULE) {  /* WHY*/
    		cur = cur->block;
    }
    if(cur == nil) {
		cur = whatblock(pc);
    }
    frp = findframe(cur);
    if (frp == nil) {
	return(false);
    }
    if(type == R_TEMP) addr = locals_base(frp) + off;
    else if (type == R_ARG) addr = args_base(frp) + off;
    else return(false);
    dread(valp,addr,sizeof(long));
    return(true);
}
