/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
static char sccsid[] = "@(#)stab.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

    /*
     *	Procedures to put out symbol table information
     *	and stabs for separate compilation type checking.
     *	These use the .stabs, .stabn, and .stabd directives.
     */

#include	"whoami.h"
#ifdef	PC
    /*	and the rest of the file */
#   include	"0.h"
#   include	"objfmt.h"
#   include	"yy.h"
#   include	<stab.h>

    /*
     *  additional symbol definition for <stab.h>
     *	that is used by the separate compilation facility --
     *	eventually, <stab.h> should be updated to include this 
     */

#   include	"pstab.h"
#   include	"pc.h"


#define private static

int oldway = 0;

    /*
     *	absolute value: line numbers are negative if error recovery.
     */
#define	ABS( x )	( x < 0 ? -x : x )
long checksum();

/*
 * Generate information about variables.
 */

stabgvar (p, length, line)
struct nl *p;
int length, line;
{
    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x",
	0, p->symbol, N_PC, N_PGVAR, ABS(line)
    );
    if (oldway != 0) {
	oldstabgvar(p->symbol, p2type(p->type), 0, length, line);
    } else if (opt('g')) {
	putprintf("\t.stabs\t\"%s:G", 1, p->symbol);
	gentype(p->type);
	putprintf("\",0x%x,0,0x%x,0", 0, N_GSYM, length);
    }
}

stablvar (p, offset, length)
struct nl *p;
int offset, length;
{
    int level;

    level = (p->nl_block & 037);
    if (oldway != 0) {
	oldstablvar(p->symbol, p2type(p->type), level, offset, length);
    } else if (opt('g')) {
	putprintf("\t.stabs\t\"%s:", 1, p->symbol);
	gentype(p->type);
	putprintf("\",0x%x,0,0x%x,0x%x", 0, N_LSYM, length, offset);
    }
}

    /*
     *	global variables
     */
/*ARGSUSED*/
oldstabgvar( name , type , offset , length , line )
    char	*name;
    int		type;
    int		offset;
    int		length;
    int		line;
    {
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0x%x,0" , 0 , N_GSYM , type );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
}

    /*
     *	local variables
     */
/*ARGSUSED*/
oldstablvar( name , type , level , offset , length )
    char	*name;
    int		type;
    int		level;
    int		offset;
    int		length;
    {

	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0x%x,0x%x" , 0 , N_LSYM , type , -offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
}


stabparam (p, offset, length)
struct nl *p;
int offset, length;
{
    if (oldway != 0) {
	oldstabparam(p->symbol, p2type(p->type), offset, length);
    } else if (opt('g')) {
	putprintf("\t.stabs\t\"%s:", 1, p->symbol);
	if (p->class == REF) {
	    putprintf("v", 1);
	} else {
	    putprintf("p", 1);
	}
	gentype((p->class == FPROC || p->class ==FFUNC) ? p : p->type);
	putprintf("\",0x%x,0,0x%x,0x%x", 0, N_PSYM, length, offset);
    }
}

    /*
     *	parameters
     */
oldstabparam( name , type , offset , length )
    char	*name;
    int		type;
    int		offset;
    int		length;
    {
	
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0x%x,0x%x" , 0 , N_PSYM , type , offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
    }

    /*
     *	fields
     */

    /*
     *	left brackets
     *  (dbx handles module-2 without these, so we won't use them either)
     */
stablbrac( level )
    int	level;
    {

	if ( ! opt('g') || oldway == 0 ) {
		return;
	}
	putprintf( "	.stabd	0x%x,0,0x%x" , 0 , N_LBRAC , level );
    }

    /*
     *	right brackets
     */
stabrbrac( level )
    int	level;
    {

	if ( ! opt('g') || oldway == 0 ) {
		return;
	}
	putprintf( "	.stabd	0x%x,0,0x%x" , 0 , N_RBRAC , level );
    }

stabfunc (p, name, line, level)
struct nl *p;
char *name;
int line, level;
{
    char extname[BUFSIZ],nestspec[BUFSIZ];
    
    if ( level == 1 ) {
	if (p->class == FUNC) {
	    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x" ,
		0 , name , N_PC , N_PGFUNC , ABS( line )
	    );
	} else if (p->class == PROC) {
	    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x" ,
		0 , name , N_PC , N_PGPROC , ABS( line )
	    );
	}
    }
    if (oldway != 0) {
	oldstabfunc(name, p->class, line, level);
    } else if (opt('g')) {
	putprintf("\t.stabs\t\"%s:", 1, name);
	if (p->class == FUNC) {
	    putprintf("F", 1);
	    gentype(p->type);
	    putprintf(",", 1);
	} else {
	    putprintf("P,", 1);
	}
	sextname(extname, name, level);  /* set extname to entry label */
	putprintf("%s,", 1, &(extname[1])); /* remove initial underbar */
	snestspec(nestspec, level);
	putprintf("%s\",0x%x,0,0,%s", 0, nestspec, N_FUN, extname);
    }
}

    /*
     * construct the colon-separated static nesting string into a
     * caller-supplied buffer
     */
private snestspec(buffer, level)
    char buffer[];
    int level;
{
    char *starthere;
    int i;
    
    if (level <= 1) {
	buffer[0] = '\0';
    } else {
	starthere = &buffer[0];
	for ( i = 1 ; i < level ; i++ ) {
	    sprintf(starthere, "%s:", enclosing[i]);
	    starthere += strlen(enclosing[i]) + 1;
	}
	*--starthere = '\0'; /* remove last colon */
	if (starthere >= &buffer[BUFSIZ-1]) {
	    panic("snestspec");
	}
    }
}

    /*
     *	functions
     */
oldstabfunc( name , typeclass , line , level )
    char	*name;
    int		typeclass;
    int		line;
    long	level;
    {
	char	extname[ BUFSIZ ];

	    /*
	     *	for sdb
	     */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , (int) name );
	sextname( extname , name , (int) level );
	putprintf( "\",0x%x,0,0x%x,%s" , 0 , N_FUN , line , (int) extname );
    }

    /*
     *	source line numbers
     */
stabline( line )
    int	line;
    {
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabd	0x%x,0,0x%x" , 0 , N_SLINE , ABS( line ) );
    }

    /*
     *	source files get none or more of these:
     *  one as they are entered,
     *  and one every time they are returned to from nested #includes
     */
stabsource(filename, firsttime)
    char	*filename;
    bool	firsttime;
{
    int		label;
    
	/*
	 *	for separate compilation
	 */
    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x", 0,
	    (int) filename, N_PC, N_PSO, N_FLAGCHECKSUM);
	/*
	 *	for debugger
	 */
    if ( ! opt('g') ) {
	    return;
    }
    if (oldway != 0) {
	label = (int) getlab();
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0x%x,0,0," , 1 , N_SO );
	putprintf( PREFIXFORMAT , 0 , LLABELPREFIX , label );
	putprintf( PREFIXFORMAT , 1 , LLABELPREFIX , label );
	putprintf( ":" , 0 );
    } else {
	if (firsttime) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( NAMEFORMAT , 1 , filename );
	    putprintf( "\",0x%x,0,0,0" , 0 , N_SO );
	}
    }
}

    /*
     *	included files get one or more of these:
     *	one as they are entered by a #include,
     *	and one every time they are returned to from nested #includes.
     */
stabinclude(filename, firsttime)
    char	*filename;
    bool	firsttime;
{
    int		label;
    long	check;
    
	/*
	 *	for separate compilation
	 */
    if (firsttime) {
	check = checksum(filename);
    } else {
	check = N_FLAGCHECKSUM;
    }
    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x", 0,
	    (int) filename, N_PC, N_PSOL, check);
	/*
	 *	for sdb
	 */
    if ( ! opt('g') ) {
	    return;
    }
    if (oldway != 0) {
	label = (int) getlab();
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0x%x,0,0," , 1 , N_SOL );
	putprintf( PREFIXFORMAT , 0 , LLABELPREFIX , label );
	putprintf( PREFIXFORMAT , 1 , LLABELPREFIX , label );
	putprintf( ":" , 0 );
    }
}

    /*
     *	anyone know a good checksum for ascii files?
     *	this does a rotate-left and then exclusive-or's in the character.
     *	also, it avoids returning checksums of 0.
     *	The rotate is implemented by shifting and adding back the
     *	sign bit when negative.
     */
long
checksum(filename)
    char	*filename;
{
    FILE		*filep;
    register int	input;
    register long	check;

    filep = fopen(filename, "r");
    if (filep == NULL) {
	perror(filename);
	pexit(DIED);
    }
    check = 0;
    while ((input = getc(filep)) != EOF) {
	if (check < 0) {
	    check <<= 1;
	    check += 1;
	} else {
	    check <<= 1;
	}
	check ^= input;
    }
    (void) fclose(filep);
    if ((unsigned) check <= N_FLAGCHECKSUM) {
	return N_FLAGCHECKSUM + 1;
    } else {
	return check;
    }
}

/*
 * global Pascal symbols :
 *   labels, types, constants, and external procedure and function names:
 *   These are used by the separate compilation facility
 *   to be able to check for disjoint header files.
 */

    /*
     *	global labels
     */
stabglabel( label , line )
    char	*label;
    int		line;
    {

	putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
		    , (int) label , N_PC , N_PGLABEL , ABS( line ) );
    }

    /*
     *	global constants
     */
stabgconst( constant , line )
    char	*constant;
    int		line;
    {

	    putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
			, (int) constant , N_PC , N_PGCONST , ABS( line ) );
    }

/*
 * Generate symbolic information about a constant.
 */

stabconst (c)
struct nl *c;
{
    if (opt('g') && oldway == 0) {
	putprintf("\t.stabs\t\"%s:c=", 1, c->symbol);
	if (c->type == nl + TSTR) {
	    putprintf("s'%s'", 1, c->ptr[0]);
	} else if (c->type == nl + T1CHAR) {
	    putprintf("c%d", 1, c->range[0]);
	} else if (isa(c->type, "i")) {
	    putprintf("i%d", 1, c->range[0]);
	} else if (isa(c->type, "d")) {
	    putprintf("r%g", 1, c->real);
	} else {
	    putprintf("e", 1);
	    gentype(c->type);
	    putprintf(",%d", 1, c->range[0]);
	}
	putprintf("\",0x%x,0,0x%x,0x%x", 0, N_LSYM, 0, 0);
    }
}

stabgtype (name, type, line)
char *name;
struct nl *type;
int line;
{
    putprintf("	.stabs	\"%s\",0x%x,0,0x%x,0x%x" ,
	0, name, N_PC , N_PGTYPE, ABS(line)
    );
    if (oldway == 0) {
	stabltype(name, type);
    }
}

stabltype (name, type)
char *name;
struct nl *type;
{
    if (opt('g')) {
	putprintf("\t.stabs\t\"%s:t", 1, name);
	gentype(type);
	putprintf("\",0x%x,0,0,0", 0, N_LSYM);
    }
}

    /*
     *	external functions and procedures
     */	
stabefunc( name , typeclass , line )
    char	*name;
    int		typeclass;
    int		line;
    {
	int	type;

	if ( typeclass == FUNC ) {
	    type = N_PEFUNC;
	} else if ( typeclass == PROC ) {
	    type = N_PEPROC;
	} else {
	    return;
	}
	putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
		    , (int) name , N_PC , type , ABS( line ) );
    }

/*
 * Generate type information encoded as a string for dbx.
 * The fwdptrnum field is used only when the type is a pointer
 * to a type that isn't known when it was entered.  When the
 * type field is filled for some such tptr, fixfwdtype should
 * be called to output an equivalencing type definition.
 */

typedef struct TypeDesc *TypeDesc;

struct TypeDesc {
    struct nl *tptr;
    int tnum;
    int fwdptrnum;
    TypeDesc chain;
};

#define TABLESIZE 2003

#define typehash(t) ( ( ((int) t) >> 2 ) % TABLESIZE )

private int tcount = 1;
private TypeDesc typetable[TABLESIZE];

private TypeDesc tdlookup (t)
struct nl *t;
{
    register TypeDesc td;

    td = typetable[typehash(t)];
    while (td != NIL && td->tptr != t) {
	td = td->chain;
    }
    return td;
}

private int typelookup (t)
struct nl *t;
{
    register TypeDesc td;
    int r;

    td = tdlookup(t);
    if (td == NIL) {
	r = 0;
    } else {
	r = td->tnum;
    }
    return r;
}

private int entertype (type)
struct nl *type;
{
    register TypeDesc td;
    register int i;

    td = (TypeDesc) malloc(sizeof(struct TypeDesc));
    td->tptr = type;
    td->tnum = tcount;
    td->fwdptrnum = 0;
    ++tcount;
    i = typehash(type);
    td->chain = typetable[i];
    typetable[i] = td;
    return td->tnum;
}

/*
 * The in_types table currently contains "boolean", "char", "integer",
 * "real" and "_nil".  (See nl.c for definition.)
 * The lookup call below will give the TYPE class nl entry for these
 * types.  In each case except _nil, the type field of that entry is a RANGE
 * class nl entry for the type.  Sometimes other symbol table entries
 * point to the TYPE entry (e.g., when there is a range over the base type),
 * and other entries point to the RANGE entry (e.g., for a variable of the
 * given type).  We don't really want to distinguish between these uses
 * in dbx, and since it appears that the RANGE entries are not reused if
 * a range happens to coincide, we will give the two the same identifying
 * dbx type number.
 */

private inittypes()
{
    int i;
    extern char *in_types[];
    struct nl *p;

    for (i = 0; in_types[i] != NIL; i++) {
	p = lookup(in_types[i]);
	if (p != NIL) {
	    entertype(p);
	    if (p->type != NIL) {
		--tcount; /* see comment above */
		entertype(p->type);
	    }
	}
    }
}

static genarray (t)
struct nl *t;
{
    register struct nl *p;

    for (p = t->chain; p != NIL; p = p->chain) {
	putprintf("a", 1);
	gentype(p);
	putprintf(";", 1);
    }
    gentype(t->type);
}

/*
 * Really we should walk through ptr[NL_FIELDLIST] for the fields,
 * and then do the variant tag and fields separately, but dbx
 * doesn't support this yet.
 * So, since all the fields of all the variants are on the chain,
 * we walk through that.  Except that this gives the fields in the
 * reverse order, so we want to print in reverse order.
 */

static genrecord (t)
struct nl *t;
{
    putprintf("s%d", 1, t->value[NL_OFFS]);
    if (t->chain != NIL) {
	genrecfield(t->chain, 1);
    }
    putprintf(";", 1);
}

static genrecfield (t, n)
struct nl *t;
int n;
{
    if (t->chain != NULL) {
	genrecfield(t->chain, n + 1);
	if (n % 2 == 0) {
	    gencontinue();
	}
    }
    putprintf("%s:", 1, t->symbol);
    gentype(t->type);
    putprintf(",%d,%d;", 1, 8*t->value[NL_OFFS], 8*lwidth(t->type));
}

static genvarnt (t)
struct nl *t;
{
    genrecord(t);
}

static genptr (t)
struct nl *t;
{
    register TypeDesc td;
    
    putprintf("*", 1);
    if (t->type != NIL) {
	gentype(t->type);
    } else {
	/*
	 * unresolved forward pointer: use tcount to represent what is
         * begin pointed to, to be defined later
	 */
	td = tdlookup(t);
	if (td == NIL) {
	    panic("nil ptr in stab.genptr");
	}
	td->fwdptrnum = tcount;
	putprintf("%d", 1, tcount);
	++tcount;
    }
}

/*
 * The type t is a pointer which has just had its type field filled.
 * We need to generate a type stab saying that the number saved
 * in t's fwdptrnum is the same as the t->type's number
 */

fixfwdtype (t)
struct nl *t;
{
    register TypeDesc td;
    
    if (opt('g') && oldway == 0) {
	td = tdlookup(t);
	if (td != NIL) {
	    putprintf("\t.stabs\t\":t%d=", 1, td->fwdptrnum);
	    gentype(t->type);
	    putprintf("\",0x%x,0,0,0", 0, N_LSYM);
	}
    }
}

static genenum (t)
struct nl *t;
{
    register struct nl *e;
    register int i;

    putprintf("e", 1);
    i = 1;
    e = t->chain;
    while (e != NULL) {
	if (i > 2) {
	    gencontinue();
	    i = 0;
	}
	putprintf("%s:%d,", 1, e->symbol, e->range[0]);
	e = e->chain;
	++i;
    }
    putprintf(";", 1);
}

static genset (t)
struct nl *t;
{
    putprintf("S", 1);
    gentype(t->type);
}

static genrange (t)
struct nl *t;
{
    putprintf("r", 1);
    gentype(t->type);
    putprintf(";%d;%d", 1, t->range[0], t->range[1]);
}

static genfparam (t)
struct nl *t;
{
    struct nl *p;
    int count;
    
    if (t->type != NULL) {
	putprintf("f", 1);
	gentype(t->type);
	putprintf(",", 1);
    } else {
	putprintf("p", 1);
    }
    count = 0;
    for (p = t->ptr[NL_FCHAIN]; p != NULL; p = p->chain) {
	++count;
    }
    putprintf("%d;", 1, count);
    for (p = t->ptr[NL_FCHAIN]; p != NULL; p = p->chain) {
	gentype(p->type);
	putprintf(",%d;", 1, p->class);
    }
}

static genfile (t)
struct nl *t;
{
    putprintf("d", 1);
    gentype(t->type);
}

static gentype (t)
struct nl *t;
{
    int id;

    if (tcount == 1) {
	inittypes();
    }
    id = typelookup(t);
    if (id != 0) {
	putprintf("%d", 1, id);
    } else if (t->class == SCAL && t->chain == NULL) {
	id = typelookup(t->type);
	if (id != 0) {
	    putprintf("%d", 1, id);
	} else {
	    genenum(t->type);
	}
    } else {
	id = entertype(t);
	putprintf("%d=", 1, id);
	switch (t->class) {
	    case TYPE:
		gentype(t->type);
		break;

	    case ARRAY:
		genarray(t);
		break;

	    case RECORD:
		genrecord(t);
		break;

	    case VARNT:
		genvarnt(t);
		break;

	    case REF:
		gentype(t->type);
		break;

	    case PTR:
		genptr(t);
		break;

	    case SET:
		genset(t);
		break;

	    case RANGE:
		genrange(t);
		break;

	    case SCAL:
		genenum(t);
		break;

	    case FPROC:
	    case FFUNC:
		genfparam(t);
		break;

	    case FILET:
	    case PTRFILE:
		genfile(t);
		break;

	    default:
		/* This shouldn't happen */
		/* Rather than bomb outright, let debugging go on */
		warning();
		error("Bad type class found in stab");
		putprintf("1", 1, t->class);
		break;
	}
    }
}

/*
 * Continue stab information in a namelist new entry.  This is necessary
 * to avoid overflowing putprintf's buffer.
 */

static gencontinue ()
{
    putprintf("?\",0x%x,0,0,0", 0, N_LSYM);
    putprintf("\t.stabs\t\"", 1);
}

#endif PC
