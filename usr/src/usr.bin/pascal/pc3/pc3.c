/*-
 * Copyright (c) 1980, 1982, 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1982, 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pc3.c	5.2 (Berkeley) %G%";
#endif /* not lint */

    /*
     *	     Pc3 is a pass in the Berkeley Pascal compilation
     *	process that is performed just prior to linking Pascal
     *	object files.  Its purpose is to enforce the rules of
     *	separate compilation for Berkeley Pascal.  Pc3 is called
     *	with the same argument list of object files that is sent to
     *	the loader.  These checks are performed by pc3 by examining
     *	the symbol tables of the object files:
     *	(1)  All .o files must be up to date with respect to the
     *	     runtime libraries.
     *	(2)  Each global Pascal symbol (label, constant, type,
     *	     variable, procedure, or function name) must be uniquely
     *	     declared, i.e. declared in only one included file or
     *	     source file.
     *	(3)  Each external function (or procedure) may be resolved
     *	     at most once in a source file which included the
     *	     external declaration of the function.
     *	
     *	     The symbol table of each object file is scanned and
     *	each global Pascal symbol is placed in a hashed symbol
     *	table.  The Pascal compiler has been modified to emit all
     *	Pascal global symbols to the object file symbol table.  The
     *	information stored in the symbol table for each such symbol
     *	is:
     *	
     *	   - the name of the symbol;
     *	   - a subtype descriptor;
     *	   - the file which logically contains the declaration of
     *	     the symbol or which caused the inclusion of an include file.
     *	   - for included files:
     *		- a checksum;
     *	   - for symbols:
     *	   	- the file which textually contains the declaration of
     *	   	  the symbol (possibly an include file);
     *	   	- the line number at which the symbol is declared;
     *	   	- the file which contains the resolution of the symbol.
     *	   	- the line number at which the symbol is resolved;
     *	
     *	     If a symbol has been previously entered into the symbol
     *	table, a check is made that the current declaration is of
     *	the same type and from the same include file as the previous
     *	one.  Except for files and functions and procedures, it is
     *	an error for a symbol declaration to be encountered more
     *	than once, unless the re-declarations come from the same
     *	included file as the original.
     *	
     *	     As an include file symbol is encountered in a source
     *	file, the symbol table entry of each symbol declared in that
     *	include file is modified to reflect its new logical
     *	inclusion in the source file.  File symbols are also
     *	encountered as an included file ends, signaling the
     *	continuation of the enclosing file.
     *	
     *	     Functions and procedures which have been declared
     *	external may be resolved by declarations from source files
     *	which included the external declaration of the function.
     *	Functions and procedures may be resolved at most once across
     *	a set of object files.  The loader will complain if a
     *	function is not resolved at least once.
     */

char	program[] = "pc";

#include <sys/types.h>
#include <sys/stat.h>
#include <ar.h>
#include <stdio.h>
#include <ctype.h>
#include <a.out.h>
#include <stab.h>
#include "pstab.h"
#include "pc3.h"

int	errors = NONE;
BOOL	wflag = FALSE;	

    /*
     *	check each of the argument .o files (or archives of .o files).
     */
main( argc , argv )
    int		argc;
    char	**argv;
    {
	struct fileinfo	ofile;

	for ( argv++ ; *argv != 0 && **argv == '-' ; argv++ ) {
	    (*argv)++;
	    switch ( **argv ) {
		default:
		    error( FATAL , "pc3: bad flag -%c\n" , **argv );
		case 'w':
		    wflag = TRUE;
		    break;
	    }
	}
	for ( /* void */ ; *argv != 0 ; argv++ ) {
#	    ifdef DEBUG
		fprintf( stderr , "[main] *argv = %s\n" , *argv );
#	    endif DEBUG
	    ofile.name = *argv;
	    checkfile( &ofile );
	}
	exit( errors );
    }

    /*
     *	check the namelist of a file, or all namelists of an archive.
     */
checkfile( ofilep )
    struct fileinfo	*ofilep;
    {
	union {
	    char	mag_armag[ SARMAG + 1 ];
	    struct exec	mag_exec;
	}		mag_un;
	int		red;
	struct stat	filestat;

	ofilep -> file = fopen( ofilep -> name , "r" );
	if ( ofilep -> file == NULL ) {
	    error( ERROR , "cannot open: %s" , ofilep -> name );
	    return;
	}
	fstat( fileno( ofilep -> file ) , &filestat );
	red = fread( (char *) &mag_un , 1 , sizeof mag_un , ofilep -> file );
	if ( red != sizeof mag_un ) {
	    error( ERROR , "cannot read header: %s" , ofilep -> name );
	    return;
	}
	if ( mag_un.mag_exec.a_magic == OARMAG ) {
	    error( WARNING , "old archive: %s" , ofilep -> name );
	    return;
	}
	if ( strncmp( mag_un.mag_armag , ARMAG , SARMAG ) == 0 ) {
		/* archive, iterate through elements */
#	    ifdef DEBUG
		fprintf( stderr , "[checkfile] archive %s\n" , ofilep -> name );
#	    endif DEBUG
	    ofilep -> nextoffset = SARMAG;
	    while ( nextelement( ofilep ) ) {
		checknl( ofilep );
	    }
	} else if ( N_BADMAG( mag_un.mag_exec ) ) {
		/* not a file.o */
	    error( ERROR , "bad format: %s" , ofilep -> name );
	    return;
	} else {
		/* a file.o */
#	    ifdef DEBUG
		fprintf( stderr , "[checkfile] .o file %s\n" , ofilep -> name );
#	    endif DEBUG
	    fseek( ofilep -> file , 0L , 0 );
	    ofilep -> nextoffset = filestat.st_size;
	    checknl( ofilep );
	}
	fclose( ofilep -> file );
    }

    /*
     *	check the namelist of this file for conflicts with
     *	previously entered symbols.
     */
checknl( ofilep )
    register struct fileinfo	*ofilep;
    {
    
	long			red;
	struct exec		oexec;
	off_t			symoff;
	long			numsyms;
	register struct nlist	*nlp;
	register char		*stringp;
	long			strsize;
	long			sym;

	red = fread( (char *) &oexec , 1 , sizeof oexec , ofilep -> file );
	if ( red != sizeof oexec ) {
	    error( ERROR , "error reading struct exec: %s"
		    , ofilep -> name );
	    return;
	}
	if ( N_BADMAG( oexec ) ) {
	    return;
	}
	symoff = N_SYMOFF( oexec ) - sizeof oexec;
	fseek( ofilep -> file , symoff , 1 );
	numsyms = oexec.a_syms / sizeof ( struct nlist );
	if ( numsyms == 0 ) {
	    error( WARNING , "no name list: %s" , ofilep -> name );
	    return;
	}
	nlp = (struct nlist *) calloc( numsyms , sizeof ( struct nlist ) );
	if ( nlp == 0 ) {
	    error( FATAL , "no room for %d nlists" , numsyms );
	}
	red = fread( ( char * ) nlp , numsyms , sizeof ( struct nlist )
		    , ofilep -> file );
	if (   ftell( ofilep -> file ) + sizeof ( off_t )
	    >= ofilep -> nextoffset ) {
	    error( WARNING , "no string table (old format .o?)"
		    , ofilep -> name );
	    return;
	}
	red = fread( (char *) &strsize , sizeof strsize , 1
		    , ofilep -> file );
	if ( red != 1 ) {
	    error( WARNING , "no string table (old format .o?)"
		    , ofilep -> name );
	    return;
	}
	stringp  = ( char * ) malloc( strsize );
	if ( stringp == 0 ) {
	    error( FATAL , "no room for %d bytes of strings" , strsize );
	}
	red = fread( stringp + sizeof strsize
		    , strsize - sizeof ( strsize ) , 1 , ofilep -> file );
	if ( red != 1 ) {
	    error( WARNING , "error reading string table: %s"
		    , ofilep -> name );
	}
#	ifdef DEBUG
	    fprintf( stderr , "[checknl] %s: %d symbols\n"
		    , ofilep -> name , numsyms );
#	endif DEBUG
	for ( sym = 0 ; sym < numsyms ; sym++) {
	    if ( nlp[ sym ].n_un.n_strx ) {
		nlp[ sym ].n_un.n_name = stringp + nlp[ sym ].n_un.n_strx;
	    } else {
		nlp[ sym ].n_un.n_name = "";
	    }
	    checksymbol( &nlp[ sym ] , ofilep );
	}
	if ( nlp ) {
	    free( nlp );
	}
	if ( stringp ) {
	    free( stringp );
	}
    }

    /*
     *	check a symbol.
     *	look it up in the hashed symbol table,
     *	entering it if necessary.
     *	this maintains a state of which .p and .i files
     *	it is currently in the midst from the nlist entries
     *	for source and included files.
     *	if we are inside a .p but not a .i, pfilep == ifilep.
     */
checksymbol( nlp , ofilep )
    struct nlist	*nlp;
    struct fileinfo	*ofilep;
    {
	static struct symbol	*pfilep = NIL;
	static struct symbol	*ifilep = NIL;
	register struct symbol	*symbolp;
	int			errtype;

#	ifdef DEBUG
	    if ( pfilep && ifilep ) {
		fprintf( stderr , "[checksymbol] pfile %s ifile %s\n"
			, pfilep -> name , ifilep -> name );
	    }
	    fprintf( stderr , "[checksymbol] ->name %s ->n_desc %x (%s)\n"
		    , nlp -> n_un.n_name , nlp -> n_desc
		    , classify( nlp -> n_desc ) );
#	endif DEBUG
	if ( nlp -> n_type != N_PC ) {
		/* don't care about the others */
	    return;
	}
	symbolp = entersymbol( nlp -> n_un.n_name );
	if ( symbolp -> lookup == NEW ) {
#	    ifdef DEBUG
		fprintf( stderr , "[checksymbol] ->name %s is NEW\n"
			, symbolp -> name );
#	    endif DEBUG
	    symbolp -> desc = nlp -> n_desc;
	    symbolp -> fromp = pfilep;
	    switch ( symbolp -> desc ) {
		default:
			error( FATAL , "panic: [checksymbol] NEW" );
		case N_PGLABEL:
		case N_PGCONST:
		case N_PGTYPE:
		case N_PGVAR:
		case N_PGFUNC:
		case N_PGPROC:
		case N_PLDATA:
		case N_PLTEXT:
			symbolp -> sym_un.sym_str.rfilep = ifilep;
			symbolp -> sym_un.sym_str.rline = nlp -> n_value;
			symbolp -> sym_un.sym_str.fromi = ifilep;
			symbolp -> sym_un.sym_str.iline = nlp -> n_value;
			return;
		case N_PEFUNC:
		case N_PEPROC:
			symbolp -> sym_un.sym_str.rfilep = NIL;
			symbolp -> sym_un.sym_str.rline = 0;
			    /*
			     *	functions can only be declared external
			     *	in included files.
			     */
			if ( pfilep == ifilep ) {
			    error( WARNING
				    , "%s, line %d: %s %s must be declared in included file"
				    , pfilep -> name , nlp -> n_value
				    , classify( symbolp -> desc )
				    , symbolp -> name );
			}
			symbolp -> sym_un.sym_str.fromi = ifilep;
			symbolp -> sym_un.sym_str.iline = nlp -> n_value;
			return;
		case N_PSO:
			if ( nlp -> n_value < N_FLAGCHECKSUM ) {
			    error( WARNING,
				"%s is out of date and should be recompiled",
				ofilep -> name );
			}
			pfilep = symbolp;
			ifilep = symbolp;
			symbolp -> sym_un.checksum = N_FLAGCHECKSUM;
			return;
		case N_PSOL:
			ifilep = symbolp;
			symbolp -> sym_un.checksum = nlp -> n_value;
			return;
	    }
	} else {
#	    ifdef DEBUG
		fprintf( stderr , "[checksymbol] ->name %s is OLD\n"
			, symbolp -> name );
#	    endif DEBUG
	    errtype = ERROR;
	    switch ( symbolp -> desc ) {
		default:
			error( FATAL , "panic [checksymbol] OLD" );
			return;
		case N_PSO:
			    /*
			     *	finding a file again means you are back
			     *	in it after finishing an include file.
			     */
			if ( symbolp -> desc != nlp -> n_desc ) {
			    error( FATAL , "panic [checksymbol] PSO" );
			    return;
			}
			pfilep = symbolp;
			ifilep = symbolp;
			return;
		case N_PSOL:
			    /*
			     *	include files can be seen more than once,
			     *	but their checksums are checked if they are
			     *	greater than N_FLAGCHECKSUM.
			     *	PSOL's are seen with checksums as the
			     *	include file is entered, and with
			     *	N_FLAGCHECKSUM as we are back in an
			     *	included file from a nested include.
			     */
			if ( symbolp -> desc != nlp -> n_desc ) {
			    error( FATAL , "panic [checksymbol] PSOL" );
			    return;
			}
			if ((unsigned) symbolp->sym_un.checksum > N_FLAGCHECKSUM
			   && (unsigned) nlp -> n_value > N_FLAGCHECKSUM
			   && symbolp -> sym_un.checksum != nlp -> n_value ) {
			    error( ERROR,
			    "%s included in %s differs from %s included in %s",
				symbolp -> name, pfilep -> name,
				symbolp -> name, symbolp -> fromp -> name );
			}
			ifilep = symbolp;
			return;
		case N_PEFUNC:
		case N_PEPROC:
			    /*
			     *	this might be the resolution of the external
			     *	has to match func/proc of external
			     *	and has to have included external
			     *	and has to not have been previously resolved.
			     */
			if (  (  ( symbolp -> desc == N_PEFUNC
			         && nlp -> n_desc == N_PGFUNC )
			      || ( symbolp -> desc == N_PEPROC
				 && nlp -> n_desc == N_PGPROC ) )
			   && ( symbolp -> fromp == pfilep )
			   && ( symbolp -> sym_un.sym_str.rfilep == NIL ) ) {
				/*
				 *	resolve external
				 */
#			    ifdef DEBUG
				fprintf( stderr , "[checksymbol] resolving external\n" );
#			    endif DEBUG
			    symbolp -> sym_un.sym_str.rfilep = ifilep;
			    symbolp -> sym_un.sym_str.rline = nlp -> n_value;
			    return;
			}
			    /*
			     *	otherwise, it might be another external,
			     *	which is okay if it's
			     *	the same type and from the same include file
			     */
			if (  (  ( symbolp -> desc == N_PEFUNC
			         && nlp -> n_desc == N_PEFUNC )
			      || ( symbolp -> desc == N_PEPROC
				 && nlp -> n_desc == N_PEPROC ) )
			   && ( symbolp -> sym_un.sym_str.fromi == ifilep ) ) {
				/*
				 *	just another pretty external
				 *	make it look like it comes from here.
				 */
#			    ifdef DEBUG
				fprintf( stderr , "[checksymbol] just another pretty external\n" );
#			    endif DEBUG
			    symbolp -> fromp = pfilep;
			    return;
			}
			    /*
			     *	something is wrong
			     *	if it's not resolved, use the header file
			     *	otherwise, it's just a regular error
			     */
			if ( symbolp -> sym_un.sym_str.rfilep == NIL ) {
			    error( ERROR ,
		    "%s, line %d: %s is already defined\n\t(%s, line %d)." ,
				ifilep -> name , nlp -> n_value , 
				nlp -> n_un.n_name , 
				symbolp -> sym_un.sym_str.fromi -> name ,
				symbolp -> sym_un.sym_str.iline );
			    return;
			}
			break;
		case N_PGFUNC:
		case N_PGPROC:
			    /*
			     *	functions may not be seen more than once.
			     *	the loader will complain about
			     *	`multiply defined', but we can, too.
			     */
			break;
		case N_PGLABEL:
		case N_PGCONST:
		case N_PGTYPE:
		case N_PGVAR:
			    /*
			     *	labels, constants, types, variables
			     *	and external declarations
			     *	may be seen as many times as they want,
			     *	as long as they come from the same include file.
			     *	make it look like they come from this .p file.
			     */
included:
			if (  nlp -> n_desc != symbolp -> desc
			   || symbolp -> sym_un.sym_str.fromi != ifilep ) {
			    break;
			}
			symbolp -> fromp = pfilep;
			return;
		case N_PLDATA:
		case N_PLTEXT:
			switch ( nlp -> n_desc ) {
			    default:
				error( FATAL , "pc3: unknown stab 0x%x"
					, nlp -> n_desc );
				return;
			    case N_PSO:
			    case N_PSOL:
			    case N_PGCONST:
			    case N_PGTYPE:
				/* these won't conflict with library */
				return;
			    case N_PGLABEL:
			    case N_PGVAR:
			    case N_PGFUNC:
			    case N_PGPROC:
			    case N_PEFUNC:
			    case N_PEPROC:
			    case N_PLDATA:
			    case N_PLTEXT:
				errtype = WARNING;
				break;
			}
			break;
	    }
		/*
		 *	this is the breaks
		 */
	    error( errtype
		, "%s, line %d: %s %s is already defined\n\t%s%s (%s, line %d)."
		, ifilep -> name
		, nlp -> n_value
		, classify( nlp -> n_desc )
		, nlp -> n_un.n_name
		, ( symbolp -> desc == nlp -> n_desc ? "" : " as " )
		, ( symbolp -> desc == nlp -> n_desc
			? "" : article( symbolp -> desc ) )
		, symbolp -> sym_un.sym_str.rfilep -> name
		, symbolp -> sym_un.sym_str.rline );
	}
    }

    /*
     *	quadratically hashed symbol table.
     *	things are never deleted from the hash symbol table.
     *	as more hash table is needed,
     *	a new one is alloc'ed and chained to the end.
     *	search is by rehashing within each table,
     *	traversing chains to next table if unsuccessful.
     */
struct symbol *
entersymbol( name )
    char	*name;
    {
	static struct symboltableinfo	symboltable;
	char				*enteredname;
	long				hashindex;
	register struct symboltableinfo	*tablep;
	register struct symbol		**herep;
	register struct symbol		**limitp;
	register long			increment;

	enteredname = enterstring( name );
	hashindex = SHORT_ABS( ( long ) enteredname ) % SYMBOLPRIME;
	for ( tablep = &symboltable ; /*return*/ ; tablep = tablep -> chain ) {
	    if ( tablep == NIL ) {
#		ifdef SPACEDEBUG
		    fprintf( stderr ,
			    "[entersymbol] calloc'ing table for %d symbols\n" , 
			    SYMBOLPRIME );
#		endif SPACEDEBUG
		for ( tablep = &symboltable
		    ; tablep->chain != NIL
		    ; tablep = tablep->chain ) {
			continue;
		}
		tablep->chain = ( struct symboltableinfo * )
			    calloc( 1 , sizeof ( struct symboltableinfo ) );
		if ( tablep->chain == NIL ) {
		    error( FATAL , "ran out of memory (entersymbol)" );
		}
		tablep = tablep->chain;
	    }
	    herep = &( tablep -> entry[ hashindex ] );
	    limitp = &( tablep -> entry[ SYMBOLPRIME ] );
	    increment = 1;
	    do {
		if ( *herep == NIL ) {
			/* empty */
		    if ( tablep -> used > ( ( SYMBOLPRIME / 4 ) * 3 ) ) {
			    /* too full, break for next table */
			break;
		    }
		    tablep -> used++;
		    *herep = symbolalloc();
		    ( *herep ) -> name = enteredname;
		    ( *herep ) -> lookup = NEW;
#		    ifdef HASHDEBUG
			fprintf( stderr ,
				"[entersymbol] name %s NEW after %d\n" ,
				enteredname , increment / 2 );
#		    endif HASHDEBUG
		    return *herep;
		}
		    /* a find? */
		if ( ( *herep ) -> name == enteredname ) {
		    ( *herep ) -> lookup = OLD;
#		    ifdef HASHDEBUG
			fprintf( stderr , "[entersymbol] name %s OLD at %d\n" ,
				enteredname , increment / 2 );
#		    endif HASHDEBUG
		    return *herep;
		}
		herep += increment;
		if ( herep >= limitp ) {
		    herep -= SYMBOLPRIME;
		}
		increment += 2;
	    } while ( increment < SYMBOLPRIME );
#	    ifdef HASHDEBUG
		fprintf( stderr , "[entersymbol] next symboltable\n" );
#	    endif HASHDEBUG
	}
    }

    /*
     *	allocate a symbol from the dynamically allocated symbol table.
     */
struct symbol *
symbolalloc()
    {
	static struct symbol	*nextsymbol = NIL;
	static long		symbolsleft = 0;
	struct symbol		*newsymbol;

	if ( symbolsleft <= 0 ) {
#	    ifdef SPACEDEBUG
		fprintf( stderr ,
			"[symbolalloc] malloc space for %d symbols\n" ,
			SYMBOLALLOC / sizeof( struct symbol ) );
#	    endif SPACEDEBUG
	    nextsymbol = ( struct symbol * ) malloc( SYMBOLALLOC );
	    if ( nextsymbol == 0 ) {
		error( FATAL , "ran out of memory (symbolalloc)" );
	    }
	    symbolsleft = SYMBOLALLOC / sizeof( struct symbol );
	}
	newsymbol = nextsymbol;
	nextsymbol++;
	symbolsleft--;
	return newsymbol;
    }

    /*
     *	hash a string based on all of its characters.
     */
long
hashstring( string )
    char	*string;
    {
	register char	*cp;
	register long	value;

	value = 0;
	for ( cp = string ; *cp ; cp++ ) {
	    value = ( value * 2 ) + *cp;
	}
	return value;
    }

    /*
     *	quadratically hashed string table.
     *	things are never deleted from the hash string table.
     *	as more hash table is needed,
     *	a new one is alloc'ed and chained to the end.
     *	search is by rehashing within each table,
     *	traversing chains to next table if unsuccessful.
     */
char *
enterstring( string )
    char	*string;
    {
	static struct stringtableinfo	stringtable;
	long				hashindex;
	register struct stringtableinfo	*tablep;
	register char			**herep;
	register char			**limitp;
	register long			increment;

	hashindex = SHORT_ABS( hashstring( string ) ) % STRINGPRIME;
	for ( tablep = &stringtable ; /*return*/ ; tablep = tablep -> chain ) {
	    if ( tablep == NIL ) {
#		ifdef SPACEDEBUG
		    fprintf( stderr ,
			    "[enterstring] calloc space for %d strings\n" ,
			    STRINGPRIME );
#		endif SPACEDEBUG
		for ( tablep = &stringtable
		    ; tablep->chain != NIL
		    ; tablep = tablep->chain ) {
			continue;
		}
		tablep->chain = ( struct stringtableinfo * )
			    calloc( 1 , sizeof ( struct stringtableinfo ) );
		if ( tablep->chain == NIL ) {
		    error( FATAL , "ran out of memory (enterstring)" );
		}
		tablep = tablep->chain;
	    }
	    herep = &( tablep -> entry[ hashindex ] );
	    limitp = &( tablep -> entry[ STRINGPRIME ] );
	    increment = 1;
	    do {
		if ( *herep == NIL ) {
			/* empty */
		    if ( tablep -> used > ( ( STRINGPRIME / 4 ) * 3 ) ) {
			    /* too full, break for next table */
			break;
		    }
		    tablep -> used++;
		    *herep = charalloc( strlen( string ) );
		    strcpy( *herep , string );
#		    ifdef HASHDEBUG
			fprintf( stderr ,
				"[enterstring] string %s copied after %d\n" , 
				*herep , increment / 2 );
#		    endif HASHDEBUG
		    return *herep;
		}
		    /* quick, check the first chars and then the rest */
		if ( **herep == *string && strcmp( *herep , string ) == 0 ) {
#		    ifdef HASHDEBUG
			fprintf( stderr ,
				"[enterstring] string %s found after %d\n" ,
				*herep , increment / 2 );
#		    endif HASHDEBUG
		    return *herep;
		}
		herep += increment;
		if ( herep >= limitp ) {
		    herep -= STRINGPRIME;
		}
		increment += 2;
	    } while ( increment < STRINGPRIME );
#	    ifdef HASHDEBUG
		fprintf( stderr , "[enterstring] next stringtable\n" );
#	    endif HASHDEBUG
	}
    }

    /*
     *	copy a string to the dynamically allocated character table.
     */
char *
charalloc( length )
    register long	length;
    {
	static char	*nextchar = NIL;
	static long	charsleft = 0;
	register long	lengthplus1 = length + 1;
	register long	askfor;
	char		*newstring;

	if ( charsleft < lengthplus1 ) {
	    askfor = lengthplus1 > CHARALLOC ? lengthplus1 : CHARALLOC;
#	    ifdef SPACEDEBUG
		fprintf( stderr , "[charalloc] malloc space for %d chars\n" 
			, askfor );
#	    endif SPACEDEBUG
	    nextchar = ( char * ) malloc( askfor );
	    if ( nextchar == 0 ) {
		error( FATAL , "no room for %d characters" , askfor );
	    }
	    charsleft = askfor;
	}
	newstring = nextchar;
	nextchar += lengthplus1;
	charsleft -= lengthplus1;
	return newstring;
    }

    /*
     *	read an archive header for the next element
     *	and find the offset of the one after this. 
     */
BOOL
nextelement( ofilep )
    struct fileinfo	*ofilep;
    {
	register char	*cp;
	register long	red;
	register off_t	arsize;
	struct ar_hdr	archdr;

	fseek( ofilep -> file , ofilep -> nextoffset , 0 );
	red = fread( (char *) &archdr , 1 , sizeof archdr , ofilep -> file );
	if ( red != sizeof archdr ) {
	    return FALSE;
	}
	    /* null terminate the blank-padded name */
	cp = &archdr.ar_name[ ( sizeof archdr.ar_name ) - 1 ];
	*cp = '\0';
	while ( *--cp == ' ' ) {
	    *cp = '\0';
	}
	    /* set up the address of the beginning of next element */
	arsize = atol( archdr.ar_size );
	    /* archive elements are aligned on 0 mod 2 boundaries */
	if ( arsize & 1 ) {
	    arsize += 1;
	}
	ofilep -> nextoffset = ftell( ofilep -> file ) + arsize;
	    /* say we had one */
	return TRUE;
    }

    /*
     *	variable number of arguments to error, like printf.
     */
error( type , message , arg1 , arg2 , arg3 , arg4 , arg5 , arg6 , arg7 , arg8 )
    int		type;
    char	*message;
    {
	errors = type > errors ? type : errors;
	if ( wflag && type == WARNING ) {
	    return;
	}
	fprintf( stderr , "%s: " , program );
	switch ( type ) {
	    case WARNING:
		    fprintf( stderr , "Warning: " );
		    break;
	    case ERROR:
		    fprintf( stderr , "Error: " );
		    break;
	    case FATAL:
		    fprintf( stderr , "Fatal: " );
		    break;
	    default:
		    fprintf( stderr , "Ooops: " );
		    break;
	}
	fprintf( stderr , message , arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8 );
	fprintf( stderr , "\n" );
	if ( type == FATAL ) {
	    exit( FATAL );
	}
    }

char *
classify( type )
    unsigned char	type;
    {
	switch ( type ) {
	    case N_PSO:
		return "source file";
	    case N_PSOL:
		return "include file";
	    case N_PGLABEL:
		return "label";
	    case N_PGCONST:
		return "constant";
	    case N_PGTYPE:
		return "type";
	    case N_PGVAR:
		return "variable";
	    case N_PGFUNC:
		return "function";
	    case N_PGPROC:
		return "procedure";
	    case N_PEFUNC:
		return "external function";
	    case N_PEPROC:
		return "external procedure";
	    case N_PLDATA:
		return "library variable";
	    case N_PLTEXT:
		return "library routine";
	    default:
		return "unknown symbol";
	}
    }

char *
article( type )
    unsigned char	type;
    {
	switch ( type ) {
	    case N_PSO:
		return "a source file";
	    case N_PSOL:
		return "an include file";
	    case N_PGLABEL:
		return "a label";
	    case N_PGCONST:
		return "a constant";
	    case N_PGTYPE:
		return "a type";
	    case N_PGVAR:
		return "a variable";
	    case N_PGFUNC:
		return "a function";
	    case N_PGPROC:
		return "a procedure";
	    case N_PEFUNC:
		return "an external function";
	    case N_PEPROC:
		return "an external procedure";
	    case N_PLDATA:
		return "a library variable";
	    case N_PLTEXT:
		return "a library routine";
	    default:
		return "an unknown symbol";
	}
    }
