/* Copyright (c) 1980 Regents of the University of California */

static	char sccsid[] = "@(#)stab.c 1.8 %G%";

    /*
     *	procedures to put out sdb symbol table information.
     *	and stabs for separate compilation type checking.
     *	these use the new .stabs, .stabn, and .stabd directives
     */

#include	"whoami.h"
#ifdef	PC
    /*	and the rest of the file */
#   include	"0.h"
#   include	"objfmt.h"
#   include	<stab.h>

    /*
     *  additional symbol definition for <stab.h>
     *	that is used by the separate compilation facility --
     *	eventually, <stab.h> should be updated to include this 
     */

#   include	"pstab.h"
#   include	"pc.h"

    /*
     *	absolute value: line numbers are negative if error recovery.
     */
#define	ABS( x )	( x < 0 ? -x : x )

    /*
     *	global variables
     */
stabgvar( name , type , offset , length , line )
    char	*name;
    int		type;
    int		offset;
    int		length;
    int		line;
    {

	    /*
	     *	for separate compilation
	     */
	putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
		    , name , N_PC , N_PGVAR , ABS( line ) );
	    /*
	     *	for sdb
	     */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0x%x,0" , 0 , N_GSYM , type );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
}

    /*
     *	local variables
     */
stablvar( name , type , level , offset , length )
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
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0x%x,0x%x" , 0 , N_LSYM , type , -offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
}


    /*
     *	parameters
     */
stabparam( name , type , offset , length )
    char	*name;
    int		type;
    int		offset;
    int		length;
    {
	
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0x%x,0x%x" , 0 , N_PSYM , type , offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
    }

    /*
     *	fields
     */
stabfield( name , type , offset , length )
    char	*name;
    int		type;
    int		offset;
    int		length;
    {
	
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0x%x,0x%x" , 0 , N_SSYM , type , offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0x%x,0,0,0x%x" , 0 , N_LENG , length );
    }

    /*
     *	left brackets
     */
stablbrac( level )
    int	level;
    {

	if ( ! opt('g') ) {
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

	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabd	0x%x,0,0x%x" , 0 , N_RBRAC , level );
    }

    /*
     *	functions
     */
stabfunc( name , typeclass , line , level )
    char	*name;
    int		typeclass;
    int		line;
    long	level;
    {
	int	type;
	long	i;
	char	extname[ BUFSIZ ];

	    /*
	     *	for separate compilation
	     */
	if ( level == 1 ) {
	    if ( typeclass == FUNC ) {
		putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
			    , name , N_PC , N_PGFUNC , ABS( line ) );
	    } else if ( typeclass == PROC ) {
		putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
			    , name , N_PC , N_PGPROC , ABS( line ) );
	    }
	}
	    /*
	     *	for sdb
	     */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	sextname( extname , name , level );
	putprintf( "\",0x%x,0,0x%x,%s" , 0 , N_FUN , line , extname );
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
     *	source files
     */
stabsource( filename )
    char	*filename;
    {
	int	label;
	
	    /*
	     *	for separate compilation
	     */
	putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0" , 0 
		    , filename , N_PC , N_PSO );
	    /*
	     *	for sdb
	     */
	if ( ! opt('g') ) {
		return;
	}
	label = getlab();
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0x%x,0,0," , 1 , N_SO );
	putprintf( PREFIXFORMAT , 0 , LLABELPREFIX , label );
	putprintf( PREFIXFORMAT , 1 , LLABELPREFIX , label );
	putprintf( ":" , 0 );
    }

    /*
     *	included files get one or more of these:
     *	one as they are entered by a #include,
     *	and one every time they are returned to by nested #includes
     */
stabinclude( filename )
    char	*filename;
    {
	int	label;
	
	    /*
	     *	for separate compilation
	     */
	putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0" , 0 
		    , filename , N_PC , N_PSOL );
	    /*
	     *	for sdb
	     */
	if ( ! opt('g') ) {
		return;
	}
	label = getlab();
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0x%x,0,0," , 1 , N_SOL );
	putprintf( PREFIXFORMAT , 0 , LLABELPREFIX , label );
	putprintf( PREFIXFORMAT , 1 , LLABELPREFIX , label );
	putprintf( ":" , 0 );
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
		    , label , N_PC , N_PGLABEL , ABS( line ) );
    }

    /*
     *	global constants
     */
stabgconst( const , line )
    char	*const;
    int		line;
    {

	    putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
			, const , N_PC , N_PGCONST , ABS( line ) );
    }

    /*
     *	global types
     */
stabgtype( type , line )
    char	*type;
    int		line;
    {

	    putprintf( "	.stabs	\"%s\",0x%x,0,0x%x,0x%x" , 0 
			, type , N_PC , N_PGTYPE , ABS( line ) );
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
		    , name , N_PC , type , ABS( line ) );
    }

#endif PC
