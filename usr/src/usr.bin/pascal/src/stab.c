/* Copyright (c) 1979 Regents of the University of California */

    /*
     *	procedures to put out sdb symbol table information
     *	    these use the new .stabs, .stabn, and .stabd directives
     */

static	char sccsid[] = "@(#)stab.c 1.1 %G%";

#include	"whoami.h"
#ifdef	PC
    /*	and the rest of the file */
#   include	"0.h"
#   include	<stab.h>

    /*
     *	here's that ugly name length limit of 8 characters
     *	until someone fixes sdb.
     */
#define	SNAMELENGTH	8
#define	SNAMEFORMAT	"%.*s"
/*
 *  the file "p.a.out" has an additional symbol definition for "a.out.h"
 *	that is used by the separate compilation facility --
 *	eventually, "a.out.h" must be updated to include this 
 */

#   include	"p.a.out.h"
#   include	"pc.h"

    /*
     *	variables
     */
stabvar( name , type , level , offset , length )
    char	*name;
    int		type;
    int		level;
    int		offset;
    int		length;
    {
	char	*nullchar;
	char	*cp;

	/* for separate compilation */

	if ((level == 1) && (strcmp(name, DISPLAYNAME) != 0)) {
		nullchar = name;
		while ( *nullchar ) {
	    		nullchar ++;
		}
		for ( cp = name ; cp < nullchar ; cp += SNAMELENGTH ) {
		    putprintf( "	.stabs	\"" , 1 );
		    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
	    	    putprintf( "\",0%o,0,0,0" , 0 , N_PGVAR , 0 );
		}
		if ( cp == nullchar ) {
		/*
		 * then the name was exactly a multiple of SNAMELENGTH long,
		 * and i have to put out a null to terminate it.
		 */
	    	putprintf( "	.stabn	0%o,0,0,0" , 0
			, N_PGVAR, 0 );
		}
    	}

	/* for sdb */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	if ( level == 1 ) {
		putprintf( "\",0%o,0,0%o,0" , 0 , N_GSYM , type );
	} else {
		putprintf( "\",0%o,0,0%o,%d" , 0
				, N_LSYM , type , offset);
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0%o,0,0,0%o" , 0 , N_LENG , length );

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
	putprintf( "\",0%o,0,0%o,%d" , 0 , N_PSYM , type , offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0%o,0,0,0%o" , 0 , N_LENG , length );
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
	putprintf( "\",0%o,0,0%o,%d" , 0 , N_SSYM , type , offset );
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0%o,0,0,0%o" , 0 , N_LENG , length );
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
	putprintf( "	.stabd	0%o,0,%d" , 0 , N_LBRAC , level );
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
	putprintf( "	.stabd	0%o,0,%d" , 0 , N_RBRAC , level );
    }

    /*
     *	functions
     */
stabfunc( name , line , level )
    char	*name;
    int		line;
    long	level;
    {

	char	*nullchar;
	char	*cp;
	int	i;

	/*
	 *	for separate compilation
	 */
	nullchar = name;
	while ( *nullchar ) {
	    	nullchar ++;
	}
	for ( cp = name ; cp < nullchar ; cp += SNAMELENGTH ) {
		putprintf( "	.stabs	\"" , 1 );
		putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
	    	putprintf( "\",0%o,0,0,0" , 0 , N_PFUN , 0 );
	}
	if ( cp == nullchar ) {
	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PFUN, 0 );
	}

	/* for sdb */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , name );
	putprintf( "\",0%o,0,%d," , 1 , N_FUN , line );
	for ( i = 1 ; i < level ; i++ ) {
	    putprintf( EXTFORMAT , 1 , enclosing[ i ] );
	}
	putprintf( EXTFORMAT , 0 , name );
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
	if ( line < 0 ) {
		/*
		 *	line numbers get to be negative if there was an error.
		 */
	    line = -line;
	}
	
	putprintf( "	.stabd	0%o,0,%d" , 0 , N_SLINE , line );
    }

    /*
     *	source files
     */
stabsource( filename )
    char	*filename;
    {
	int	label = getlab();
	char	*nullchar;
	char	*cp;
	
	nullchar = filename;
	while ( *nullchar ) {
    		nullchar ++;
	}
	for ( cp = filename ; cp < nullchar ; cp += SNAMELENGTH ) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
    	    putprintf( "\",0%o,0,0,0" , 0 , N_PSO , 0 );
	}
	if ( cp == nullchar ) {
	/*
	 * then the name was exactly a multiple of SNAMELENGTH long,
	 */
    	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PSO, 0 );
	}

	/* for sdb */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0%o,0,0," , 1 , N_SO );
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
	int	label = getlab();
	char	*nullchar;
	char	*cp;
	
	nullchar = filename;
	while ( *nullchar ) {
    		nullchar ++;
	}
	for ( cp = filename ; cp < nullchar ; cp += SNAMELENGTH ) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
    	    putprintf( "\",0%o,0,0,0" , 0 , N_PSOL , 0 );
	}
	if ( cp == nullchar ) {
	/*
	 * then the name was exactly a multiple of SNAMELENGTH long,
	 */
    	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PSOL, 0 );
	}

	/* for sdb */
	if ( ! opt('g') ) {
		return;
	}
	putprintf( "	.stabs	\"" , 1 );
	putprintf( NAMEFORMAT , 1 , filename );
	putprintf( "\",0%o,0,0," , 1 , N_SOL );
	putprintf( PREFIXFORMAT , 0 , LLABELPREFIX , label );
	putprintf( PREFIXFORMAT , 1 , LLABELPREFIX , label );
	putprintf( ":" , 0 );
    }


/*
 * global Pascal symbols :
 *   labels, types, constants, and external procedure and function names:
 *   These are used by the separate compilation facility
 *   to be able to check for disjoint header files.
 *   New symbol codes : (N_PGVAR, N_PFUN defined above),
 *   N_PGLAB, N_PGCON, N_PGTYP  
 *   and N_PEFUN are defined for these additional global Pascal	
 *   symbols in p.a.out.h so that 
 *   they can be ignored by "sdb".  The only information
 *   put out for constants and types is their names.
 *   For labels, the integer label is put out. For external functions
 *   and procedures, the name of the function or procedure is put out.
 */

/* global constants */
stabcname( name )
    char *name;

    {
	char	*nullchar;
	char	*cp;

	nullchar = name;
	while ( *nullchar ) {
    		nullchar ++;
	}
	for ( cp = name ; cp < nullchar ; cp += SNAMELENGTH ) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
    	    putprintf( "\",0%o,0,0,0" , 0 , N_PGCON , 0 );
	}
	if ( cp == nullchar ) {
    	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PGCON, 0 );
	}

    }

/* global types */
stabtname( name )
    char *name;

    {
	char	*nullchar;
	char	*cp;

	nullchar = name;
	while ( *nullchar ) {
    		nullchar ++;
	}
	for ( cp = name ; cp < nullchar ; cp += SNAMELENGTH ) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
    	    putprintf( "\",0%o,0,0,0" , 0 , N_PGTYP , 0 );
	}
	if ( cp == nullchar ) {
    	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PGTYP, 0 );
	}
    }

/* global labels */
stabglab( label )
    int label;

    {

	putprintf( "	.stabs	\"" , 1 );
	putprintf( PREFIXFORMAT , 1 , PLABELPREFIX , label );
	putprintf( "\",0%o,0,0,0" , 0 , N_PGLAB , 0 );
    }


/* external functions and procedures */	
stabefunc( name , line )
    char *name;
    int line;
    {

	char	*nullchar;
	char	*cp;

	nullchar = name;
	while ( *nullchar ) {
    		nullchar ++;
	}
	for ( cp = name ; cp < nullchar ; cp += SNAMELENGTH ) {
	    putprintf( "	.stabs	\"" , 1 );
	    putprintf( SNAMEFORMAT , 1 , SNAMELENGTH , cp );
    	    putprintf( "\",0%o,0,0,0" , 0 , N_PEFUN , 0 );
	}
	if ( cp == nullchar ) {
    	putprintf( "	.stabn	0%o,0,0,0" , 0 , N_PEFUN, 0 );
	}
    }

#endif PC


