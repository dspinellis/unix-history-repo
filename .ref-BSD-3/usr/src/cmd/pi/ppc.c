    /*
     *	functions to help pi put out
     *	polish postfix binary portable c compiler intermediate code
     *	thereby becoming the portable pascal compiler
     */

#include	"whoami"
#ifdef PPC
#include	"0.h"
#include	"opcode.h"
#include	"ppc.h"

    /*
     *	extract from pi's format
     */
#define		FROMPI( nth , word )	( ( ( word ) >> ( (nth) * 8 ) ) & 0377 )

    /*
     *	mash into f77's format
     */
#define		TOF77( fop,val,rest )	( ( ( (rest) & 0177777 ) << 16 ) \
					| ( ( (val) & 0377 ) << 8 )	 \
					| ( (fop) & 0377 ) )

    /*
     *	this version of put generates stephen c johnson intermediate code
     *	as modified for the fortran77 compiler, to be both
     *	binary and postfix.
     *	it still uses the address of its argument
     *	as the address of its (variable length) argument list.
     *	the name is changed by a #define for the ctags program.
     */
#define	ppcput	put
ppcput( arglist )
    {
	register int	*argp;
	int		narg;
	int		op;
	int		subop;

	    /*
	     * are we not generating code?
	     */
	if ( cgenflg )
	    return;
	argp = &arglist;
	narg = *argp++;
	op = FROMPI( 0 , *argp );
	subop = FROMPI( 1 , *argp );
	putprintf( "#		[put] op = 0%o subop = %d argp[1] = %d"
		 , op , subop , argp[1] );
	switch( op ) {
	    default:
		/*
		 * panic( "[put] op" );
		 */
		putprintf( "#		[put] op ignored" );
		break;
	    case O_LV:
		putLV( subop >> 1 , argp[1] , P2PTR | P2INT );
		break;
	    case O_RV1:
	    case O_RV2:
	    case O_RV4:
	    case O_RV8:
		putRV( op , subop >> 1 , argp[1] , 0 );
		break;
	    case O_RV:
		putRV( op , subop >> 1 , argp[1] , argp[2] );
		break;
	    case O_CON1:
	    case O_CON2:
	    case O_CON4:
		putCON( op , argp[1] );
		break;
	    case O_CON8:
		putCON( op , *( (double *) &argp[1] ) );
		break;
	    case O_AS21:
	    case O_AS41:
	    case O_AS2:
	    case O_AS42:
	    case O_AS24:
	    case O_AS4:
	    case O_AS28:
	    case O_AS48:
	    case O_AS8:
		putAS( op , 0 );
		break;
	    case O_AS:
		putAS( op , argp[1] );
		break;
	    case O_ADD2:
	    case O_ADD42:
	    case O_ADD82:
	    case O_ADD24:
	    case O_ADD4:
	    case O_ADD84:
	    case O_ADD28:
	    case O_ADD48:
	    case O_ADD8:
		putADD( op );
		break;
	    case O_SUB2:
	    case O_SUB42:
	    case O_SUB82:
	    case O_SUB24:
	    case O_SUB4:
	    case O_SUB84:
	    case O_SUB28:
	    case O_SUB48:
	    case O_SUB8:
		putSUB( op );
		break;
	    case O_MUL2:
	    case O_MUL42:
	    case O_MUL82:
	    case O_MUL24:
	    case O_MUL4:
	    case O_MUL84:
	    case O_MUL28:
	    case O_MUL48:
	    case O_MUL8:
		putMUL( op );
		break;
	    case O_DVD2:
	    case O_DVD42:
	    case O_DVD82:
	    case O_DVD24:
	    case O_DVD4:
	    case O_DVD84:
	    case O_DVD28:
	    case O_DVD48:
	    case O_DVD8:
		putDVD( op );
		break;
	    case O_DIV2:
	    case O_DIV42:
	    case O_DIV24:
	    case O_DIV4:
		putDIV( op );
		break;
	    case O_MOD2:
	    case O_MOD42:
	    case O_MOD24:
	    case O_MOD4:
		putMOD( op );
		break;
	}
    }


putLV( level , offset , type )
    int	level;
    int	offset;
    int	type;
    {
	if ( level == cbn ) {
		putleaf( P2REG , 0 , P2FP , P2PTR | P2INT , 0 );
	} else {
		putleaf( P2NAME , level * sizeof (int *) , 0 , P2PTR | P2INT 
		       , "_display" );
	}
	putleaf( P2ICON , offset , 0 , P2INT , 0 );
	putop( P2PLUS , P2PTR | P2INT );
	putop( P2UNARY P2MUL , type );
    }

    /*
     *	an operand, given its level and offset, 
     *	and its length if it is other than 1, 2, 4, or 8
     */
putRV( op , level , offset , length )
    int	op;
    int	level;
    int	offset;
    int	length;
    {
	int		type;

	switch ( op ) {
	    default:
		panic( "putRV" );
	    case O_RV:
		/*
		 * no structures, yet
		 */
		panic( "putRV O_RV" );
	    case O_RV1:
		type = P2CHAR;
		break;
	    case O_RV2:
		type = P2SHORT;
		break;
	    case O_RV4:
		type = P2LONG;
		break;
	    case O_RV8:
		type = P2DOUBLE;
		break;
	}
	putLV( level , offset , type );
    }

putCON( op , value )
    int op;
    int	value;
    {
	int	type;

	switch( op ) {
	    case O_CON1:
		type = P2CHAR;
		break;
	    case O_CON2:
		type = P2SHORT;
		break;
	    case O_CON4:
		type = P2LONG;
		break;
	    case O_CON8:
		type = P2DOUBLE;
		break;
	}
	if ( type != P2DOUBLE ) {
		putleaf( P2ICON , value , 0 , type , 0 );
	} else {
		char	name[8];

		sprintf( name , "D%d" , newlabel() );
		puttext( "	.data" );
		puttext( "	.align 2" );
		putprintf( "%s:" , name );
		putprintf( "	.double 0d%.20e" , *( (double *) &value ) );
		puttext( "	.text" );
		puttext( "	.align 1" );
		putleaf( P2NAME , 0 , 0 , P2DOUBLE , name );
	}
    }

    /*
     *	generate an assignment
     *	given the length of the destination if not 1, 2, 4, or 8
     */
putAS( op , length )
    int	op;
    int	length;
    {
	int	type;
	
	switch ( op ) {
	    default:
		panic( "[putAS]" );
	    case O_AS:
		/*
		 * no structures, yet
		 */
		panic( "[putAS] O_AS" );
	    case O_AS21:
	    case O_AS41:
		type = P2CHAR;
		break;
	    case O_AS2:
	    case O_AS42:
		type = P2SHORT;
		break;
	    case O_AS24:
	    case O_AS4:
		type = P2LONG;
		break;
	    case O_AS28:
	    case O_AS48:
	    case O_AS8:
		type = P2DOUBLE;
		break;
	}
	putop( P2ASSIGN , type );
    }

    /*
     *	the various additions
     */
putADD( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_ADD2:
	    case O_ADD42:
	    case O_ADD82:
		type = P2SHORT;
		break;
	    case O_ADD24:
	    case O_ADD4:
	    case O_ADD84:
		type = P2LONG;
		break;
	    case O_ADD28:
	    case O_ADD48:
	    case O_ADD8:
		type = P2DOUBLE;
		break;
	}
	putop( P2PLUS , type );
    }

    /*
     *	the various subtractions
     */
putSUB( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_SUB2:
	    case O_SUB42:
	    case O_SUB82:
		type = P2SHORT;
		break;
	    case O_SUB24:
	    case O_SUB4:
	    case O_SUB84:
		type = P2LONG;
		break;
	    case O_SUB28:
	    case O_SUB48:
	    case O_SUB8:
		type = P2DOUBLE;
		break;
	}
	putop( P2MINUS , type );
    }

    /*
     *	the various multiplications
     */
putMUL( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_MUL2:
	    case O_MUL42:
	    case O_MUL82:
		type = P2SHORT;
		break;
	    case O_MUL24:
	    case O_MUL4:
	    case O_MUL84:
		type = P2LONG;
		break;
	    case O_MUL28:
	    case O_MUL48:
	    case O_MUL8:
		type = P2DOUBLE;
		break;
	}
	putop( P2MUL , type );
    }

    /*
     *	the various divisions (floating results)
     */
putDVD( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_DVD2:
	    case O_DVD42:
	    case O_DVD82:
		type = P2SHORT;
		break;
	    case O_DVD24:
	    case O_DVD4:
	    case O_DVD84:
		type = P2LONG;
		break;
	    case O_DVD28:
	    case O_DVD48:
	    case O_DVD8:
		type = P2DOUBLE;
		break;
	}
	/*
	 * convert the right operand to a double to force floating result
	 *	putop( P2SCONV , P2DOUBLE );
	 * unfortunately, this doesn't work, and both operands have to
	 * be converted, and it's too late to get the left one. (sigh).
	 * that would work if the left operand were already a double,
	 * but for now ...
	 */
	if ( op != O_DVD8 )
	    panic( "[putDVD]" );
	putop( P2DIV , type );
    }

    /*
     *	the various DIVs (truncated integer results)
     */
putDIV( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_DIV2:
	    case O_DIV42:
		type = P2SHORT;
		break;
	    case O_DIV24:
	    case O_DIV4:
		type = P2LONG;
		break;
	}
	putop( P2DIV , type );
    }

    /*
     *	the various MODs (truncated integer results)
     */
putMOD( op )
    int	op;
    {
	int	type;

	switch ( op ) {
	    case O_MOD2:
	    case O_MOD42:
		type = P2SHORT;
		break;
	    case O_MOD24:
	    case O_MOD4:
		type = P2LONG;
		break;
	}
	putop( P2MOD , type );
    }


    /*
     *	this returns a unique integer to be made into a label
     */
int
newlabel()
    {
	static lastlabel = 0;

	return ++lastlabel;
    }

    /*
     *	to round string lengths up to 0 mod 4
     */
str4len( string )
    char	*string;
    {
	
	return ( ( strlen( string ) + 3 ) / 4 );
    }


    /*
     *	emits an ftext operator and a string to the ppcstream
     */
puttext( string )
    char	*string;
    {
	int	length = str4len( string );

	emitword( TOF77( P2FTEXT , length , 0 ) );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "P2FTEXT | %d | 0\n" , length );
	    }
#	endif
	emitstring( string );
    }

    /*
     *	puts out formatted text to the ppcstream.
     *	none of arg1 .. arg5 need be present.
     *	and you can add more if you need them.
     */
    /* VARARGS */
putprintf( format , arg1 , arg2 , arg3 , arg4 , arg5 )
    char	*format;
    {
	char	buffer[128];

	sprintf( buffer , format , arg1 , arg2 , arg3 , arg4 , arg5 );
	puttext( buffer );
    }

    /*
     *	emit a left bracket operator to ppcstream
     *	with function number, the maximum temp register, and total locals
     *	from globals ftnno and sizes[ cbn ]
     *	until i figure out how to use them, regs 0 .. 11 are free.
     *	one idea for one reg is to save the display pointer on block entry
     */
putlbracket()
    {
#	define	MAXTP2REG	11
#	define	BITSPERBYTE	8

	emitword( TOF77( P2FLBRAC , MAXTP2REG , ftnno ) );
	emitword( BITSPERBYTE * -sizes[ cbn ].om_off );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "P2FLBRAC | %d | %d\n" , MAXTP2REG , ftnno );
		fprintf( ppcdstream , "%d\n"
		       , BITSPERBYTE * -sizes[ cbn ].om_off );
	    }
#	endif
    }

    /*
     *	emit a right bracket operator
     *	which for the binary (fortran) interface
     *	doesn't have any (label) arguments,
     *	it just forces the stack allocate and register mask
     */
putrbracket()
    {

	emitword( P2FRBRAC );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "P2FRBRAC\n" );
	    }
#	endif
    }

    /*
     *	emit an eof operator
     */
puteof()
    {
	
	emitword( P2FEOF );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "P2FEOF\n" );
	    }
#	endif
    }

    /*
     *	emit a dot operator,
     *	with a source file line number and name
     *	from globals filename and line
     */
putexpr()
    {
	int	length = str4len( filename );

	emitword( TOF77( P2FEXPR , length , line ) );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "P2FEXPR | %d | %d\n" , length , line );
	    }
#	endif
	emitstring( filename );
    }

    /*
     *	put out a leaf node
     */
putleaf( op , lval , rval , type , name )
    int		op;
    int		lval;
    int		rval;
    int		type;
    char	*name;
    {
	switch ( op ) {
	    default:
		panic( "[putleaf]" );
	    case P2ICON:
		emitword( TOF77( P2ICON , name != NIL , type ) );
		emitword( lval );
#		ifdef DEBUG
		    if ( ppcdebug ) {
			fprintf( ppcdstream , "P2ICON | %d | %d\n" 
			       , name != NIL , type );
			fprintf( ppcdstream , "%d\n" , lval );
		    }
#		endif
		if ( name )
		    emitname( name );
		break;
	    case P2NAME:
		emitword( TOF77( P2NAME , lval != 0 , type ) );
		if ( lval ) 
		    emitword( lval );
#		ifdef DEBUG
		    if ( ppcdebug ) {
			fprintf( ppcdstream , "P2NAME | %d | %d\n" 
			       , lval != 0 , type );
			if ( lval )
			    fprintf( ppcdstream , "%d\n" , lval );
		    }
#		endif
		emitname( name );
		break;
	    case P2REG:
		emitword( TOF77( P2REG , rval , type ) );
#		ifdef DEBUG
		    if ( ppcdebug ) {
			fprintf( ppcdstream , "P2REG | %d | %d\n" , rval , type );
		    }
#		endif
		break;
	}
    }

    /*
     *	put a typed operator to the ppcstream
     */
putop( op , type )
    int		op;
    int		type;
    {
	
	emitword( TOF77( op , 0 , type ) );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "%d | 0 | %d\n" , op , type );
	    }
#	endif
    }


    /*
     *	puts a long word on the ppcstream
     */
emitword( word )
    long	word;
    {

	putw( word , ppcstream );
    };

    /*
     *	put a length 0 mod 4 null padded string onto the ppcstream
     *	this would use
     *		fprintf( ppcstream , "%*s" , -str4len( string ) , string )
     *	except that doesn't work, and also it wants to be padded with nulls.
     */
emitstring( string )
    char	*string;
    {
	int	slen = strlen( string );
	int	wlen = ( slen + 3 ) / 4;
	int	plen = ( wlen * 4 ) - slen;
	char	*cp;
	int	p;

	for ( cp = string ; *cp ; cp++ )
	    putc( *cp , ppcstream );
	for ( p = 1 ; p <= plen ; p++ )
	    putc( '\0' , ppcstream );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "\"%s" , string );
		for ( p = 1 ; p <= plen ; p++ )
		    fprintf( ppcdstream , "\\0" );
		fprintf( ppcdstream , "\"\n" );
	    }
#	endif
    }

    /*
     *	puts a blank-padded 8 character name on the ppcstream
     */
emitname( name )
    char	*name;
    {

	fprintf( ppcstream , "%-8.8s" , name );
#	ifdef DEBUG
	    if ( ppcdebug ) {
		fprintf( ppcdstream , "<%-8.8s>\n" , name );
	    }
#	endif
    }
    
#endif PPC
