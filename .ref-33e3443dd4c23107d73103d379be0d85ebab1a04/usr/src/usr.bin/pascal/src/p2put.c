/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)p2put.c	5.4 (Berkeley) %G%";
#endif not lint

    /*
     *	functions to help pi put out
     *	polish postfix binary portable c compiler intermediate code
     *	thereby becoming the portable pascal compiler
     */

#include	"whoami.h"
#ifdef PC
#include	"0.h"
#include	"objfmt.h"
#include	<pcc.h>
#include	"pc.h"
#include	"align.h"
#include	"tmps.h"

    /*
     *	emits an ftext operator and a string to the pcstream
     */
puttext( string )
    char	*string;
    {
	int	length = str4len( string );

	if ( !CGENNING )
	    return;
	p2word( PCCM_TRIPLE( PCCF_FTEXT , length , 0 ) );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "PCCF_FTEXT | %3d | 0	" , length );
	    }
#	endif
	p2string( string );
    }

int
str4len( string )
    char	*string;
    {
	
	return ( ( strlen( string ) + 3 ) / 4 );
    }

    /*
     *	put formatted text into a buffer for printing to the pcstream.
     *	a call to putpflush actually puts out the text.
     *	none of arg1 .. arg5 need be present.
     *	and you can add more if you need them.
     */
/* VARARGS */
putprintf( format , incomplete , arg1 , arg2 , arg3 , arg4 , arg5 )
    char	*format;
    int		incomplete;
    {
	static char	ppbuffer[ BUFSIZ ];
	static char	*ppbufp = ppbuffer;

	if ( !CGENNING )
	    return;
	sprintf( ppbufp , format , arg1 , arg2 , arg3 , arg4 , arg5 );
	ppbufp = &( ppbuffer[ strlen( ppbuffer ) ] );
	if ( ppbufp >= &( ppbuffer[ BUFSIZ ] ) )
	    panic( "putprintf" );
	if ( ! incomplete ) {
	    puttext( ppbuffer );
	    ppbufp = ppbuffer;
	}
    }

    /*
     *	emit a left bracket operator to pcstream
     *	with function number, the maximum temp register, and total local bytes
     */
putlbracket(ftnno, sizesp)
    int		ftnno;
    struct om	*sizesp;
{
    int	maxtempreg;	
    int	alignedframesize;

#   if defined(vax) || defined(tahoe)
	maxtempreg = sizesp->curtmps.next_avail[REG_GENERAL];
#   endif vax || tahoe
#   ifdef mc68000
	    /*
	     *	this is how /lib/f1 wants it.
	     */
	maxtempreg =	(sizesp->curtmps.next_avail[REG_ADDR] << 4)
		      | (sizesp->curtmps.next_avail[REG_DATA]);
#   endif mc68000
    alignedframesize = roundup((int)(BITSPERBYTE * -sizesp->curtmps.om_off),
	(long)(BITSPERBYTE * A_STACK));
    p2word( PCCM_TRIPLE( PCCF_FLBRAC , maxtempreg , ftnno ) );
    p2word(alignedframesize);
#   ifdef DEBUG
	if ( opt( 'k' ) ) {
	    fprintf(stdout, "PCCF_FLBRAC | %3d | %d	%d\n",
		maxtempreg, ftnno, alignedframesize);
	}
#   endif
}

    /*
     *	emit a right bracket operator
     *	which for the binary interface
     *	forces the stack allocate and register mask
     */
putrbracket( ftnno )
    int	ftnno;
    {

	p2word( PCCM_TRIPLE( PCCF_FRBRAC , 0 , ftnno ) );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "PCCF_FRBRAC |   0 | %d\n" , ftnno );
	    }
#	endif
    }

    /*
     *	emit an eof operator
     */
puteof()
    {
	
	p2word( PCCF_FEOF );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "PCCF_FEOF\n" );
	    }
#	endif
    }

    /*
     *	emit a dot operator,
     *	with a source file line number and name
     *	if line is negative, there was an error on that line, but who cares?
     */
putdot( filename , line )
    char	*filename;
    int		line;
    {
	int	length = str4len( filename );

	if ( line < 0 ) {
	    line = -line;
	}
	p2word( PCCM_TRIPLE( PCCF_FEXPR , length , line ) );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "PCCF_FEXPR | %3d | %d	" , length , line );
	    }
#	endif
	p2string( filename );
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
	if ( !CGENNING )
	    return;
	switch ( op ) {
	    default:
		panic( "[putleaf]" );
	    case PCC_ICON:
		p2word( PCCM_TRIPLE( PCC_ICON , name != NIL , type ) );
		p2word( lval );
#		ifdef DEBUG
		    if ( opt( 'k' ) ) {
			fprintf( stdout , "PCC_ICON | %3d | 0x%x	" 
			       , name != NIL , type );
			fprintf( stdout , "%d\n" , lval );
		    }
#		endif
		if ( name )
		    p2name( name );
		break;
	    case PCC_NAME:
		p2word( PCCM_TRIPLE( PCC_NAME , lval != 0 , type ) );
		if ( lval ) 
		    p2word( lval );
#		ifdef DEBUG
		    if ( opt( 'k' ) ) {
			fprintf( stdout , "PCC_NAME | %3d | 0x%x	" 
			       , lval != 0 , type );
			if ( lval )
			    fprintf( stdout , "%d	" , lval );
		    }
#		endif
		p2name( name );
		break;
	    case PCC_REG:
		p2word( PCCM_TRIPLE( PCC_REG , rval , type ) );
#		ifdef DEBUG
		    if ( opt( 'k' ) ) {
			fprintf( stdout , "PCC_REG | %3d | 0x%x\n" ,
				rval , type );
		    }
#		endif
		break;
	}
    }

    /*
     *	rvalues are just lvalues with indirection, except
     *	special cases for registers and for named globals,
     *	whose names are their rvalues.
     */
putRV( name , level , offset , other_flags , type )
    char	*name;
    int		level;
    int		offset;
    char	other_flags;
    int		type;
    {
	char	extname[ BUFSIZ ];
	char	*printname;

	if ( !CGENNING )
	    return;
	if ( other_flags & NREGVAR ) {
	    if ( ( offset < 0 ) || ( offset > P2FP ) ) {
		panic( "putRV regvar" );
	    }
	    putleaf( PCC_REG , 0 , offset , type , (char *) 0 );
	    return;
	}
	if ( whereis( offset , other_flags ) == GLOBALVAR ) {
	    if ( name != 0 ) {
		if ( name[0] != '_' ) {
			sprintf( extname , EXTFORMAT , name );
			printname = extname;
		} else {
			printname = name;
		}
		putleaf( PCC_NAME , offset , 0 , type , printname );
		return;
	    } else {
		panic( "putRV no name" );
	    }
	}
	putLV( name , level , offset , other_flags , type );
	putop( PCCOM_UNARY PCC_MUL , type );
    }

    /*
     *	put out an lvalue 
     *	given a level and offset
     *	special case for
     *	    named globals, whose lvalues are just their names as constants.
     */
putLV( name , level , offset , other_flags , type )
    char	*name;
    int		level;
    int		offset;
    char	other_flags;
    int		type;
{
    char		extname[ BUFSIZ ];
    char		*printname;

    if ( !CGENNING )
	return;
    if ( other_flags & NREGVAR ) {
	panic( "putLV regvar" );
    }
    switch ( whereis( offset , other_flags ) ) {
	case GLOBALVAR:
	    if ( ( name != 0 ) ) {
		if ( name[0] != '_' ) {
			sprintf( extname , EXTFORMAT , name );
			printname = extname;
		} else {
			printname = name;
		}
		putleaf( PCC_ICON , offset , 0 , PCCM_ADDTYPE( type , PCCTM_PTR )
			, printname );
		return;
	    } else {
		panic( "putLV no name" );
	    }
	case PARAMVAR:
	    if ( level == cbn ) {
		putleaf( PCC_REG, 0, P2AP, PCCM_ADDTYPE( type , PCCTM_PTR ), (char *) 0 );
	    } else {
		putleaf( PCC_NAME , (level * sizeof(struct dispsave)) + AP_OFFSET
		    , 0 , PCCTM_PTR | PCCT_CHAR , DISPLAYNAME );
		parts[ level ] |= NONLOCALVAR;
	    }
	    putleaf( PCC_ICON , offset , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_PLUS , PCCTM_PTR | PCCT_CHAR );
	    break;
	case LOCALVAR:
	    if ( level == cbn ) {
		putleaf( PCC_REG, 0, P2FP, PCCM_ADDTYPE( type , PCCTM_PTR ), (char *) 0 );
	    } else {
		putleaf( PCC_NAME , (level * sizeof(struct dispsave)) + FP_OFFSET
		    , 0 , PCCTM_PTR | PCCT_CHAR , DISPLAYNAME );
		parts[ level ] |= NONLOCALVAR;
	    }
	    putleaf( PCC_ICON , -offset , 0 , PCCT_INT , (char *) 0 );
	    putop( PCC_MINUS , PCCTM_PTR | PCCT_CHAR );
	    break;
	case NAMEDLOCALVAR:
	    if ( level == cbn ) {
		putleaf( PCC_REG, 0, P2FP, PCCM_ADDTYPE( type , PCCTM_PTR ), (char *) 0 );
	    } else {
		putleaf( PCC_NAME , (level * sizeof(struct dispsave)) + FP_OFFSET
		    , 0 , PCCTM_PTR | PCCT_CHAR , DISPLAYNAME );
		parts[ level ] |= NONLOCALVAR;
	    }
	    putleaf( PCC_ICON , 0 , 0 , PCCT_INT , name );
	    putop( PCC_MINUS , PCCTM_PTR | PCCT_CHAR );
	    break;
    }
    return;
}

    /*
     *	put out a floating point constant leaf node
     *	the constant is declared in aligned data space
     *	and a PCC_NAME leaf put out for it
     */
putCON8( val )
    double	val;
    {
	char	*label;
	char	name[ BUFSIZ ];

	if ( !CGENNING )
	    return;
	label = getlab();
	putprintf( "	.data" , 0 );
	aligndot(A_DOUBLE);
	(void) putlab( label );
#	if defined(vax) || defined(tahoe)
	    putprintf( "	.double 0d%.20e" , 0 , val );
#	endif vax || tahoe
#	ifdef mc68000
	    putprintf( "	.long 	0x%x,0x%x", 0, val);
#	endif mc68000
	putprintf( "	.text" , 0 );
	sprintf( name , PREFIXFORMAT , LABELPREFIX , label );
	putleaf( PCC_NAME , 0 , 0 , PCCT_DOUBLE , name );
    }

	/*
	 * put out either an lvalue or an rvalue for a constant string.
	 * an lvalue (for assignment rhs's) is the name as a constant, 
	 * an rvalue (for parameters) is just the name.
	 */
putCONG( string , length , required )
    char	*string;
    int		length;
    int		required;
    {
	char	name[ BUFSIZ ];
	char	*label;
	char	*cp;
	int	pad;
	int	others;

	if ( !CGENNING )
	    return;
	putprintf( "	.data" , 0 );
	aligndot(A_STRUCT);
	label = getlab();
	(void) putlab( label );
	cp = string;
	while ( *cp ) {
	    putprintf( "	.byte	0%o" , 1 , *cp ++ );
	    for ( others = 2 ; ( others <= 8 ) && *cp ; others ++ ) {
		putprintf( ",0%o" , 1 , *cp++ );
	    }
	    putprintf( "" , 0 );
	}
	pad = length - strlen( string );
	while ( pad-- > 0 ) {
	    putprintf( "	.byte	0%o" , 1 , ' ' );
	    for ( others = 2 ; ( others <= 8 ) && ( pad-- > 0 ) ; others++ ) {
		putprintf( ",0%o" , 1 , ' ' );
	    }
	    putprintf( "" , 0 );
	}
	putprintf( "	.byte	0" , 0 );
	putprintf( "	.text"  , 0 );
	sprintf( name , PREFIXFORMAT , LABELPREFIX , label );
	if ( required == RREQ ) {
	    putleaf( PCC_NAME , 0 , 0 , PCCTM_ARY | PCCT_CHAR , name );
	} else {
	    putleaf( PCC_ICON , 0 , 0 , PCCTM_PTR | PCCT_CHAR , name );
	}
    }

    /*
     *	map a pascal type to a c type
     *	this would be tail recursive, but i unfolded it into a for (;;).
     *	this is sort of like isa and lwidth
     *	a note on the types used by the portable c compiler:
     *	    they are divided into a basic type (char, short, int, long, etc.)
     *	    and qualifications on those basic types (pointer, function, array).
     *	    the basic type is kept in the low 4 bits of the type descriptor,
     *	    and the qualifications are arranged in two bit chunks, with the
     *	    most significant on the right,
     *	    and the least significant on the left
     *		e.g. int *foo();
     *			(a function returning a pointer to an integer)
     *		is stored as
     *		    <ptr><ftn><int>
     *	so, we build types recursively
     *	also, we know that /lib/f1 can only deal with 6 qualifications
     *	so we stop the recursion there.  this stops infinite type recursion
     *	through mutually recursive pointer types.
     */
#define	MAXQUALS	6
int
p2type( np )
    struct nl	*np;
{

    return typerecur( np , 0 );
}
typerecur( np , quals )
    struct nl	*np;
    int		quals;
    {
	
	if ( np == NIL || quals > MAXQUALS ) {
	    return PCCT_UNDEF;
	}
	switch ( np -> class ) {
	    case SCAL :
	    case RANGE :
	    case CRANGE :
		if ( np -> type == ( nl + TDOUBLE ) ) {
		    return PCCT_DOUBLE;
		}
		switch ( bytes( np -> range[0] , np -> range[1] ) ) {
		    case 1:
			return PCCT_CHAR;
		    case 2:
			return PCCT_SHORT;
		    case 4:
			return PCCT_INT;
		    default:
			panic( "p2type int" );
			/* NOTREACHED */
		}
	    case STR :
		return ( PCCTM_ARY | PCCT_CHAR );
	    case RECORD :
	    case SET :
		return PCCT_STRTY;
	    case FILET :
		return ( PCCTM_PTR | PCCT_STRTY );
	    case CONST :
	    case VAR :
	    case FIELD :
		return p2type( np -> type );
	    case TYPE :
		switch ( nloff( np ) ) {
		    case TNIL :
			return ( PCCTM_PTR | PCCT_UNDEF );
		    case TSTR :
			return ( PCCTM_ARY | PCCT_CHAR );
		    case TSET :
			return PCCT_STRTY;
		    default :
			return ( p2type( np -> type ) );
		}
	    case REF:
	    case WITHPTR:
	    case PTR :
		return PCCM_ADDTYPE( typerecur( np -> type , quals + 1 ) , PCCTM_PTR );
	    case ARRAY :
		return PCCM_ADDTYPE( typerecur( np -> type , quals + 1 ) , PCCTM_ARY );
	    case FUNC :
		    /*
		     * functions are really pointers to functions
		     * which return their underlying type.
		     */
		return PCCM_ADDTYPE( PCCM_ADDTYPE( typerecur( np -> type , quals + 2 ) ,
					PCCTM_FTN ) , PCCTM_PTR );
	    case PROC :
		    /*
		     * procedures are pointers to functions 
		     * which return integers (whether you look at them or not)
		     */
		return PCCM_ADDTYPE( PCCM_ADDTYPE( PCCT_INT , PCCTM_FTN ) , PCCTM_PTR );
	    case FFUNC :
	    case FPROC :
		    /*
		     *	formal procedures and functions are pointers
		     *	to structures which describe their environment.
		     */
		return ( PCCTM_PTR | PCCT_STRTY );
	    default :
		panic( "p2type" );
		/* NOTREACHED */
	}
    }

    /*
     *	put a typed operator to the pcstream
     */
putop( op , type )
    int		op;
    int		type;
    {
	extern char	*p2opname();
	
	if ( !CGENNING )
	    return;
	p2word( PCCM_TRIPLE( op , 0 , type ) );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "%s (%d) |   0 | 0x%x\n"
			, p2opname( op ) , op , type );
	    }
#	endif
    }

    /*
     *	put out a structure operator (STASG, STARG, STCALL, UNARY STCALL )
     *	which looks just like a regular operator, only the size and
     *	alignment go in the next consecutive words
     */
putstrop( op , type , size , alignment )
    int	op;
    int	type;
    int	size;
    int	alignment;
    {
	extern char	*p2opname();
	
	if ( !CGENNING )
	    return;
	p2word( PCCM_TRIPLE( op , 0 , type ) );
	p2word( size );
	p2word( alignment );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "%s (%d) |   0 | 0x%x	%d %d\n"
			, p2opname( op ) , op , type , size , alignment );
	    }
#	endif
    }

    /*
     *	the string names of p2ops
     */

struct p2op {
    int op;
    char *name;
};

static struct p2op	p2opnames[] = {
	PCC_ERROR, "PCC_ERROR",
	PCC_NAME, "PCC_NAME",
	PCC_STRING, "PCC_STRING",
	PCC_ICON, "PCC_ICON",
	PCC_FCON, "PCC_FCON",
	PCC_PLUS, "PCC_PLUS",
	PCC_MINUS, "PCC_MINUS",
	PCC_UMINUS, "PCC_UMINUS",
	PCC_MUL, "PCC_MUL",
	PCC_DEREF, "PCC_DEREF",
	PCC_AND, "PCC_AND",
	PCC_ADDROF, "PCC_ADDROF",
	PCC_OR, "PCC_OR",
	PCC_ER, "PCC_ER",
	PCC_QUEST, "PCC_QUEST",
	PCC_COLON, "PCC_COLON",
	PCC_ANDAND, "PCC_ANDAND",
	PCC_OROR, "PCC_OROR",
	PCC_CM, "PCC_CM",
	PCC_ASSIGN, "PCC_ASSIGN",
	PCC_COMOP, "PCC_COMOP",
	PCC_DIV, "PCC_DIV",
	PCC_MOD, "PCC_MOD",
	PCC_LS, "PCC_LS",
	PCC_RS, "PCC_RS",
	PCC_DOT, "PCC_DOT",
	PCC_STREF, "PCC_STREF",
	PCC_CALL, "PCC_CALL",
	PCC_UCALL, "PCC_UCALL",
	PCC_FORTCALL, "PCC_FORTCALL",
	PCC_UFORTCALL, "PCC_UFORTCALL",
	PCC_NOT, "PCC_NOT",
	PCC_COMPL, "PCC_COMPL",
	PCC_INCR, "PCC_INCR",
	PCC_DECR, "PCC_DECR",
	PCC_EQ, "PCC_EQ",
	PCC_NE, "PCC_NE",
	PCC_LE, "PCC_LE",
	PCC_LT, "PCC_LT",
	PCC_GE, "PCC_GE",
	PCC_GT, "PCC_GT",
	PCC_ULE, "PCC_ULE",
	PCC_ULT, "PCC_ULT",
	PCC_UGE, "PCC_UGE",
	PCC_UGT, "PCC_UGT",
	PCC_REG, "PCC_REG",
	PCC_OREG, "PCC_OREG",
	PCC_CCODES, "PCC_CCODES",
	PCC_FREE, "PCC_FREE",
	PCC_STASG, "PCC_STASG",
	PCC_STARG, "PCC_STARG",
	PCC_STCALL, "PCC_STCALL",
	PCC_USTCALL, "PCC_USTCALL",
	PCC_FLD, "PCC_FLD",
	PCC_SCONV, "PCC_SCONV",
	PCC_PCONV, "PCC_PCONV",
	PCC_PMCONV, "PCC_PMCONV",
	PCC_PVCONV, "PCC_PVCONV",
	PCC_FORCE, "PCC_FORCE",
	PCC_CBRANCH, "PCC_CBRANCH",
	PCC_INIT, "PCC_INIT",
	PCC_CAST, "PCC_CAST",
	-1, ""
    };

char *
p2opname( op )
    register int	op;
    {
	static char		*p2map[PCC_MAXOP+1];
	static bool		mapready = FALSE;
	register struct p2op	*pp;

	if ( mapready == FALSE ) {
	    for ( pp = p2opnames; pp->op >= 0; pp++ )
		p2map[ pp->op ] = pp->name;
	    mapready = TRUE;
	}
	return ( p2map[ op ] ? p2map[ op ] : "unknown" );
    }

    /*
     *	low level routines
     */

    /*
     *	puts a long word on the pcstream
     */
p2word( word )
    int		word;
    {

	putw( word , pcstream );
    }

    /*
     *	put a length 0 mod 4 null padded string onto the pcstream
     */
p2string( string )
    char	*string;
    {
	int	slen = strlen( string );
	int	wlen = ( slen + 3 ) / 4;
	int	plen = ( wlen * 4 ) - slen;
	char	*cp;
	int	p;

	for ( cp = string ; *cp ; cp++ )
	    putc( *cp , pcstream );
	for ( p = 1 ; p <= plen ; p++ )
	    putc( '\0' , pcstream );
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , "\"%s" , string );
		for ( p = 1 ; p <= plen ; p++ )
		    fprintf( stdout , "\\0" );
		fprintf( stdout , "\"\n" );
	    }
#	endif
    }

    /*
     *	puts a name on the pcstream
     */
p2name( name )
    char	*name;
    {
	int	pad;

	fprintf( pcstream , NAMEFORMAT , name );
	pad = strlen( name ) % sizeof (long);
	for ( ; pad < sizeof (long) ; pad++ ) {
	    putc( '\0' , pcstream );
	}
#	ifdef DEBUG
	    if ( opt( 'k' ) ) {
		fprintf( stdout , NAMEFORMAT , name );
		pad = strlen( name ) % sizeof (long);
		for ( ; pad < sizeof (long) ; pad++ ) {
		    fprintf( stdout , "\\0" );
		}
		fprintf( stdout , "\n" );
	    }
#	endif
    }
    
    /*
     *	put out a jump to a label
     */
putjbr( label )
    long	label;
    {

	printjbr( LABELPREFIX , label );
    }

    /*
     *	put out a jump to any kind of label
     */
printjbr( prefix , label )
    char	*prefix;
    long	label;
    {

#	if defined(vax) || defined(tahoe)
	    putprintf( "	jbr	" , 1 );
	    putprintf( PREFIXFORMAT , 0 , prefix , label );
#	endif vax || tahoe
#	ifdef mc68000
	    putprintf( "	jra	" , 1 );
	    putprintf( PREFIXFORMAT , 0 , prefix , label );
#	endif mc68000
    }

    /*
     *	another version of put to catch calls to put
     */
/* VARARGS */
put()
    {

	panic("put()");
    }

#endif PC
