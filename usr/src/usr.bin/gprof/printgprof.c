#ifndef lint
    static	char *sccsid = "@(#)printgprof.c	1.8 (Berkeley) %G%";
#endif lint

#include "gprof.h"

printprof()
{
    register nltype	*np;
    nltype		**sortednlp;
    int			index;

    printf( "\ngranularity: each sample hit covers %d byte(s)" , (long) scale );
    printf( " for %.2f%% of %.2f seconds\n\n" , 100.0/totime , totime / HZ );
    actime = 0.0;
    flatprofheader();
	/*
	 *	Sort the symbol table in by time
	 */
    sortednlp = (nltype **) calloc( nname , sizeof(nltype *) );
    if ( sortednlp == (nltype **) 0 ) {
	fprintf( stderr , "[printprof] ran out of memory for time sorting\n" );
    }
    for ( index = 0 ; index < nname ; index += 1 ) {
	sortednlp[ index ] = &nl[ index ];
    }
    qsort( sortednlp , nname , sizeof(nltype *) , timecmp );
    for ( index = 0 ; index < nname ; index += 1 ) {
	np = sortednlp[ index ];
	flatprofline( np );
    }
    actime = 0.0;
}

timecmp( npp1 , npp2 )
    nltype **npp1, **npp2;
{
    double	timediff;
    long	calldiff;

    timediff = (*npp2) -> time - (*npp1) -> time;
    if ( timediff > 0.0 )
	return 1 ;
    if ( timediff < 0.0 )
	return -1;
    calldiff = (*npp2) -> ncall - (*npp1) -> ncall;
    if ( calldiff > 0 )
	return 1;
    if ( calldiff < 0 )
	return -1;
    return( strcmp( (*npp1) -> name , (*npp2) -> name ) );
}

    /*
     *	header for flatprofline
     */
flatprofheader()
{
    
    if ( bflag ) {
	printblurb( "flat.blurb" );
    }
    printf( "%5.5s %7.7s %7.7s %7.7s %-8.8s\n" ,
	    "%time" , "cumsecs" , "seconds" , "calls" , "name" );
}

flatprofline( np )
    register nltype	*np;
{

    if ( zflag == 0 && np -> ncall == 0 && np -> time == 0 ) {
	return;
    }
    actime += np -> time;
    printf( "%5.1f %7.2f %7.2f" ,
	100 * np -> time / totime , actime / HZ , np -> time / HZ );
    if ( np -> ncall != 0 ) {
	printf( " %7d" , np -> ncall );
    } else {
	printf( " %7.7s" , "" );
    }
    printf( " %s\n" , np -> name );
}

gprofheader()
{

    if ( bflag ) {
	printblurb( "callg.blurb" );
    }
    printf( "%6.6s %5.5s %7.7s %11.11s %7.7s/%-7.7s     %-8.8s\n" ,
	"" , "" , "" , "" , "called" , "total" , "parents" , "" );
    printf( "%-6.6s %5.5s %7.7s %11.11s %7.7s+%-7.7s %-8.8s\t%5.5s\n" ,
	"index" , "%time" , "self" , "descendents" ,
	"called" , "self" , "name" , "index" );
    printf( "%6.6s %5.5s %7.7s %11.11s %7.7s/%-7.7s     %-8.8s\n" ,
	"" , "" , "" , "" , "called" , "total" , "children" , "" );
    printf( "\n" );
}

gprofline( np )
    register nltype	*np;
{
    char	kirkbuffer[ BUFSIZ ];

    sprintf( kirkbuffer , "[%d]" , np -> index );
    printf( "%-6.6s %5.1f %7.2f %11.2f" ,
	    kirkbuffer ,
	    100 * ( np -> time + np -> childtime ) / totime ,
	    np -> time / HZ ,
	    np -> childtime / HZ );
    if ( ( np -> ncall + np -> selfcalls ) != 0 ) {
	printf( " %7d" , np -> ncall );
	if ( np -> selfcalls != 0 ) {
	    printf( "+%-7d " , np -> selfcalls );
	} else {
	    printf( " %7.7s " , "" );
	}
    } else {
	printf( " %7.7s %7.7s " , "" , "" );
    }
    printname( np );
    printf( "\n" );
}

printgprof()
{
    nltype	**timesortnlp;
    int		index;
    nltype	*parentp;

	/*
	 *	Now, sort by time + childtime.
	 *	sorting both the regular function names
	 *	and cycle headers.
	 */
    timesortnlp = (nltype **) calloc( nname + ncycle , sizeof(nltype *) );
    if ( timesortnlp == (nltype **) 0 ) {
	fprintf( stderr , "%s: ran out of memory for sorting\n" , whoami );
    }
    for ( index = 0 ; index < nname ; index++ ) {
	timesortnlp[index] = &nl[index];
    }
    for ( index = 1 ; index <= ncycle ; index++ ) {
	timesortnlp[nname+index-1] = &cyclenl[index];
    }
    qsort( timesortnlp , nname + ncycle , sizeof(nltype *) , totalcmp );
    for ( index = 0 ; index < nname + ncycle ; index++ ) {
	timesortnlp[ index ] -> index = index + 1;
    }
	/*
	 *	Now, print out the structured profiling list
	 */
    printf( "\f\n" );
    gprofheader();
    for ( index = 0 ; index < nname + ncycle ; index ++ ) {
	parentp = timesortnlp[ index ];
	if ( zflag == 0 &&
	     parentp -> ncall == 0 &&
	     parentp -> selfcalls == 0 &&
	     parentp -> time == 0 &&
	     parentp -> childtime == 0 ) {
	    continue;
	}
	if ( parentp -> name == 0 && parentp -> cycleno != 0 ) {
		/*
		 *	cycle header
		 */
	    printcycle( parentp );
	    printmembers( parentp );
	} else {
	    printparents( parentp );
	    gprofline( parentp );
	    printchildren( parentp );
	}
	printf( "\n" );
	printf( "-----------------------------------------------\n" );
	printf( "\n" );
    }
}

    /*
     *	sort by decreasing total time (time+childtime)
     *	if times are equal, but one is a cycle header,
     *		say that's first (e.g. less, i.e. -1).
     *	if one's name doesn't have an underscore and the other does,
     *		say the one is first.
     *	all else being equal, sort by names.
     */
int
totalcmp( npp1 , npp2 )
    nltype	**npp1;
    nltype	**npp2;
{
    register nltype	*np1 = *npp1;
    register nltype	*np2 = *npp2;
    double		diff;

    diff =    ( np1 -> time + np1 -> childtime )
	    - ( np2 -> time + np2 -> childtime );
    if ( diff < 0.0 )
	    return 1;
    if ( diff > 0.0 )
	    return -1;
    if ( np1 -> name == 0 && np1 -> cycleno != 0 ) 
	return -1;
    if ( np2 -> name == 0 && np2 -> cycleno != 0 )
	return 1;
    if ( np1 -> name == 0 )
	return -1;
    if ( np2 -> name == 0 )
	return 1;
    if ( *(np1 -> name) != '_' && *(np2 -> name) == '_' )
	return -1;
    if ( *(np1 -> name) == '_' && *(np2 -> name) != '_' )
	return 1;
    return strcmp( np1 -> name , np2 -> name );
}

printparents( childp )
    nltype	*childp;
{
    nltype	*parentp;
    arctype	*arcp;
    nltype	*cycleheadp;

    if ( childp -> cyclehead != 0 ) {
	cycleheadp = childp -> cyclehead;
    } else {
	cycleheadp = childp;
    }
    if ( childp -> parents == 0 ) {
	printf( "%6.6s %5.5s %7.7s %11.11s %7.7s %7.7s     <spontaneous>\n" ,
		"" , "" , "" , "" , "" , "" );
	return;
    }
    sortparents( childp );
    for ( arcp = childp -> parents ; arcp ; arcp = arcp -> arc_parentlist ) {
	parentp = arcp -> arc_parentp;
	if ( childp == parentp ||
	     ( childp->cycleno != 0 && parentp->cycleno == childp->cycleno ) ) {
		/*
		 *	selfcall or call among siblings
		 */
	    printf( "%6.6s %5.5s %7.7s %11.11s %7d %7.7s     " ,
		    "" , "" , "" , "" ,
		    arcp -> arc_count , "" );
	    printname( parentp );
	    printf( "\n" );
	} else {
		/*
		 *	regular parent of child
		 */
	    printf( "%6.6s %5.5s %7.2f %11.2f %7d/%-7d     " ,
		    "" , "" ,
		    arcp -> arc_time / HZ , arcp -> arc_childtime / HZ ,
		    arcp -> arc_count , cycleheadp -> ncall );
	    printname( parentp );
	    printf( "\n" );
	}
    }
}

printchildren( parentp )
    nltype	*parentp;
{
    nltype	*childp;
    arctype	*arcp;

    sortchildren( parentp );
    arcp = parentp -> children;
    for ( arcp = parentp -> children ; arcp ; arcp = arcp -> arc_childlist ) {
	childp = arcp -> arc_childp;
	if ( childp == parentp ||
	    ( childp->cycleno != 0 && childp->cycleno == parentp->cycleno ) ) {
		/*
		 *	self call or call to sibling
		 */
	    printf( "%6.6s %5.5s %7.7s %11.11s %7d %7.7s     " ,
		    "" , "" , "" , "" , arcp -> arc_count , "" );
	    printname( childp );
	    printf( "\n" );
	} else {
		/*
		 *	regular child of parent
		 */
	    printf( "%6.6s %5.5s %7.2f %11.2f %7d/%-7d     " ,
		    "" , "" ,
		    arcp -> arc_time / HZ , arcp -> arc_childtime / HZ ,
		    arcp -> arc_count , childp -> cyclehead -> ncall );
	    printname( childp );
	    printf( "\n" );
	}
    }
}

printname( selfp )
    nltype	*selfp;
{

    if ( selfp -> name != 0 ) {
	printf( "%s" , selfp -> name );
#	ifdef DEBUG
	    if ( debug & DFNDEBUG ) {
		printf( "{%d} " , selfp -> toporder );
	    }
#	endif DEBUG
    }
    if ( selfp -> cycleno != 0 ) {
	printf( "\t<cycle %d>" , selfp -> cycleno );
    }
    if ( selfp -> index != 0 ) {
	printf( " [%d]" , selfp -> index );
    }
}

sortchildren( parentp )
    nltype	*parentp;
{
    arctype	*arcp;
    arctype	*detachedp;
    arctype	sorted;
    arctype	*prevp;

	/*
	 *	unlink children from parent,
	 *	then insertion sort back on to sorted's children.
	 *	    *arcp	the arc you have detached and are inserting.
	 *	    *detachedp	the rest of the arcs to be sorted.
	 *	    sorted	arc list onto which you insertion sort.
	 *	    *prevp	arc before the arc you are comparing.
	 */
    sorted.arc_childlist = 0;
    for (   arcp = parentp -> children , detachedp = arcp -> arc_childlist ;
	    arcp ;
	    arcp = detachedp , detachedp = detachedp -> arc_childlist ) {
	    /*
	     *	consider *arcp as disconnected
	     *	insert it into sorted
	     */
	for (   prevp = &sorted ;
		prevp -> arc_childlist ;
		prevp = prevp -> arc_childlist ) {
	    if ( arccmp( arcp , prevp -> arc_childlist ) != LESSTHAN ) {
		break;
	    }
	}
	arcp -> arc_childlist = prevp -> arc_childlist;
	prevp -> arc_childlist = arcp;
    }
	/*
	 *	reattach sorted children to parent
	 */
    parentp -> children = sorted.arc_childlist;
}

sortparents( childp )
    nltype	*childp;
{
    arctype	*arcp;
    arctype	*detachedp;
    arctype	sorted;
    arctype	*prevp;

	/*
	 *	unlink parents from child,
	 *	then insertion sort back on to sorted's parents.
	 *	    *arcp	the arc you have detached and are inserting.
	 *	    *detachedp	the rest of the arcs to be sorted.
	 *	    sorted	arc list onto which you insertion sort.
	 *	    *prevp	arc before the arc you are comparing.
	 */
    sorted.arc_parentlist = 0;
    for (   arcp = childp -> parents , detachedp = arcp -> arc_parentlist ;
	    arcp ;
	    arcp = detachedp , detachedp = detachedp -> arc_parentlist ) {
	    /*
	     *	consider *arcp as disconnected
	     *	insert it into sorted
	     */
	for (   prevp = &sorted ;
		prevp -> arc_parentlist ;
		prevp = prevp -> arc_parentlist ) {
	    if ( arccmp( arcp , prevp -> arc_parentlist ) != GREATERTHAN ) {
		break;
	    }
	}
	arcp -> arc_parentlist = prevp -> arc_parentlist;
	prevp -> arc_parentlist = arcp;
    }
	/*
	 *	reattach sorted arcs to child
	 */
    childp -> parents = sorted.arc_parentlist;
}

    /*
     *	print a cycle header
     */
printcycle( cyclep )
    nltype	*cyclep;
{
    char	kirkbuffer[ BUFSIZ ];

    sprintf( kirkbuffer , "[%d]" , cyclep -> index );
    printf( "%-6.6s %5.1f %7.2f %11.2f %7d" ,
	    kirkbuffer ,
	    100 * ( cyclep -> time + cyclep -> childtime ) / totime ,
	    cyclep -> time / HZ ,
	    cyclep -> childtime / HZ ,
	    cyclep -> ncall );
    if ( cyclep -> selfcalls != 0 ) {
	printf( "+%-7d" , cyclep -> selfcalls );
    } else {
	printf( " %7.7s" , "" );
    }
    printf( " <cycle %d as a whole>\t[%d]\n" ,
	    cyclep -> cycleno , cyclep -> index );
}

    /*
     *	print the members of a cycle
     */
printmembers( cyclep )
    nltype	*cyclep;
{
    nltype	*memberp;

    sortmembers( cyclep );
    for ( memberp = cyclep -> cnext ; memberp ; memberp = memberp -> cnext ) {
	printf( "%6.6s %5.5s %7.2f %11.2f %7d" , 
		"" , "" , memberp -> time / HZ , memberp -> childtime / HZ ,
		memberp -> ncall );
	if ( memberp -> selfcalls != 0 ) {
	    printf( "+%-7d" , memberp -> selfcalls );
	} else {
	    printf( " %7.7s" , "" );
	}
	printf( "     " );
	printname( memberp );
	printf( "\n" );
    }
}

    /*
     *	sort members of a cycle
     */
sortmembers( cyclep )
    nltype	*cyclep;
{
    nltype	*todo;
    nltype	*doing;
    nltype	*prev;

	/*
	 *	detach cycle members from cyclehead,
	 *	and insertion sort them back on.
	 */
    todo = cyclep -> cnext;
    cyclep -> cnext = 0;
    for (   doing = todo , todo = doing -> cnext ;
	    doing ;
	    doing = todo , todo = doing -> cnext ) {
	for ( prev = cyclep ; prev -> cnext ; prev = prev -> cnext ) {
	    if ( membercmp( doing , prev -> cnext ) == GREATERTHAN ) {
		break;
	    }
	}
	doing -> cnext = prev -> cnext;
	prev -> cnext = doing;
    }
}

    /*
     *	major sort is on time + childtime,
     *	next is sort on ncalls + selfcalls.
     */
int
membercmp( this , that )
    nltype	*this;
    nltype	*that;
{
    double	thistime = this -> time + this -> childtime;
    double	thattime = that -> time + that -> childtime;
    long	thiscalls = this -> ncall + this -> selfcalls;
    long	thatcalls = that -> ncall + that -> selfcalls;

    if ( thistime > thattime ) {
	return GREATERTHAN;
    }
    if ( thistime < thattime ) {
	return LESSTHAN;
    }
    if ( thiscalls > thatcalls ) {
	return GREATERTHAN;
    }
    if ( thiscalls < thatcalls ) {
	return LESSTHAN;
    }
    return EQUALTO;
}
    /*
     *	compare two arcs to/from the same child/parent.
     *	- if one arc is a self arc, it's least.
     *	- if one arc is within a cycle, it's less than.
     *	- if both arcs are within a cycle, compare arc counts.
     *	- if neither arc is within a cycle, compare with
     *		time + childtime as major key
     *		arc count as minor key
     */
int
arccmp( thisp , thatp )
    arctype	*thisp;
    arctype	*thatp;
{
    nltype	*thisparentp = thisp -> arc_parentp;
    nltype	*thischildp = thisp -> arc_childp;
    nltype	*thatparentp = thatp -> arc_parentp;
    nltype	*thatchildp = thatp -> arc_childp;
    double	thistime;
    double	thattime;

#   ifdef DEBUG
	if ( debug & TIMEDEBUG ) {
	    printf( "[arccmp] " );
	    printname( thisparentp );
	    printf( " calls " );
	    printname ( thischildp );
	    printf( " %f + %f %d/%d\n" ,
		    thisp -> arc_time , thisp -> arc_childtime ,
		    thisp -> arc_count , thischildp -> ncall );
	    printf( "[arccmp] " );
	    printname( thatparentp );
	    printf( " calls " );
	    printname( thatchildp );
	    printf( " %f + %f %d/%d\n" ,
		    thatp -> arc_time , thatp -> arc_childtime ,
		    thatp -> arc_count , thatchildp -> ncall );
	    printf( "\n" );
	}
#   endif DEBUG
    if ( thisparentp == thischildp ) {
	    /* this is a self call */
	return LESSTHAN;
    }
    if ( thatparentp == thatchildp ) {
	    /* that is a self call */
	return GREATERTHAN;
    }
    if ( thisparentp -> cycleno != 0 && thischildp -> cycleno != 0 &&
	thisparentp -> cycleno == thischildp -> cycleno ) {
	    /* this is a call within a cycle */
	if ( thatparentp -> cycleno != 0 && thatchildp -> cycleno != 0 &&
	    thatparentp -> cycleno == thatchildp -> cycleno ) {
		/* that is a call within the cycle, too */
	    if ( thisp -> arc_count < thatp -> arc_count ) {
		return LESSTHAN;
	    }
	    if ( thisp -> arc_count > thatp -> arc_count ) {
		return GREATERTHAN;
	    }
	    return EQUALTO;
	} else {
		/* that isn't a call within the cycle */
	    return LESSTHAN;
	}
    } else {
	    /* this isn't a call within a cycle */
	if ( thatparentp -> cycleno != 0 && thatchildp -> cycleno != 0 &&
	    thatparentp -> cycleno == thatchildp -> cycleno ) {
		/* that is a call within a cycle */
	    return GREATERTHAN;
	} else {
		/* neither is a call within a cycle */
	    thistime = thisp -> arc_time + thisp -> arc_childtime;
	    thattime = thatp -> arc_time + thatp -> arc_childtime;
	    if ( thistime < thattime )
		return LESSTHAN;
	    if ( thistime > thattime )
		return GREATERTHAN;
	    if ( thisp -> arc_count < thatp -> arc_count )
		return LESSTHAN;
	    if ( thisp -> arc_count > thatp -> arc_count )
		return GREATERTHAN;
	    return EQUALTO;
	}
    }
}

printblurb( blurbname )
    char	*blurbname;
{
    char	pathname[ BUFSIZ ];
    FILE	*blurbfile;
    int		input;

    sprintf( pathname , "%s%s" , BLURBLIB , blurbname );
    blurbfile = fopen( pathname , "r" );
    if ( blurbfile == NULL ) {
	perror( pathname );
	return;
    }
    while ( ( input = getc( blurbfile ) ) != EOF ) {
	putchar( input );
    }
    fclose( blurbfile );
}
