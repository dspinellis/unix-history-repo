#ifndef lint
    static	char *sccsid = "@(#)printgprof.c	1.2 (Berkeley) %G%";
#endif lint

#include "gprof.h"

printgprof()
{
    nltype	**timesortnlp;
    int		index;
    nltype	*parentp;
    nltype	*childp;

	/*
	 *	Now, sort by time + childtime.
	 *	include the cycle headers hiding out past nl[nname].
	 */
    timesortnlp = (nltype **) calloc( nname+1+cyclemax , sizeof(nltype *) );
    if ( timesortnlp == (nltype **) 0 ) {
	fprintf( stderr , "[doarcs] ran out of memory for sorting\n" );
    }
    for ( index = 0 ; index < nname+1+cyclemax ; index++ ) {
	timesortnlp[index] = &nl[index];
    }
    qsort( timesortnlp , nname+1+cyclemax , sizeof(nltype *) , totalcmp );
    for ( index = 0 ; index < nname+1+cyclemax ; index++ ) {
	timesortnlp[ index ] -> index = index + 1;
    }
	/*
	 *	Now, print out the structured profiling list
	 */
    actime = 0.0;
    printf( "\f" );
    putprofheader();
    for ( index = 0 ; index < nname + 1 + cyclemax ; index ++ ) {
	parentp = timesortnlp[ index ];
	if ( zflg == 0 &&
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
	    putprofline( parentp , 0 );
	    for ( childp = parentp->cnext ; childp ; childp = childp->cnext ) {
		putprofline( childp , 0 );
	    }
	} else {
	    printparents( parentp );
	    putprofline( parentp , 1 );
	    printchildren( parentp );
	}
	printf( "\n" );
    }
    actime = 0.0;
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
	printf( "\t%5.5s %7.7s %7.7s %7.7s %7.7s %7.7s      <spontaneous>\n" ,
		"" , "" , "" , "" , "" , "" );
	return;
    }
    sortparents( childp );
    for ( arcp = childp -> parents ; arcp ; arcp = arcp -> arc_parentlist ) {
	parentp = arcp -> arc_parentp;
	if ( childp == parentp ||
	     ( childp->cycleno != 0 && parentp->cycleno == childp->cycleno ) ) {
		/*
		 *	selfcall or call amoung siblings
		 */
	    printf( "\t%5.5s %7.7s %7.7s %7.7s %7d %7.7s      " ,
		    "" , "" , "" , "" ,
		    arcp -> arc_count , "" );
	    printname( parentp );
	    printf( "\n" );
	} else {
		/*
		 *	regular parent of child
		 */
	    printf( "\t%5.5s %7.7s %7.1f %7.1f %7d/%-7d      " , "" , "" ,
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
	    printf( "\t%5.5s %7.7s %7.7s %7.7s %7d %7.7s      " ,
		    "" , "" , "" , "" ,
		    arcp -> arc_count , "" );
	    printname( childp );
	    printf( "\n" );
	} else {
		/*
		 *	regular child of parent
		 */
	    printf( "\t%5.5s %7.7s %7.1f %7.1f %7d/%-7d      " , "" , "" ,
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
	printf( "%s\t" , selfp -> name );
	if ( selfp -> index != 0 ) {
	    printf( "[%d] " , selfp -> index );
	}
#	ifdef DEBUG
	    if ( debug & DFNDEBUG ) {
		printf( "{%d} " , selfp -> toporder );
	    }
#	endif DEBUG
    }
    if ( selfp -> cycleno != 0 ) {
	printf( "<cycle %d>" , selfp -> cycleno );
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
