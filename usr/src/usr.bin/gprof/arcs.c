#ifndef lint
    static	char *sccsid = "@(#)arcs.c	1.7 (Berkeley) %G%";
#endif lint

#include "gprof.h"

    /*
     *	add (or just increment) an arc
     */
addarc( parentp , childp , count )
    nltype	*parentp;
    nltype	*childp;
    long	count;
{
    arctype		*calloc();
    arctype		*arcp;

#   ifdef DEBUG
	if ( debug & TALLYDEBUG ) {
	    printf( "[addarc] %d arcs from %s to %s\n" ,
		    count , parentp -> name , childp -> name );
	}
#   endif DEBUG
    arcp = arclookup( parentp , childp );
    if ( arcp != 0 ) {
	    /*
	     *	a hit:  just increment the count.
	     */
#	ifdef DEBUG
	    if ( debug & TALLYDEBUG ) {
		printf( "[tally] hit %d += %d\n" ,
			arcp -> arc_count , count );
	    }
#	endif DEBUG
	arcp -> arc_count += count;
	return;
    }
    arcp = calloc( 1 , sizeof *arcp );
    arcp -> arc_parentp = parentp;
    arcp -> arc_childp = childp;
    arcp -> arc_count = count;
	/*
	 *	prepend this child to the children of this parent
	 */
    arcp -> arc_childlist = parentp -> children;
    parentp -> children = arcp;
	/*
	 *	prepend this parent to the parents of this child
	 */
    arcp -> arc_parentlist = childp -> parents;
    childp -> parents = arcp;
}

    /*
     *	the code below topologically sorts the graph (collapsing cycles),
     *	and propagates time bottom up and flags top down.
     */

    /*
     *	the topologically sorted name list pointers
     */
nltype	**topsortnlp;

topcmp( npp1 , npp2 )
    nltype	**npp1;
    nltype	**npp2;
{
    return (*npp1) -> toporder - (*npp2) -> toporder;
}

doarcs()
{
    nltype	*parentp;
    arctype	*arcp;
    long	index;

	/*
	 *	initialize various things:
	 *	    zero out child times.
	 *	    count self-recursive calls.
	 *	    indicate that nothing is on cycles.
	 */
    for ( parentp = nl ; parentp < npe ; parentp++ ) {
	parentp -> childtime = 0.0;
	arcp = arclookup( parentp , parentp );
	if ( arcp != 0 ) {
	    parentp -> ncall -= arcp -> arc_count;
	    parentp -> selfcalls = arcp -> arc_count;
	} else {
	    parentp -> selfcalls = 0;
	}
	parentp -> printflag = FALSE;
	parentp -> toporder = 0;
	parentp -> cycleno = 0;
	parentp -> cyclehead = parentp;
	parentp -> cnext = 0;
	if ( cflag ) {
	    findcalls( parentp , parentp -> value , (parentp+1) -> value );
	}
    }
	/*
	 *	time for functions which print is accumulated here.
	 */
    printtime = 0.0;
	/*
	 *	topologically order things
	 *	from each of the roots of the call graph
	 */
    for ( parentp = nl ; parentp < npe ; parentp++ ) {
	if ( parentp -> parents == 0 ) {
	    dfn( parentp );
	}
    }
	/*
	 *	link together nodes on the same cycle
	 */
    cyclelink();
	/*
	 *	Sort the symbol table in reverse topological order
	 */
    topsortnlp = (nltype **) calloc( nname , sizeof(nltype *) );
    if ( topsortnlp == (nltype **) 0 ) {
	fprintf( stderr , "[doarcs] ran out of memory for topo sorting\n" );
    }
    for ( index = 0 ; index < nname ; index += 1 ) {
	topsortnlp[ index ] = &nl[ index ];
    }
    qsort( topsortnlp , nname , sizeof(nltype *) , topcmp );
#   ifdef DEBUG
	if ( debug & DFNDEBUG ) {
	    printf( "[doarcs] topological sort listing\n" );
	    for ( index = 0 ; index < nname ; index += 1 ) {
		printf( "[doarcs] " );
		printf( "%d:" , topsortnlp[ index ] -> toporder );
		printname( topsortnlp[ index ] );
		printf( "\n" );
	    }
	}
#   endif DEBUG
	/*
	 *	starting from the topological bottom, 
	 *	propogate children times up to parents.
	 */
    dotime();
	/*
	 *	starting from the topological top,
	 *	propagate print flags to children.
	 */
    doflags();
    printgprof();
}

dotime()
{
    int	index;

    cycletime();
    for ( index = 0 ; index < nname ; index += 1 ) {
	timepropagate( topsortnlp[ index ] );
    }
}

timepropagate( parentp )
    nltype	*parentp;
{
    arctype	*arcp;
    nltype	*childp;
    double	share;

	/*
	 *	gather time from children of this parent.
	 */
    for ( arcp = parentp -> children ; arcp ; arcp = arcp -> arc_childlist ) {
	childp = arcp -> arc_childp;
#	ifdef DEBUG
	    if ( debug & ARCDEBUG ) {
		printf( "[propagate] " );
		printname( parentp );
		printf( " calls " );
		printname( childp );
		printf( " %d (%d) times\n" ,
			arcp -> arc_count , childp -> ncall );
	    }
#	endif DEBUG
	if ( arcp -> arc_count == 0 ) {
	    continue;
	}
	if ( childp -> ncall == 0 ) {
	    continue;
	}
	if ( childp == parentp ) {
	    continue;
	}
	if ( childp -> cyclehead != childp ) {
	    if ( parentp -> cycleno == childp -> cycleno ) {
		continue;
	    }
#	    ifdef DEBUG
		if ( debug & ARCDEBUG ) {
		    printf( "[propagate]\t it's a call into cycle %d\n" ,
			    childp -> cycleno );
		}
#	    endif DEBUG
	    if ( parentp -> toporder <= childp -> toporder ) {
		fprintf( stderr , "[propagate] toporder botches\n" );
	    }
	    childp = childp -> cyclehead;
	} else {
	    if ( parentp -> toporder <= childp -> toporder ) {
		fprintf( stderr , "[propagate] toporder botches\n" );
		continue;
	    }
	}
	    /*
	     *	distribute time for this arc
	     */
	arcp -> arc_time = childp -> time *
			    ( ( (double) arcp -> arc_count ) /
			    ( (double) childp -> ncall ) );
	arcp -> arc_childtime = childp -> childtime *
			    ( ( (double) arcp -> arc_count ) /
			    ( (double) childp -> ncall ) );
	share = arcp -> arc_time + arcp -> arc_childtime;
#	ifdef DEBUG
	    if ( debug & ARCDEBUG ) {
		printf( "[propagate]\t " );
		printname( childp );
		printf( " time %8.2f + childtime %8.2f\n" ,
		    childp -> time , childp -> childtime );
		printf( "[propagate]\t this is %d arcs of the %d calls\n",
		    arcp -> arc_count , childp -> ncall );
		printf( "[propagate]\t so this gives %8.2f+%8.2f to %s\n" ,
		    arcp -> arc_time , arcp -> arc_childtime ,
		    parentp -> name );
	    }
#	endif DEBUG
	parentp -> childtime += share;
	    /*
	     *	add this share to the cycle header, if any
	     */
	if ( parentp -> cyclehead != parentp ) {
#	    ifdef DEBUG
		if ( debug & ARCDEBUG ) {
		    printf( "[propagate]\t and to cycle %d\n" ,
			    parentp -> cycleno );
		}
#	    endif DEBUG
	    parentp -> cyclehead -> childtime += share;
	}
    }
}

cyclelink()
{
    register nltype	*nlp;
    register nltype	*cyclenlp;
    int			cycle;
    nltype		*memberp;

	/*
	 *	Count the number of cycles, and initialze the cycle lists
	 */
    ncycle = 0;
    for ( nlp = nl ; nlp < npe ; nlp++ ) {
	    /*
	     *	this is how you find unattached cycles
	     */
	if ( nlp -> cyclehead == nlp && nlp -> cnext != 0 ) {
	    ncycle += 1;
	}
    }
	/*
	 *	cyclenl is indexed by cycle number:
	 *	i.e. it is origin 1, not origin 0.
	 */
    cyclenl = (nltype *) calloc( ncycle + 1 , sizeof( nltype ) );
    if ( cyclenl == 0 ) {
	fprintf( stderr , "%s: No room for %d bytes of cycle headers\n" ,
		whoami , ( ncycle + 1 ) * sizeof( nltype ) );
	done();
    }
	/*
	 *	now link cycles to true cycleheads,
	 *	number them, accumulate the data for the cycle
	 */
    cycle = 0;
    for ( nlp = nl ; nlp < npe ; nlp++ ) {
	if ( nlp -> cyclehead != nlp || nlp -> cnext == 0 ) {
	    continue;
	}
	cycle += 1;
	cyclenlp = &cyclenl[cycle];
	cyclenlp -> cycleno = cycle;
	cyclenlp -> cyclehead = cyclenlp;
	cyclenlp -> cnext = nlp;
#	ifdef DEBUG
	    if ( debug & CYCLEDEBUG ) {
		printf( "[cyclelink] " );
		printname( nlp );
		printf( " is the head of cycle %d\n" , cycle );
	    }
#	endif DEBUG
	for ( memberp = nlp ; memberp ; memberp = memberp -> cnext ) { 
	    memberp -> cycleno = cycle;
	    memberp -> cyclehead = cyclenlp;
	}
    }
}

cycletime()
{
    int			cycle;
    nltype		*cyclenlp;
    nltype		*parentp;
    nltype		*childp;
    arctype		*arcp;
    long		ncall;
    double		time;
    long		callsamong;

    for ( cycle = 1 ; cycle <= ncycle ; cycle += 1 ) {
	cyclenlp = &cyclenl[ cycle ];
	    /*
	     *	n-squaredly (in the size of the cycle)
	     *	find all the call within the cycle 
	     *	(including self-recursive calls)
	     *	and remove them, thus making the cycle into
	     *	`node' with calls only from the outside.
	     *	note: that this doesn't deal with
	     *	self-recursive calls outside cycles (sigh).
	     */
	callsamong = 0;
	for ( parentp = cyclenlp->cnext; parentp; parentp = parentp->cnext ) {
	    for ( childp = cyclenlp->cnext; childp; childp = childp -> cnext ) {
		if ( parentp == childp ) {
		    continue;
		}
		arcp = arclookup( parentp , childp );
		if ( arcp != 0 ) {
		    callsamong += arcp -> arc_count;
#		    ifdef DEBUG
			if ( debug & CYCLEDEBUG ) {
			    printf("[cyclelink] %s calls sibling %s %d times\n",
				parentp -> name , childp -> name ,
				arcp -> arc_count );
			}
#		    endif DEBUG
		}
	    }
	}
	    /*
	     *	collect calls and time around the cycle,
	     *	and save it in the cycle header.
	     */
	ncall = -callsamong;
	time = 0.0;
	for ( parentp = cyclenlp->cnext; parentp; parentp = parentp->cnext ) {
	    ncall += parentp -> ncall;
	    time += parentp -> time;
	}
#	ifdef DEBUG
	    if ( debug & CYCLEDEBUG ) {
		printf( "[cyclelink] cycle %d %f ticks in %d (%d) calls\n" ,
			cycle , time , ncall , callsamong );
	    }
#	endif DEBUG
	cyclenlp -> ncall = ncall;
	cyclenlp -> selfcalls = callsamong;
	cyclenlp -> time = time;
	cyclenlp -> childtime = 0.0;
    }
}

    /*
     *	in one top to bottom pass over the topologically sorted namelist
     *	set the print flags and sum up the time that will be shown in the
     *	graph profile.
     */
doflags()
{
    int		index;
    nltype	*childp;
    nltype	*oldhead;

    oldhead = 0;
    for ( index = nname-1 ; index >= 0 ; index -= 1 ) {
	childp = topsortnlp[ index ];
	    /*
	     *	if we haven't done this function or cycle,
	     *	calculate its printflag.
	     *	this way, we are linear in the number of arcs
	     *	since we do all members of a cycle (and the cycle itself)
	     *	as we hit the first member of the cycle.
	     */
	if ( childp -> cyclehead != oldhead ) {
	    oldhead = childp -> cyclehead;
	    parentprint( childp );
	}
	if ( ! childp -> printflag ) {
		/*
		 *	-f function says print the sucker
		 *	-e function says don't print it (leave it non-printing)
		 *	no -f's at all says print everything
		 */
	    if ( fflag && onflist( childp -> name ) ) {
		childp -> printflag = TRUE;
		printtime += childp -> time + childp -> childtime;
		continue;
	    }
	    if ( eflag && onelist( childp -> name ) ) {
		continue;
	    }
	    if ( !fflag ) {
		childp -> printflag = TRUE;
		printtime += childp -> time + childp -> childtime;
		continue;
	    }
	    continue;
	}
	    /*
	     *	this function has printing parents:
	     *	maybe someone wants to shut it up.
	     */
	if ( eflag && onelist( childp -> name ) ) {
	    childp -> printflag = FALSE;
	    continue;
	}
    }
}

    /*
     *	check if any parent of this child
     *	(or outside parents of this cycle)
     *	have their print flags on and set the 
     *	print flag of the child (cycle) appropriately.
     */
parentprint( childp )
    nltype	*childp;
{
    nltype	*headp;
    nltype	*memp;
    arctype	*arcp;

    headp = childp -> cyclehead;
    if ( childp == headp ) {
	    /*
	     *	just a regular child, check its parents
	     */
	for ( arcp = childp->parents ; arcp ; arcp = arcp->arc_parentlist ) {
	    if ( arcp -> arc_parentp -> printflag ) {
		childp -> printflag = TRUE;
		break;
	    }
	}
	return;
    }
	/*
	 *	its a member of a cycle, look at all parents from 
	 *	outside the cycle
	 */
    for ( memp = headp -> cnext ; memp ; memp = memp -> cnext ) {
	for ( arcp = memp -> parents ; arcp ; arcp = arcp -> arc_parentlist ) {
	    if ( arcp -> arc_parentp -> cyclehead == headp ) {
		continue;
	    }
	    if ( arcp -> arc_parentp -> printflag ) {
		goto set;
	    }
	}
    }
    return;
	/*
	 *	the cycle has a printing parent:  set the cycle
	 */
set:
    for ( memp = headp ; memp ; memp = memp -> cnext ) {
	memp -> printflag = TRUE;
    }
}
