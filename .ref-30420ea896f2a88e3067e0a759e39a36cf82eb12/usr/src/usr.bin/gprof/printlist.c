#ifndef lint
    static	char *sccsid = "@(#)printlist.c	1.2 (Berkeley) %G%";
#endif lint

#include "gprof.h"

    /*
     *	these are the lists of names:
     *	there is the list head and then the listname
     *	is a pointer to the list head
     *	(for ease of passing to stringlist functions).
     */
struct stringlist	fhead = { 0 , 0 };
struct stringlist	*flist = &fhead;
struct stringlist	Fhead = { 0 , 0 };
struct stringlist	*Flist = &Fhead;
struct stringlist	ehead = { 0 , 0 };
struct stringlist	*elist = &ehead;
struct stringlist	Ehead = { 0 , 0 };
struct stringlist	*Elist = &Ehead;

addlist( listp , funcname )
    struct stringlist	*listp;
    char		*funcname;
{
    struct stringlist	*slp;

    slp = (struct stringlist *) malloc( sizeof(struct stringlist));
    if ( slp == (struct stringlist *) 0 ) {
	fprintf( stderr, "gprof: ran out room for printlist\n" );
	done();
    }
    slp -> next = listp -> next;
    slp -> string = funcname;
    listp -> next = slp;
}

bool
onlist( listp , funcname )
    struct stringlist	*listp;
    char		*funcname;
{
    struct stringlist	*slp;

    for ( slp = listp -> next ; slp ; slp = slp -> next ) {
	if ( ! strcmp( slp -> string , funcname ) ) {
	    return TRUE;
	}
	if ( funcname[0] == '_' && ! strcmp( slp -> string , &funcname[1] ) ) {
	    return TRUE;
	}
    }
    return FALSE;
}
