#ifndef lint
    static	char *sccsid = "@(#)printlist.c	1.1 (Berkeley) %G%";
#endif lint

#include "gprof.h"

struct stringlist {
    struct stringlist	*next;
    char		*string;
};

struct stringlist	fflaglist = { 0 , 0 };

addflist( funcname )
    char	*funcname;
{
    struct stringlist	*slp;

    slp = (struct stringlist *) malloc( sizeof(struct stringlist));
    if ( slp == (struct stringlist *) 0 ) {
	fprintf( stderr, "gprof: ran out room for printlist\n" );
	done();
    }
    slp -> next = fflaglist.next;
    slp -> string = funcname;
    fflaglist.next = slp;
}

bool
onflist( funcname )
    char	*funcname;
{
    struct stringlist	*slp;

    for ( slp = fflaglist.next ; slp ; slp = slp -> next ) {
	if ( ! strcmp( slp -> string , funcname ) ) {
	    return TRUE;
	}
	if ( funcname[0] == '_' && ! strcmp( slp -> string , &funcname[1] ) ) {
	    return TRUE;
	}
    }
    return FALSE;
}

struct stringlist	eflaglist = { 0 , 0 };

addelist( funcname )
    char	*funcname;
{
    struct stringlist	*slp;

    slp = (struct stringlist *) malloc( sizeof(struct stringlist));
    if ( slp == (struct stringlist *) 0 ) {
	fprintf( stderr, "gprof: ran out room for printlist\n" );
	done();
    }
    slp -> next = eflaglist.next;
    slp -> string = funcname;
    eflaglist.next = slp;
}

bool
onelist( funcname )
    char	*funcname;
{
    struct stringlist	*slp;

    for ( slp = eflaglist.next ; slp ; slp = slp -> next ) {
	if ( ! strcmp( slp -> string , funcname ) ) {
	    return TRUE;
	}
	if ( funcname[0] == '_' && ! strcmp( slp -> string , &funcname[1] ) ) {
	    return TRUE;
	}
    }
    return FALSE;
}
