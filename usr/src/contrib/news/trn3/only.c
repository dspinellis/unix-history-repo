/* $Id: only.c,v 3.0 1991/09/09 20:23:31 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "search.h"
#include "util.h"
#include "final.h"
#include "ngsrch.h"
#include "INTERN.h"
#include "only.h"

void
only_init()
{
    ;
}

void
setngtodo(pat)
char *pat;
{
#ifdef ONLY
#ifdef SPEED_OVER_MEM
    char *s;
#endif

    if (!*pat)
	return;
    if (maxngtodo < NGMAX) {
	ngtodo[maxngtodo] = savestr(pat);
#ifdef SPEED_OVER_MEM
#ifndef lint
	compextodo[maxngtodo] = (COMPEX*)safemalloc(sizeof(COMPEX));
#endif /* lint */
	init_compex(compextodo[maxngtodo]);
	compile(compextodo[maxngtodo],pat,TRUE,TRUE);
	if ((s = ng_comp(compextodo[maxngtodo],pat,TRUE,TRUE)) != Nullch) {
					    /* compile regular expression */
	    printf("\n%s\n",s) FLUSH;
	    finalize(1);
	}
#endif
	maxngtodo++;
    }
#else
    notincl("o");
#endif
}

/* if command line list is non-null, is this newsgroup wanted? */

bool
inlist(ngnam)
char *ngnam;
{
#ifdef ONLY
    register int i;
#ifdef SPEED_OVER_MEM

    if (maxngtodo == 0)
	return TRUE;
    for (i=0; i<maxngtodo; i++) {
	if (execute(compextodo[i],ngnam))
	    return TRUE;
    }
    return FALSE;
#else
    COMPEX ilcompex;
    char *s;

    if (maxngtodo == 0)
	return TRUE;
    init_compex(&ilcompex);
    for (i=0; i<maxngtodo; i++) {
	if ((s = ng_comp(&ilcompex,ngtodo[i],TRUE,TRUE)) != Nullch) {
					    /* compile regular expression */
	    printf("\n%s\n",s) FLUSH;
	    finalize(1);
	}
	
	if (execute(&ilcompex,ngnam) != Nullch) {
	    free_compex(&ilcompex);
	    return TRUE;
	}
    }
    free_compex(&ilcompex);
    return FALSE;
#endif
#else
    return TRUE;
#endif
}

#ifdef ONLY
void
end_only()
{
    if (maxngtodo) {			/* did they specify newsgroup(s) */
	int whicharg;

#ifdef VERBOSE
	IF(verbose)
	    printf("\nRestriction %s%s removed.\n",ngtodo[0],
		maxngtodo > 1 ? ", etc." : nullstr) FLUSH;
	ELSE
#endif
#ifdef TERSE
	    fputs("\nExiting \"only\".\n",stdout) FLUSH;
#endif
	for (whicharg = 0; whicharg < maxngtodo; whicharg++) {
	    free(ngtodo[whicharg]);
#ifdef SPEED_OVER_MEM
	    free_compex(compextodo[whicharg]);
#ifndef lint
	    free((char*)compextodo[whicharg]);
#endif /* lint */
#endif
	}
	maxngtodo = 0;
    }
}
#endif
