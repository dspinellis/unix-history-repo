/* $Header: only.c,v 4.3 85/05/01 11:45:21 lwall Exp $
 *
 * $Log:	only.c,v $
 * Revision 4.3  85/05/01  11:45:21  lwall
 * Baseline for release with 4.3bsd.
 * 
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
    char *s;

#ifdef ONLY
    if (!*pat)
	return;
    if (maxngtodo < NGMAX) {
	ngtodo[maxngtodo] = savestr(pat);
#ifdef SPEEDOVERMEM
#ifndef lint
	compextodo[maxngtodo] = (COMPEX*)safemalloc(sizeof(COMPEX));
#endif lint
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
#ifdef SPEEDOVERMEM

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
#ifdef SPEEDOVERMEM
	    free_compex(compextodo[whicharg]);
#ifndef lint
	    free((char*)compextodo[whicharg]);
#endif lint
#endif
	}
	maxngtodo = 0;
    }
}
#endif
