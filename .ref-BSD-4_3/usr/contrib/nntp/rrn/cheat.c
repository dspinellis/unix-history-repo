/* $Header: cheat.c,v 4.3 85/05/01 11:36:46 lwall Exp $
 *
 * $Log:	cheat.c,v $
 * Revision 4.3  85/05/01  11:36:46  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "search.h"
#include "ng.h"
#include "bits.h"
#include "artio.h"
#include "term.h"
#include "artsrch.h"
#include "head.h"
#include "INTERN.h"
#include "cheat.h"

/* see what we can do while they are reading */

#ifdef PENDING
#   ifdef ARTSEARCH
	COMPEX srchcompex;		/* compiled regex for searchahead */
#   endif
#endif

void
cheat_init()
{
    ;
}

#ifdef PENDING
void
look_ahead()
{
#ifdef ARTSEARCH
    register char *h, *s;

#ifdef DEBUGGING
    if (debug && srchahead) {
	printf("(%ld)",(long)srchahead);
	fflush(stdout);
    }
#endif
    if (srchahead && srchahead < art) {	/* in ^N mode? */
	char *pattern;

	pattern = buf+1;
	strcpy(pattern,": *");
	h = pattern + strlen(pattern);
	interp(h,(sizeof buf) - (h-buf),"%s");
	h[24] = '\0';		/* compensate for notesfiles */
	while (*h) {
	    if (index("\\[.^*$'\"",*h) != Nullch)
		*h++ = '.';
	    else
		h++;
	}
#ifdef DEBUGGING
	if (debug & DEB_SEARCH_AHEAD) {
	    fputs("(hit CR)",stdout);
	    fflush(stdout);
	    gets(buf+128);
	    printf("\npattern = %s\n",pattern);
	}
#endif
	if ((s = compile(&srchcompex,pattern,TRUE,TRUE)) != Nullch) {
				    /* compile regular expression */
	    printf("\n%s\n",s);
	    srchahead = 0;
	}
	if (srchahead) {
	    srchahead = art;
	    for (;;) {
		srchahead++;	/* go forward one article */
		if (srchahead > lastart) { /* out of articles? */
#ifdef DEBUGGING
		    if (debug)
			fputs("(not found)",stdout);
#endif
		    break;
		}
		if (!was_read(srchahead) &&
		    wanted(&srchcompex,srchahead,0)) {
				    /* does the shoe fit? */
#ifdef DEBUGGING
		    if (debug)
			printf("(%ld)",(long)srchahead);
#endif
		    artopen(srchahead);
		    break;
		}
		if (input_pending())
		    break;
	    }
	    fflush(stdout);
	}
    }
    else
#endif
    {
	if (art+1 <= lastart)/* how about a pre-fetch? */
	    artopen(art+1);	/* look for the next article */
    }
}
#endif

/* see what else we can do while they are reading */

void
collect_subjects()
{
#ifdef PENDING
# ifdef CACHESUBJ
    ART_NUM oldart = openart;
    ART_POS oldartpos;

    if (!in_ng || !srchahead)
	return;
    if (oldart)			/* remember where we were in art */
	oldartpos = ftell(artfp);
    if (srchahead >= subj_to_get)
	subj_to_get = srchahead+1;
    while (!input_pending() && subj_to_get <= lastart)
	fetchsubj(subj_to_get++,FALSE,FALSE);
    if (oldart) {
	artopen(oldart);
	fseek(artfp,oldartpos,0);	/* do not screw the pager */
    }
# endif
#endif
}

