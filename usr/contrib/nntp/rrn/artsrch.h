/* $Header: artsrch.h,v 4.3 85/05/01 11:35:55 lwall Exp $
 *
 * $Log:	artsrch.h,v $
 * Revision 4.3  85/05/01  11:35:55  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifndef NBRA
#include "search.h"
#endif

#ifdef ARTSEARCH

#define SRCH_ABORT 0
#define SRCH_INTR 1
#define SRCH_FOUND 2
#define SRCH_NOTFOUND 3
#define SRCH_DONE 4
#define SRCH_SUBJDONE 5
#define SRCH_ERROR 6
#endif

EXT char *lastpat INIT(nullstr);	/* last search pattern */
#ifdef ARTSEARCH
    EXT COMPEX sub_compex;		/* last compiled subject search */
    EXT COMPEX art_compex;		/* last compiled normal search */
#   ifdef CONDSUB
	EXT COMPEX *bra_compex INIT(&art_compex);
					/* current compex with brackets */
#   endif
    EXT char art_howmuch;		/* search just the subjects */
    EXT bool art_doread;		/* search read articles? */
#endif

void	artsrch_init();
#ifdef ARTSEARCH
    int		art_search();
    bool	wanted();	/* return TRUE if current article matches pattern */
#endif
