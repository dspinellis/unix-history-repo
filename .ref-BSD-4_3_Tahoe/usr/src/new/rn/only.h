/* $Header: only.h,v 4.3 85/05/01 11:45:27 lwall Exp $
 *
 * $Log:	only.h,v $
 * Revision 4.3  85/05/01  11:45:27  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifndef NBRA
#include "search.h"
#endif

#ifdef ONLY
    EXT char *ngtodo[NGMAX];		/* restrictions in effect */
#   ifdef SPEEDOVERMEM
	EXT COMPEX *compextodo[NGMAX];	/* restrictions in compiled form */
#   endif
#endif

EXT int maxngtodo INIT(0);			/*  0 => no restrictions */
					/* >0 => # of entries in ngtodo */

void	only_init();
bool	inlist();	/* return TRUE if ngname is in command line list */
			/* or if there was no list */
void	setngtodo();
#ifdef ONLY
    void	end_only();
#endif
