/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)search.h	8.1 (Berkeley) %G%
 */

/*
 * el.search.h: Line and history searching utilities
 */
#ifndef _h_el_search 
#define _h_el_search 

#include "histedit.h"

typedef struct el_search_t {
    char *patbuf;		/* The pattern buffer		*/
    int  patlen;		/* Length of the pattern buffer	*/
    int  patdir;		/* Direction of the last search	*/
    int  chadir;		/* Character search direction	*/
    char chacha;		/* Character we are looking for	*/
} el_search_t;


protected int 		el_match	__P((const char *, const char *));
protected int		search_init	__P((EditLine *));
protected void		search_end	__P((EditLine *));
protected int		c_hmatch	__P((EditLine *, const char *));
protected void		c_setpat	__P((EditLine *));
protected el_action_t	ce_inc_search	__P((EditLine *, int));
protected el_action_t	cv_search	__P((EditLine *, int));
protected el_action_t	ce_search_line	__P((EditLine *, char *, int));
protected el_action_t	cv_repeat_srch	__P((EditLine *, int));
protected el_action_t	cv_csearch_back	__P((EditLine *, int, int, int));
protected el_action_t	cv_csearch_fwd	__P((EditLine *, int, int, int));

#endif /* _h_el_search */
