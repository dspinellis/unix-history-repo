/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prompt.h	8.1 (Berkeley) %G%
 */

/*
 * el.prompt.h: Prompt printing stuff
 */
#ifndef _h_el_prompt
#define _h_el_prompt

#include "histedit.h"

typedef char * (*el_pfunc_t) __P((EditLine*));

typedef struct el_prompt_t {
    el_pfunc_t p_func;		/* Function to return the prompt	*/
    coord_t    p_pos;		/* position in the line after prompt	*/
} el_prompt_t;

protected void prompt_print	__P((EditLine *));
protected int  prompt_set	__P((EditLine *, el_pfunc_t));
protected int  prompt_init	__P((EditLine *));
protected void prompt_end	__P((EditLine *));

#endif /* _h_el_prompt */
