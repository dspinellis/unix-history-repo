/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * %sccs.include.redist.c%
 */

#if !defined(lint) && !defined(SCCSID)
static char sccsid[] = "@(#)prompt.c	8.1 (Berkeley) %G%";
#endif /* not lint && not SCCSID */

/*
 * prompt.c: Prompt printing functions
 */
#include "sys.h"
#include <stdio.h>
#include "el.h"

private char *prompt_default	__P((EditLine *));

/* prompt_default():
 *	Just a default prompt, in case the user did not provide one
 */
private char *
/*ARGSUSED*/
prompt_default(el)
    EditLine *el;
{
    static char a[3] = { '?', ' ', '\0' };
    return a;
}


/* prompt_print():
 *	Print the prompt and update the prompt position.
 *	We use an array of integers in case we want to pass
 * 	literal escape sequences in the prompt and we want a
 *	bit to flag them
 */
protected void
prompt_print(el)
    EditLine *el;
{
    char *p = (*el->el_prompt.p_func)(el);
    while (*p)
	re_putc(el, *p++);

    el->el_prompt.p_pos.v = el->el_refresh.r_cursor.v;
    el->el_prompt.p_pos.h = el->el_refresh.r_cursor.h;

} /* end prompt_print */


/* prompt_init():
 *	Initialize the prompt stuff
 */
protected int 
prompt_init(el)
    EditLine *el;
{
    el->el_prompt.p_func = prompt_default;
    el->el_prompt.p_pos.v = 0;
    el->el_prompt.p_pos.h = 0;
    return 0;
} /* end prompt_init */


/* prompt_end():
 *	Clean up the prompt stuff
 */
protected void
/*ARGSUSED*/ 
prompt_end(el)
    EditLine *el;
{
} /* end prompt_end */


/* prompt_set():
 *	Install a prompt printing function
 */
protected int 
prompt_set(el, prf)
    EditLine *el;
    el_pfunc_t prf;
{
    if (prf == NULL)
	el->el_prompt.p_func = prompt_default;
    else
	el->el_prompt.p_func = prf;
    el->el_prompt.p_pos.v = 0;
    el->el_prompt.p_pos.h = 0;
    return 0;
} /* end prompt_set */
