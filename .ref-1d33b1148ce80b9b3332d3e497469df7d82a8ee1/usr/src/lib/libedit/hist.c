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
static char sccsid[] = "@(#)hist.c	8.1 (Berkeley) %G%";
#endif /* not lint && not SCCSID */

/*
 * hist.c: History access functions
 */
#include "sys.h"
#include <stdlib.h>
#include "el.h"

/* hist_init():
 *	Initialization function.
 */
protected int
hist_init(el)
    EditLine *el;
{
    el->el_history.fun  = NULL;
    el->el_history.ref  = NULL;
    el->el_history.buf   = (char *) el_malloc(EL_BUFSIZ);
    el->el_history.last  = el->el_history.buf;
    return 0;
}


/* hist_end():
 *	clean up history;
 */
protected void
hist_end(el)
    EditLine *el;
{
    el_free((ptr_t) el->el_history.buf);
    el->el_history.buf   = NULL;
}


/* hist_set():
 *	Set new history interface
 */
protected int
hist_set(el, fun, ptr)
    EditLine *el;
    hist_fun_t fun;
    ptr_t ptr;

{
    el->el_history.ref = ptr;
    el->el_history.fun = fun;
    return 0;
}


/* hist_get():
 *	Get a history line and update it in the buffer.
 *	eventno tells us the event to get.
 */
protected el_action_t
hist_get(el)
    EditLine *el;
{
    const char    *hp;
    int     h;

    if (el->el_history.eventno == 0) {	/* if really the current line */
	(void) strncpy(el->el_line.buffer, el->el_history.buf, EL_BUFSIZ);
	el->el_line.lastchar = el->el_line.buffer + 
		(el->el_history.last - el->el_history.buf);

#ifdef KSHVI
    if (el->el_map.type == MAP_VI)
	el->el_line.cursor = el->el_line.buffer;
    else
#endif /* KSHVI */
	el->el_line.cursor = el->el_line.lastchar;

	return CC_REFRESH;
    }

    if (el->el_history.ref == NULL)
	return CC_ERROR;

    hp = HIST_FIRST(el);

    if (hp == NULL)
	return CC_ERROR;

    for (h = 1; h < el->el_history.eventno; h++)
	if ((hp = HIST_NEXT(el)) == NULL) {
	    el->el_history.eventno = h;
	    return CC_ERROR;
	}

    (void) strncpy(el->el_line.buffer, hp, EL_BUFSIZ);
    el->el_line.lastchar = el->el_line.buffer + strlen(el->el_line.buffer);

    if (el->el_line.lastchar > el->el_line.buffer) {
	if (el->el_line.lastchar[-1] == '\n')
	    el->el_line.lastchar--;
	if (el->el_line.lastchar[-1] == ' ')
	    el->el_line.lastchar--;
	if (el->el_line.lastchar < el->el_line.buffer)
	    el->el_line.lastchar = el->el_line.buffer;
    }

#ifdef KSHVI
    if (el->el_map.type == MAP_VI)
	el->el_line.cursor = el->el_line.buffer;
    else
#endif /* KSHVI */
	el->el_line.cursor = el->el_line.lastchar;

    return CC_REFRESH;
}

/* hist_list()
 *	List history entries
 */
protected int
/*ARGSUSED*/
hist_list(el, argc, argv)
    EditLine *el;
    int argc;
    char **argv;
{
    const char *str;

    if (el->el_history.ref == NULL)
	return -1;
    for (str = HIST_LAST(el); str != NULL; str = HIST_PREV(el))
	(void) fprintf(el->el_outfile, "%d %s", el->el_history.ev->num, str);
    return 0;
}
