/* m_delete.c - delete an entry from the profile */

#include "../h/mh.h"
#include <stdio.h>


m_delete (key)
register char  *key;
{
    register struct node   *np,
                           *pp;

    m_getdefs ();
    for (np = m_defs, pp = NULL; np; pp = np, np = np -> n_next) {
	if (uleq (np -> n_name, key)) {
	    if (!np -> n_context)
		admonish (NULLCP, "bug: m_delete(key=\"%s\")", np -> n_name);
	    if (pp)
		pp -> n_next = np -> n_next;
	    else
		m_defs = np -> n_next;
	    free (np -> n_name);
	    if (np -> n_field)
		free (np -> n_field);
	    free ((char *) np);
	    ctxflags |= CTXMOD;
	    return 0;
	}
    }

    return 1;
}
