/* m_replace.c - replace an entry in the profile */

#include "../h/mh.h"
#include <stdio.h>


void m_replace (key, value)
register char  *key,
               *value;
{
    register struct node   *np;

    m_getdefs ();
    if (m_defs == NULL) {
	np = m_defs = (struct node *) malloc (sizeof *np);
	if (np == NULL)
	    adios (NULLCP, "unable to allocate profile storage");

	np -> n_name = getcpy (key);
	np -> n_field = getcpy (value);
	np -> n_context = 1;
	np -> n_next = NULL;
	ctxflags |= CTXMOD;
	return;
    }

    for (np = m_defs;; np = np -> n_next) {
	if (uleq (np -> n_name, key)) {
	    if (strcmp (value, np -> n_field) != 0) {
		if (!np -> n_context)
		    admonish (NULLCP, "bug: m_replace(key=\"%s\",value=\"%s\")",
			    key, value);
		if (np -> n_field)
		    free (np -> n_field);
		np -> n_field = getcpy (value);
		ctxflags |= CTXMOD;
	    }
	    return;
	}
	if (!np -> n_next)
	    break;
    }
    np -> n_next = (struct node *) malloc (sizeof *np);
    if (np -> n_next == NULL)
	adios (NULLCP, "unable to allocate profile storage");

    np = np -> n_next;
    np -> n_name = getcpy (key);
    np -> n_field = getcpy (value);
    np -> n_context = 1;
    np -> n_next = NULL;
    ctxflags |= CTXMOD;
}
