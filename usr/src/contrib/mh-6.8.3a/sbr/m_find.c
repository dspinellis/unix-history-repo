/* m_find.c - find an entry in the profile */

#include "../h/mh.h"
#include <stdio.h>


char   *m_find (str)
register char  *str;
{
    register struct node   *np;

    m_getdefs ();
    for (np = m_defs; np; np = np -> n_next)
	if (uleq (np -> n_name, str))
	    return (np -> n_field);

    return NULL;
}
