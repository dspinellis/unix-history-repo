#include "mh.h"
#include <stdio.h>

char *m_find(str)
{
	register struct node *n;

	m_getdefs();
	for(n = m_defs; n; n = n->n_next)
		if(uleq(n->n_name, str))
			return(n->n_field);
	return(NULL);
}
