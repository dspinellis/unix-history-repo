#include "mh.h"

m_delete(key)
char *key;
{
	register struct node *np, *npprev;

	m_getdefs();
	for(np = (struct node *) &m_defs; npprev = np; )  {
		if((np = np->n_next) == 0)
			break;
		if(uleq(np->n_name, key)) {
			npprev->n_next = np->n_next;
			cndfree(np->n_name);
			cndfree(np->n_field);
			free(np);
			def_flags |= DEFMOD;
			return(0);
		}
	}
	return(1);
}
