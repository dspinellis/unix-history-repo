#include "quipu/util.h"
#include "quipu/name.h"

rdn_comp_free (rdn)
RDN rdn;
{
	AttrV_free_aux (&rdn->rdn_av);
	free ((char *) rdn);
}

rdn_free (rdn)
RDN rdn;
{
register RDN eptr;
register RDN next;

	for (eptr=rdn; eptr!=NULLRDN; eptr=next) {
		next = eptr->rdn_next;
		rdn_comp_free (eptr);
	}
}

