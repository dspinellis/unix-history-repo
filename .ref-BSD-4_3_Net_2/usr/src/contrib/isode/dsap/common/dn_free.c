#include "quipu/util.h"
#include "quipu/name.h"

dn_comp_free (dn)
DN dn;
{
	rdn_free (dn->dn_rdn);
	free ((char *) dn);
}
dn_free (dn)
DN dn;
{
register DN eptr;
register DN next;

	for (eptr = dn; eptr != NULLDN; eptr=next) {
		next = eptr->dn_parent;
		dn_comp_free (eptr);
	}
}

