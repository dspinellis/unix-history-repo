#include "quipu/util.h"
#include "quipu/name.h"

extern LLog * log_dsap;

DN  dn_comp_cpy (dn)
register DN  dn;
{
register DN ptr;
	if (dn==NULLDN) {
		return (NULLDN);
	}
	ptr = (DN) smalloc (sizeof(dncomp));
	ptr->dn_rdn    = rdn_cpy (dn->dn_rdn);
	ptr->dn_parent = NULLDN;
	return (ptr);
}

DN  dn_cpy (dn)
register DN  dn;
{
DN start;
register DN eptr,ptr,ptr2;

	if (dn == NULLDN) {
		return (NULLDN);
	}
	start = dn_comp_cpy (dn);
	ptr2 = start;
	for (eptr = dn->dn_parent; eptr != NULLDN; eptr = eptr->dn_parent) {
		ptr = dn_comp_cpy (eptr);
		ptr2->dn_parent = ptr;
		ptr2 = ptr;
	}
	return (start);
}

