#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/malloc.h"

extern LLog * log_dsap;

RDN  rdn_comp_cpy (rdn)
RDN  rdn;
{
register RDN ptr;
unsigned last_heap;

	if (rdn==NULLRDN) {
		return (NULLRDN);
	}

	ptr = rdn_comp_alloc();
	ptr->rdn_at = AttrT_cpy (rdn->rdn_at);

	if ((last_heap = mem_heap) == 1)
		mem_heap = 2 + attr_index;

	AttrV_cpy_aux  (&rdn->rdn_av,&ptr->rdn_av);
	ptr->rdn_next = NULLRDN;

	mem_heap = last_heap;

	return (ptr);
}

RDN  rdn_cpy (rdn)
RDN  rdn;
{
RDN start;
register RDN eptr;
register RDN ptr,ptr2;

	if (rdn == NULLRDN) {
		return (NULLRDN);
	}
	start = rdn_comp_cpy (rdn);
	ptr2 = start;

	for (eptr=rdn->rdn_next; eptr!=NULLRDN; eptr=eptr->rdn_next) {
		ptr = rdn_comp_cpy (eptr);
		ptr2->rdn_next = ptr;
		ptr2 = ptr;
	}
	return (start);
}

