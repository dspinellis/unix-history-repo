#include "quipu/util.h"
#include "quipu/name.h"

DN  dn_comp_new (rdn)
RDN rdn;
{
DN ptr;
	ptr = dn_comp_alloc ();
	dn_comp_fill (ptr,rdn);
	ptr->dn_parent = NULLDN;
	return (ptr);
}

