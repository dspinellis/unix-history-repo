#include "quipu/util.h"
#include "quipu/name.h"

RDN  rdn_comp_new (at,av)
AttributeType  at;
AttributeValue av;
{
register RDN ptr;
	ptr = rdn_comp_alloc ();
	bzero ((char *)ptr, sizeof (*ptr));
	ptr->rdn_at = at;
	if (av)
		AttrV_cpy_aux (av,&ptr->rdn_av);
	ptr->rdn_next = NULLRDN;
	return (ptr);
}

