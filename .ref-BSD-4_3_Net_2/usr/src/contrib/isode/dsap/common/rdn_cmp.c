#include "quipu/util.h"
#include "quipu/name.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"

rdn_cmp_comp (a,b)
RDN  a,b;
{
register int j;

	if (a->rdn_at != b->rdn_at) 
		return ((a->rdn_at > b->rdn_at) ? 1 : -1);

	j = AttrV_cmp (&a->rdn_av,&b->rdn_av);

	return (j);

}

