#include "quipu/util.h"
#include "quipu/attrvalue.h"

as_comp_free (as)
Attr_Sequence as;
{
	avs_free (as->attr_value);
	free ((char *) as);
}

as_free (as)
Attr_Sequence as;
{
register Attr_Sequence eptr;
register Attr_Sequence next;

	for(eptr = as; eptr != NULLATTR; eptr = next) {
		next = eptr->attr_link;
		as_comp_free (eptr);
	}
}
