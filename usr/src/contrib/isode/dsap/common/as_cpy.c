#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/malloc.h"

extern LLog * log_dsap;

Attr_Sequence  as_comp_cpy (as)
Attr_Sequence  as;
{
Attr_Sequence ptr;

	if (as==NULLATTR) {
		return (NULLATTR);
	}

	ptr = (Attr_Sequence) smalloc (sizeof(attrcomp));
	ptr->attr_type = AttrT_cpy (as->attr_type);

	ATTRIBUTE_HEAP;

	ptr->attr_value = avs_cpy  (as->attr_value);
	ptr->attr_link = NULLATTR;
	ptr->attr_acl = NULLACL_INFO;

	RESTORE_HEAP;

	return (ptr);
}

Attr_Sequence  as_cpy (as)
Attr_Sequence  as;
{
Attr_Sequence start;
Attr_Sequence ptr,ptr2;
register Attr_Sequence eptr;

	if (as == NULLATTR) {
		return (NULLATTR);
	}
	start = as_comp_cpy (as);
	ptr2 = start;

	for(eptr = as->attr_link; eptr != NULLATTR; eptr=eptr->attr_link) {
		ptr = as_comp_cpy (eptr);
		ptr2->attr_link = ptr;
		ptr2 = ptr;
	}
	return (start);
}

