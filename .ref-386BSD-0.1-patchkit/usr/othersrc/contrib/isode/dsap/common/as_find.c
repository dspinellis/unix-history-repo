#include "quipu/util.h"
#include "quipu/attrvalue.h"

Attr_Sequence as_find_type (a,b)
Attr_Sequence a;
AttributeType b;
{
register int i;
register Attr_Sequence ptr;
	/* if Attr_cmp returns <0 no point in continuing due to ordering */

	for(ptr = a; ptr != NULLATTR; ptr=ptr->attr_link) {
		if (  (i = AttrT_cmp (ptr->attr_type,b)) <= 0)
			return (i ? NULLATTR : ptr);
	}
	return (NULLATTR);
}


