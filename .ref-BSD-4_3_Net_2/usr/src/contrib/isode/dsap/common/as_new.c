#include "quipu/util.h"
#include "quipu/attrvalue.h"

Attr_Sequence  as_comp_new (at,as,acl)
AttributeType  at;
AV_Sequence    as;
struct acl_info * acl;
{
Attr_Sequence ptr;
	ptr = as_comp_alloc ();
	bzero ((char *)ptr,sizeof(*ptr));
	ptr->attr_value = as;
	ptr->attr_acl   = acl;
	if (at) 
		ptr->attr_type = at;
	ptr->attr_link = NULLATTR;
	return (ptr);
}

