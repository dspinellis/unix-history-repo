#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/malloc.h"

extern short acl_sntx;
extern IFP merge_acl;
extern IFP acl_fn;
extern char dsa_mode;

Attr_Sequence str2as (str)
register char * str;
{
register char * ptr;
char * save, val;
AttributeType at;
Attr_Sequence as;

	if (str == NULLCP)
		return (NULLATTR);

	if ((ptr = index (str,'=')) == 0) {
		parse_error ("equals missing in '%s'",str);
		return (NULLATTR);
	}

	save = ptr++;
	if (! isspace (*--save))
		save++;

	ptr = SkipSpace (ptr);
	if (*ptr == 0)
		return (NULLATTR);

	val = *save;
	*save = 0;
	
	if ((at = AttrT_new (str)) == NULLAttrT) {
		parse_error ("unknown attribute type '%s'",str);
		*ptr = '=';
		return (NULLATTR);
	}

	*save = val;

	as = as_comp_alloc ();
	as->attr_acl  = NULLACL_INFO;
	as->attr_type = at;
	as->attr_link = NULLATTR;

	ATTRIBUTE_HEAP;

	if ((as->attr_value = str2avs (ptr,as->attr_type)) == NULLAV) {
		RESTORE_HEAP;
		as_free (as);
		return (NULLATTR);
	}

	RESTORE_HEAP;

	return (as);

}

Attr_Sequence as_combine (as,str,allownull)
Attr_Sequence as;
register char * str;
char allownull;
{
register char * ptr = str;
char * save, val;
AV_Sequence avs;
Attr_Sequence as2,nas;
AttributeType at;
int i;

	if (str == NULLCP)
		return (as);

	while (*ptr != 0)
		if (*ptr == '=')
			break;
		else
			ptr++;

	if (*ptr == 0) {
		if (allownull) {
			nas = as_comp_alloc ();
			nas->attr_acl  = NULLACL_INFO;
			if ((nas->attr_type = AttrT_new (str)) == NULLAttrT) {
				parse_error ("unknown attribute type '%s'",str);
				return as;
			}
			nas->attr_link = NULLATTR;
			nas->attr_value = NULLAV;
			as = as_merge (as,nas);
			return as;
		}
		parse_error ("equals missing in '%s'",str);
		return (as);
	}

	save = ptr++;
	if (! isspace (*--save))
		save++;

	val = *save;
	*save = 0;

	ptr = SkipSpace (ptr);

	if ((at = AttrT_new (str)) == NULLAttrT) {
		parse_error ("unknown attribute type '%s'",str);
		*ptr = '=';
		return (as);
	}

	if (*ptr == 0) {
		if ((at->oa_syntax == acl_sntx) && dsa_mode) {
			/* Add default ACL */	
			struct acl * acl = (struct acl *) NULL;

			acl = acl_alloc();
			acl->ac_child = (struct acl_info *)(*acl_fn)();
			acl->ac_entry = (struct acl_info *)(*acl_fn)();
			acl->ac_default = (struct acl_info *)(*acl_fn)();
			acl->ac_attributes = NULLACL_ATTR;

			nas = as_comp_alloc ();
			nas->attr_acl  = NULLACL_INFO;
			nas->attr_link = NULLATTR;
			nas->attr_value = avs_comp_alloc();
			nas->attr_value->avseq_next = NULLAV;
			nas->attr_value->avseq_av.av_syntax = acl_sntx;
			nas->attr_value->avseq_av.av_struct = (caddr_t) acl;
			nas->attr_type = at;
			as = as_merge (as,nas);
		} else if (allownull) {
			nas = as_comp_alloc ();
			nas->attr_acl  = NULLACL_INFO;
			nas->attr_link = NULLATTR;
			nas->attr_value = NULLAV;
			nas->attr_type = at;
			as = as_merge (as,nas);
		}
		return (as);
	}

	for (as2=as; as2 != NULLATTR; as2=as2->attr_link) {
		if ((i = AttrT_cmp (at, as2->attr_type)) == 0) {
			*save = val;

			ATTRIBUTE_HEAP;

			if (at->oa_syntax == acl_sntx) {
				(*merge_acl)(as2->attr_value,SkipSpace(ptr));
				RESTORE_HEAP;
				return (as);
			}

			if ((avs = str2avs (ptr,as2->attr_type)) == NULLAV) {
				RESTORE_HEAP;
				return (as);
			}

			as2->attr_value = avs_merge (as2->attr_value,avs);

			RESTORE_HEAP;

			return (as);	
		} 
		else 
			if ( i > 0 )
				break;
	}

	*save = val;

	nas = as_comp_alloc ();
	nas->attr_acl  = NULLACL_INFO;
	nas->attr_type = at;
	nas->attr_link = NULLATTR;

	ATTRIBUTE_HEAP;

	if ((nas->attr_value = str2avs (ptr,nas->attr_type)) == NULLAV) {
		RESTORE_HEAP;
		as_free (nas);
		return (as);
	}

	RESTORE_HEAP;

	return (as_merge(as,nas));

}
