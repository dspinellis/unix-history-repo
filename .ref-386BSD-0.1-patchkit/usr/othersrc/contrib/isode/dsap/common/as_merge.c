#include "quipu/util.h"
#include "quipu/attrvalue.h"

Attr_Sequence as_merge (a,b)
Attr_Sequence a,b;
{
register Attr_Sequence aptr, bptr, result, trail, tmp;

	if ( a == NULLATTR )
		return (b);
	if ( b == NULLATTR )
		return (a);

	/* start sequence off, make sure 'a' is the first */
	switch (AttrT_cmp (a->attr_type,b->attr_type)) {
		case 0: /* equal */
			result = a;
			a->attr_value = avs_merge (a->attr_value, b->attr_value);
			aptr = a->attr_link;
			bptr = b->attr_link;
			free ((char *) b);
			break;
		case -1:
			result = b;
			aptr = a;
			bptr = b->attr_link;
			break;
		case 1:
			result = a;
			aptr = a->attr_link;
			bptr = b;
			break;
		}

	trail = result;
	while (  (aptr != NULLATTR) && (bptr != NULLATTR) ) {

	   switch (AttrT_cmp (aptr->attr_type,bptr->attr_type)) {
		case 0: /* equal */
			aptr->attr_value = avs_merge (aptr->attr_value, bptr->attr_value);
			tmp = bptr->attr_link;
			free ((char *) bptr);
			trail->attr_link = aptr;
			trail = aptr;
			aptr = aptr->attr_link;
			bptr = tmp;
			break;
		case -1:
			trail->attr_link = bptr;
			trail = bptr;
			bptr = bptr->attr_link;
			break;
		case 1:
			trail->attr_link = aptr;
			trail = aptr;
			aptr = aptr->attr_link;
			break;
	    }
	}
	if (aptr == NULLATTR)
		trail->attr_link = bptr;
	else
		trail->attr_link = aptr;

	return (result);
}

Attr_Sequence as_merge_aux (a,b)
Attr_Sequence a,b;
{
register Attr_Sequence aptr, bptr, result, trail, tmp;

	if ( a == NULLATTR )
		return (b);
	if ( b == NULLATTR )
		return (a);

	/* start sequence off, make sure 'a' is the first */
	switch (AttrT_cmp (a->attr_type,b->attr_type)) {
		case 0: /* equal */
			result = a;
			avs_free (a->attr_value);
			a->attr_value = b->attr_value;
			aptr = a->attr_link;
			bptr = b->attr_link;
			free ((char *) b);
			break;
		case -1:
			result = b;
			aptr = a;
			bptr = b->attr_link;
			break;
		case 1:
			result = a;
			aptr = a->attr_link;
			bptr = b;
			break;
		}

	trail = result;
	while (  (aptr != NULLATTR) && (bptr != NULLATTR) ) {

	   switch (AttrT_cmp (aptr->attr_type,bptr->attr_type)) {
		case 0: /* equal */
			avs_free (aptr->attr_value);
			aptr->attr_value = bptr->attr_value;
			tmp = bptr->attr_link;
			free ((char *) bptr);
			trail->attr_link = aptr;
			trail = aptr;
			aptr = aptr->attr_link;
			bptr = tmp;
			break;
		case -1:
			trail->attr_link = bptr;
			trail = bptr;
			bptr = bptr->attr_link;
			break;
		case 1:
			trail->attr_link = aptr;
			trail = aptr;
			aptr = aptr->attr_link;
			break;
	    }
	}
	if (aptr == NULLATTR)
		trail->attr_link = bptr;
	else
		trail->attr_link = aptr;

	return (result);
}

