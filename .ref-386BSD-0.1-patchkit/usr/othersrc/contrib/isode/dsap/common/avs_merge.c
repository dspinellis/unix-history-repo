#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/dsp.h"        /* for ds_error.h */
#include "quipu/ds_error.h"

extern LLog * log_dsap;

AV_Sequence avs_merge (a,b)
AV_Sequence a,b;
{
register AV_Sequence aptr, bptr, result, trail, tmp;

	if ( a == NULLAV )
		return (b);
	if ( b == NULLAV )
		return (a);

	/* start sequence off, make sure 'a' is the first */
	switch (avs_cmp_comp (a,b)) {
		default:
			LLOG (log_dsap,LLOG_EXCEPTIONS,("matching failed in avs_merge")) ;
			/* continue as if equal */

		case 0: /* equal */
			result = a;
			aptr = a->avseq_next;
			bptr = b->avseq_next;
			avs_comp_free (b);
			break;
		case -1:
			result = b;
			aptr = a;
			bptr = b->avseq_next;
			break;
		case 2: /* no compare function defined - treat as if a > b */
		case 1:
			result = a;
			aptr = a->avseq_next;
			bptr = b;
			break;
		}

	trail = result;
	while (  (aptr != NULLAV) && (bptr != NULLAV) ) {

	   switch (avs_cmp_comp (aptr,bptr)) {
		default:
			LLOG (log_dsap,LLOG_EXCEPTIONS,("matching failed in avs_merge (2)")) ;
			/* continue as if equal */

		case 0: /* equal */
			trail->avseq_next = aptr;
			trail = aptr;
			tmp = bptr->avseq_next;
			avs_comp_free (bptr);
			aptr = aptr->avseq_next;
			bptr = tmp;
			break;
		case -1:
			trail->avseq_next = bptr;
			trail = bptr;
			bptr = bptr->avseq_next;
			break;
		case 2: /* no compare function defined - treat as if a > b */
		case 1:
			trail->avseq_next = aptr;
			trail = aptr;
			aptr = aptr->avseq_next;
			break;
	    }
	}
	if (aptr == NULLAV)
		trail->avseq_next = bptr;
	else
		trail->avseq_next = aptr;

	return (result);
}


