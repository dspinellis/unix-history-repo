#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/dsp.h"        /* for ds_error.h */
#include "quipu/ds_error.h"

avs_cmp (a,b)
AV_Sequence  a,b;
{
int i;

	for (; (a != NULLAV) && (b != NULLAV) ; a = a->avseq_next, b = b->avseq_next)
		if ( (i = avs_cmp_comp (a,b))  != 0) 
			return (i);

	if ( (a == NULLAV) && (b == NULLAV) )   {
		return 0;
	} else {
		return (a ?  1 : -1);
		}
}
