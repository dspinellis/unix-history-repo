#include "quipu/util.h"
#include "quipu/attrvalue.h"

avs_comp_free (avs)
AV_Sequence avs;
{
	AttrV_free_aux (&avs->avseq_av);
	free ((char *) avs);
}

avs_free (avs)
AV_Sequence avs;
{
register AV_Sequence eptr;
register AV_Sequence next;

	for(eptr = avs; eptr != NULLAV; eptr = next) {
		next = eptr->avseq_next;
		avs_comp_free (eptr);
	}
}
