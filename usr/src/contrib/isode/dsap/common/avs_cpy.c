#include "quipu/util.h"
#include "quipu/attrvalue.h"

extern LLog * log_dsap;

AV_Sequence  avs_comp_cpy (avs)
AV_Sequence  avs;
{
AV_Sequence ptr;

	if (avs==NULLAV) {
		return (NULLAV);
	}
	ptr = (AV_Sequence) smalloc (sizeof(avseqcomp));
	AttrV_cpy_aux (&avs->avseq_av,&ptr->avseq_av);
	ptr->avseq_next = NULLAV;
	return (ptr);
}

static AV_Sequence  avs_comp_cpy_enc (avs)
AV_Sequence  avs;
{
AV_Sequence ptr;

	if (avs==NULLAV) {
		return (NULLAV);
	}
	ptr = (AV_Sequence) smalloc (sizeof(avseqcomp));
	AttrV_cpy_enc (&avs->avseq_av,&ptr->avseq_av);
	ptr->avseq_next = NULLAV;
	return (ptr);
}

AV_Sequence  avs_cpy (avs)
AV_Sequence  avs;
{
AV_Sequence start;
AV_Sequence ptr,ptr2;
register AV_Sequence eptr;

	if (avs == NULLAV ) {
		return (NULLAV);
	}
	start = avs_comp_cpy (avs);
	ptr2 = start;
	
	for(eptr = avs->avseq_next; eptr != NULLAV; eptr = eptr->avseq_next) {
		ptr = avs_comp_cpy (eptr);
		ptr2->avseq_next = ptr;
		ptr2 = ptr;
	}
	return (start);
}

AV_Sequence  avs_cpy_enc (avs)
AV_Sequence  avs;
{
AV_Sequence start;
AV_Sequence ptr,ptr2;
register AV_Sequence eptr;

	/* an AVS, and encode at the same time */

	if (avs == NULLAV ) {
		return (NULLAV);
	}
	start = avs_comp_cpy_enc (avs);
	ptr2 = start;
	
	for(eptr = avs->avseq_next; eptr != NULLAV; eptr = eptr->avseq_next) {
		ptr = avs_comp_cpy_enc (eptr);
		ptr2->avseq_next = ptr;
		ptr2 = ptr;
	}
	return (start);
}

