#include "quipu/util.h"
#include "quipu/attrvalue.h"

avs_comp_fill (ptr,pe)
AV_Sequence ptr;
AttributeValue pe;
{
	ptr->avseq_av.av_struct = pe->av_struct;
	ptr->avseq_av.av_syntax = pe->av_syntax;
}

AV_Sequence  avs_comp_new (pe)
AttributeValue pe;
{
AV_Sequence ptr;
	ptr = avs_comp_alloc ();
	avs_comp_fill (ptr,pe);
	free ((char *)pe);
	ptr->avseq_next = NULLAV;
	return (ptr);
}
