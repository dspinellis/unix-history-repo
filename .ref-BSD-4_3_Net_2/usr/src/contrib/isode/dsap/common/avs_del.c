#include "quipu/util.h"
#include "quipu/attrvalue.h"

extern LLog * log_dsap;

avs_delnext (avs)
AV_Sequence  avs;
{
AV_Sequence ptr;
	if (avs == NULLAV)
		DLOG (log_dsap,LLOG_DEBUG,("delnext of null avs!"));
	else    {
		ptr = avs->avseq_next;
		if (ptr == NULLAV)
			DLOG (log_dsap,LLOG_DEBUG,("no new avs to delete!"));
		else    {
			avs->avseq_next = ptr->avseq_next;
			avs_comp_free (ptr);
			}
		}
}

