#include "quipu/util.h"
#include "quipu/attrvalue.h"

extern LLog * log_dsap;

as_delnext (as)
Attr_Sequence  as;
{
Attr_Sequence ptr;

	if (as == NULLATTR)
		DLOG (log_dsap,LLOG_DEBUG,("delnext of null as!"));
	else    {
		ptr = as->attr_link;
		if (ptr == NULLATTR)
			DLOG (log_dsap,LLOG_DEBUG,("no new as to delete!"));
		else    {
			as->attr_link = ptr->attr_link;
			as_comp_free (ptr);
			}
		}
}

