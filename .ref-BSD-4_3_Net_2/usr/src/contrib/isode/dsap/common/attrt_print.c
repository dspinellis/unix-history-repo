#include "quipu/util.h"
#include "quipu/name.h"

extern int oidformat;
AttributeType last_at;
int avs_count = 1;

AttrT_print (ps,x,format)
register PS ps;
register AttributeType x;
register int format;
{
	if (x == NULLAttrT) {
		ps_print(ps,"Unknown Type");
		return;
	}

	last_at = x;
	avs_count = 1;

	if (format == READOUT)
		ps_printf (ps,"%s",attr2name (x,oidformat));
	else
		ps_printf (ps,"%s",attr2name_aux (x));
}
