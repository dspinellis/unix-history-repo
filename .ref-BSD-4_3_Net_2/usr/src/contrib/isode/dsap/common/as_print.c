#include "quipu/util.h"
#include "quipu/attrvalue.h"

extern AttributeType last_at;
extern int avs_count;
short inherit_sntx = -1;
extern char dsa_mode;

as_comp_print (ps,as,format)
PS   ps;
Attr_Sequence  as;
int  format;
{
AV_Sequence avs;
char buffer [LINESIZE];
extern int oidformat;
char * ptr;

		last_at = as->attr_type;
		avs_count = 1;

		if (format == READOUT) {
			(void) sprintf (buffer,"%s",attr2name (as->attr_type,oidformat));
			ptr = buffer;
		} else 
			ptr = attr2name_aux (as->attr_type);

		if (split_attr (as))
			if (as->attr_value == NULLAV)
				if (format != READOUT)
					ps_printf (ps, "%s=\n", ptr);
				else 
					ps_printf (ps, "%-21s - NO VALUE\n", ptr);
			else {
			   if (format != READOUT)
				ps_printf (ps, "%s= ", ptr);
			   for (avs = as->attr_value; avs != NULLAV; avs = avs->avseq_next,avs_count++) {
				if (format == READOUT)
					ps_printf (ps, "%-21s - ", ptr);
				avs_comp_print (ps, avs, format);
				if ((format != READOUT) && (avs->avseq_next))
					if (!dsa_mode || 
					    (as->attr_type->oa_syntax == inherit_sntx))
						ps_printf (ps,"\n%s= ",ptr);
					else
						ps_print (ps," &\\\n\t");
				else
					ps_print (ps, "\n");
			   }
			}
		else {
			if (format == READOUT)
				ps_printf (ps, "%-21s - ", ptr);
			else
				ps_printf (ps, "%s= ", ptr);
			avs_print (ps,as->attr_value,format);
		}

		avs_count = 1;
}

as_print (ps,as,format)
Attr_Sequence  as;
PS   ps;
int  format;
{
register Attr_Sequence eptr;

	for(eptr = as; eptr != NULLATTR; eptr=eptr->attr_link)
		as_comp_print (ps,eptr,format);

}

