/* showattr.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/showattr.c,v 7.2 91/02/22 09:40:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/showattr.c,v 7.2 91/02/22 09:40:53 mrose Interim $
 *
 *
 * $Log:	showattr.c,v $
 * Revision 7.2  91/02/22  09:40:53  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:55:40  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:21  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/entry.h"

extern Entry    current_entry;
extern DN       current_dn;
extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	rps, opt;

extern char     value_flag;
extern char     key_flag;
extern char	print_format;
extern char	show_all_flag;

static Attr_Sequence ignore_attr = NULLATTR;
static char ignore_unknown = FALSE;

showattribute (at)
AttributeType at;
{
Attr_Sequence  eptr;

	if (! show_all_flag) {
		if ( ! check_want_attr (at))
			return;
	} else if ( ! check_want_tmp_attr (at))
		return;

	for (eptr = current_entry->e_attributes; eptr != NULLATTR; eptr = eptr->attr_link) {	
		/* Tiptoe through the list of types until one matches, and then print value. */
		if (AttrT_cmp (eptr->attr_type,at) == 0) {
			if (value_flag) 
				if (key_flag)
					as_comp_print (RPS,eptr,print_format);
				else
					avs_print (RPS,eptr->attr_value,print_format);
			else {
				AttrT_print (RPS,at,print_format);
				ps_print (RPS, "\n");
			}
			break;
		} 
	}
		
	if (eptr == NULLATTR)
		if (key_flag)
			ps_printf (OPT, "%-21s - (No such attribute in this entry)\n", at->oa_ot.ot_name);
		else
			ps_printf (OPT, "No value\n");
		
}

show_unknown ()
{
	ignore_unknown = TRUE;
}

check_want_attr (at)
AttributeType at;
{
Attr_Sequence as;

	if (at == NULLTABLE_ATTR) 
		return (ignore_unknown);

	if (at->oa_syntax == 0)
		return (ignore_unknown);

	for (as=ignore_attr; as != NULLATTR; as=as->attr_link)
		if (AttrT_cmp (at,as->attr_type) == 0)
			return (FALSE);

	return (check_want_tmp_attr(at));
}

check_want_tmp_attr (at)
AttributeType at;
{
Attr_Sequence as;
Attr_Sequence as2;
extern Attr_Sequence tmp_ignore;
extern Attr_Sequence as_flag;

	if (at->oa_syntax == 0)
		return (ignore_unknown || show_all_flag);

	for (as=tmp_ignore; as != NULLATTR; as=as->attr_link)
		if (AttrT_cmp (at,as->attr_type) == 0) {
			/* may be explicitly wanted... */
			for (as2=as_flag; as2 != NULLATTR; as2=as2->attr_link)
				if (AttrT_cmp (as2->attr_type,as->attr_type) == 0) 
					return (TRUE);
			return (FALSE);
		}

	return (TRUE);
}

new_ignore (ptr)
char * ptr;
{
AttributeType at;
Attr_Sequence newas;

	if ((at = str2AttrT (ptr)) == NULLAttrT)
		return;
	newas = as_comp_new (at,NULLAV,NULLACL_INFO);
	ignore_attr = as_merge(ignore_attr,newas);
}
