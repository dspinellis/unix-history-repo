/* entryinfo.c - Entry Information routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/entryinfo.c,v 7.3 91/02/22 09:19:10 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/entryinfo.c,v 7.3 91/02/22 09:19:10 mrose Interim $
 *
 *
 * $Log:	entryinfo.c,v $
 * Revision 7.3  91/02/22  09:19:10  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:41:49  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:34:26  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:42:11  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/commonarg.h"

entryinfo_comp_free (a,state)
register EntryInfo *a;
int state;
{
register EntryInfo * einfo;
register Attr_Sequence as;

	if (a == NULLENTRYINFO)
		return;

	dn_free (a->ent_dn);
	if (state == 1)
		for ( as=a->ent_attr; as!= NULLATTR; as=as->attr_link)
			free ((char *) as);
	else
		as_free (a->ent_attr);

	for (einfo=a->ent_next; einfo!=NULLENTRYINFO; einfo=einfo->ent_next) {
		dn_free (einfo->ent_dn);
		if (state == 1)
			for ( as=einfo->ent_attr; as!= NULLATTR; as=as->attr_link)
				free ((char *) as);
		else
			as_free (einfo->ent_attr);
		free ((char *) einfo);
	}
}

entryinfo_free (a,state)
register EntryInfo * a;
register int state;
{
	if (a == NULLENTRYINFO)
		return;
	entryinfo_comp_free (a,state);
	free ((char *) a);
}

entryinfo_cpy (a,b)
register EntryInfo *a;
register EntryInfo *b;
{
	a->ent_dn        = dn_cpy (b->ent_dn);
	a->ent_attr      = as_cpy (b->ent_attr);
	a->ent_iscopy    = b->ent_iscopy;
	a->ent_age       = b->ent_age;
	a->ent_next      = b->ent_next;
}

entryinfo_append (a,b)
register EntryInfo *a,*b;
{
register EntryInfo *ptr;

	if ( a  == NULLENTRYINFO )
		return;

	for (ptr=a; ptr->ent_next != NULLENTRYINFO; ptr=ptr->ent_next)
		;  /* noop */

	ptr->ent_next = b;
}

entryinfo_merge (a,b)
register EntryInfo *a,*b;
{
register EntryInfo *ptr;
EntryInfo *tmp, *prev, *trail;

	if (( a == NULLENTRYINFO )
	   || (b == NULLENTRYINFO ))
		return;

	for (ptr=a; ptr != NULLENTRYINFO; ptr=ptr->ent_next) {
		prev = NULLENTRYINFO;
		for (tmp=b; tmp != NULLENTRYINFO; tmp=tmp->ent_next) {
		 	if (dn_cmp (ptr->ent_dn, tmp->ent_dn) == OK) {
				/* already got it - throw it away */
				if (prev == NULLENTRYINFO)
					b = tmp->ent_next;
				else
					prev->ent_next = tmp->ent_next;
				as_free (tmp->ent_attr);
				dn_free (tmp->ent_dn);
				free ((char *)tmp);
				break;	
			}
			prev = tmp;
		}
		trail = ptr;
	}

	trail->ent_next = b;
}

entryinfo_print (ps,entryinfo,format)
PS  ps;
EntryInfo *entryinfo;
int format;
{
register EntryInfo *einfo;

	for (einfo= entryinfo; einfo!=NULLENTRYINFO; einfo=einfo->ent_next) {
		dn_print(ps,einfo->ent_dn,EDBOUT);
		ps_print (ps,"\n");
		if (einfo->ent_attr)
			as_print (ps,einfo->ent_attr,format);
		ps_print (ps,"\n");
	}
}
