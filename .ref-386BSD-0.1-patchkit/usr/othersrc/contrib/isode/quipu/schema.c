/* schema.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/schema.c,v 7.4 91/02/22 09:39:48 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/schema.c,v 7.4 91/02/22 09:39:48 mrose Interim $
 *
 *
 * $Log:	schema.c,v $
 * Revision 7.4  91/02/22  09:39:48  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:54:43  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:46:34  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:19:09  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:18:03  mrose
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


#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/ds_error.h"

extern int oidformat;
extern LLog * log_dsap;

extern AttributeType at_schema;
extern OID alias_oc;
Attr_Sequence entry_find_type();

check_avs_schema (at,avs_oc)
AttributeType at;
AV_Sequence avs_oc;
{
table_seq optr;
AV_Sequence avs;
objectclass * oc;

	optr = NULLTABLE_SEQ;
	for (avs = avs_oc; avs != NULLAV; avs = avs->avseq_next) {
	    oc = (objectclass *) avs->avseq_av.av_struct;
	    optr=oc->oc_must;
	    if ((optr == NULLTABLE_SEQ) && (oc->oc_may == NULLTABLE_SEQ) && (oc->oc_hierachy == NULLOCSEQ)) 
		return OK;	/* unknown object class */
	    for (; optr!=NULLTABLE_SEQ;  optr=optr->ts_next) 
		if (at == optr->ts_oa)
				break;
	    if (optr != NULLTABLE_SEQ)
	    	break;

	    for (optr=oc->oc_may; optr!=NULLTABLE_SEQ;  optr=optr->ts_next) 
		if (at == optr->ts_oa)
			break;
	    if (optr != NULLTABLE_SEQ)
	    	break;
	}

	if (optr == NULLTABLE_SEQ) 
		return (NOTOK);

	return OK;

}


real_check_schema (eptr,as,error)
Entry eptr;
Attr_Sequence as;
struct DSError * error;
{
register Attr_Sequence at;
table_seq optr;
AV_Sequence avs;
AV_Sequence avs_oc;
AV_Sequence tavs = NULLAV;
objectclass * oc;
extern OID alias_oc;

	shadow_entry (eptr);

	if (eptr->e_data != E_DATA_MASTER)
		return (OK);	/* only check schema of MASTERed entries */

	if (eptr->e_parent == NULLENTRY)
		return (OK);    /* no schema for root */

	avs_oc = avs = eptr->e_oc;

	if ((at = as_find_type (eptr->e_parent->e_attributes,at_schema)) != NULLATTR) {
		/* what should default be !!! */
		
		tavs = at->attr_value;
		/* make sure object class is allowed */
		if (test_schema (tavs,avs) != OK) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("Specified object class is not in the tree structure (schema) list"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_NAMINGVIOLATION;
			return (NOTOK);
		}
	}

	/* now check 'must contain' attributes */
	for (; avs != NULLAV; avs = avs->avseq_next) {
	    oc = (objectclass *) avs->avseq_av.av_struct;
	    for (optr=oc->oc_must; optr!=NULLTABLE_SEQ;  optr=optr->ts_next) {
		at = (as == NULLATTR) ? eptr->e_attributes : as;
		for (; at!=NULLATTR; at=at->attr_link)
			if (at->attr_type == optr->ts_oa)
				break;

		if (at == NULLATTR) {
			if (eptr->e_iattr) {
				if (eptr->e_iattr->i_always
					&& (as_find_type (eptr->e_iattr->i_always,optr->ts_oa)))
					break;
				if (eptr->e_iattr->i_default
					&& (as_find_type (eptr->e_iattr->i_default,optr->ts_oa)))
					break;
			}
			LLOG (log_dsap,LLOG_EXCEPTIONS,("'Must' attribute missing '%s'",attr2name(optr->ts_oa,OIDPART)));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
			return (NOTOK);
		}
	   }
	}


	/* Now try the 'may' contain bits */
	/* BUT not if "alias" */

	if ( check_in_oc (alias_oc, avs_oc) )
		return (OK);

	at = (as == NULLATTR) ? eptr->e_attributes : as;
	for (; at!=NULLATTR; at=at->attr_link) {
		if (check_avs_schema (at->attr_type,avs_oc) == NOTOK) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("attribute '%s' not allowed in the specified objectclass",attr2name(at->attr_type,OIDPART)));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
			return (NOTOK);
		}
	}

	if ((as == NULLATTR) && eptr->e_iattr) {
		/* Check inherited ones as well */
		for (at=eptr->e_iattr->i_default; at!=NULLATTR; at=at->attr_link) {
			if (check_avs_schema (at->attr_type,avs_oc) == NOTOK) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("default attribute '%s' not allowed in the specified objectclass",attr2name(at->attr_type,OIDPART)));
				error->dse_type = DSE_UPDATEERROR;
				error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
				return (NOTOK);
			}
		}
		for (at=eptr->e_iattr->i_always; at!=NULLATTR; at=at->attr_link) {
			if (check_avs_schema (at->attr_type,avs_oc) == NOTOK) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("always attribute '%s' not allowed in the specified objectclass",attr2name(at->attr_type,OIDPART)));
				error->dse_type = DSE_UPDATEERROR;
				error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
				return (NOTOK);
			}
		}

	}
	return (OK);

}

check_schema_type (eptr,attr,error)
Entry eptr;
AttributeType attr;
struct DSError * error;
{
Attr_Sequence at;
AV_Sequence avs;
AV_Sequence tavs = NULLAV;

	DLOG (log_dsap,LLOG_TRACE,("check schema type"));

	if (eptr->e_parent == NULLENTRY)
		return (OK);    /* no schema for root */

	avs = eptr->e_oc;

	if ((at = as_find_type (eptr->e_parent->e_attributes,at_schema)) != NULLATTR) {
		tavs = at->attr_value;
		if (test_schema (tavs,avs) != OK) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("given objectclass not in schema (tree structure) list"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_NAMINGVIOLATION;
			return (NOTOK);
		}
	}

	/* Now try the 'may' contain bits */
	/* BUT not if "alias" */

	if ( check_in_oc (alias_oc, avs) )
		return (OK);

	if (check_avs_schema(attr,avs) == NOTOK) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("attribute type '%s' not allowed in the specified objectclass",attr2name(attr,OIDPART)));
		error->dse_type = DSE_UPDATEERROR;
		error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
		return (NOTOK);
	}
	return (OK);

}


test_schema (tree,oc)
AV_Sequence tree;
AV_Sequence oc;
{
AV_Sequence aptr, tavs;
struct tree_struct *tptr;
char found;
objectclass * oc1;

	if (oc == NULLAV)
		return (NOTOK);

	for (aptr=oc; aptr!= NULLAV; aptr=aptr->avseq_next) {
		found = FALSE;
		for (tavs=tree; tavs!=NULLAV ;tavs=tavs->avseq_next) {
			tptr = (struct tree_struct *) tavs->avseq_av.av_struct;
			if (tptr->tree_object == NULLOBJECTCLASS) {
				/* is this correct behaviour ? */
				found = TRUE;
				break;
			}
			oc1 = (objectclass *) aptr->avseq_av.av_struct;
			if (test_hierarchy (tptr->tree_object, oc1) == 0) {
				found = TRUE;
				break;
			}
		}
		if (found == FALSE) {
			return (NOTOK);
		}
	}
	return (OK);
}

test_hierarchy (a,b)    /* see if b in oc a */
objectclass *a, *b;
{
struct oc_seq * oidseq;

	if ( a == b )
		return OK;

	for (oidseq = a->oc_hierachy; oidseq != NULLOCSEQ; oidseq = oidseq->os_next) 
		if (test_hierarchy (oidseq->os_oc,b) == OK)
			return (OK);
		
	return (NOTOK);
}


check_oc_hierarchy (avs)
AV_Sequence avs;
{
AV_Sequence avs1, avs2;
struct oc_seq * oidseq;
objectclass *oc1, *oc2;
char found = FALSE;
objectclass * str2oc();
static objectclass * topoc = NULLOBJECTCLASS;

	if (topoc == NULLOBJECTCLASS) 
		topoc = str2oc (TOP_OC);

	/* Check the OC attribute has all the hierarchy elements */
	/* ALWAYS the case with Quipu - but other implementations... */

	for ( avs1 = avs; avs1 != NULLAV ; avs1=avs1->avseq_next) {
		oc1 = (objectclass *) avs1->avseq_av.av_struct;
		for (oidseq = oc1->oc_hierachy; oidseq != NULLOCSEQ; oidseq = oidseq->os_next) {
			for ( avs2 = avs; avs2 != NULLAV ; avs2=avs2->avseq_next) {
				oc2 = (objectclass *) avs2->avseq_av.av_struct;
				if (objclass_cmp(oidseq->os_oc,oc2) == 0) {
					found = TRUE;
					break;
				}
			}
			if ( ! found ) {
				/* make sure it is not the 'top' special case */
				if (objclass_cmp(topoc,oidseq->os_oc) != 0) {
					LLOG (log_dsap, LLOG_EXCEPTIONS, ("Objectclass %s missing for OC attribute hierarchy", oc2name (oidseq->os_oc,OIDPART)));
					return FALSE;
				}
			}
		}
	}
	return TRUE;
}
