/* dsp_cache.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/dsp_cache.c,v 7.2 91/02/22 09:39:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/dsp_cache.c,v 7.2 91/02/22 09:39:16 mrose Interim $
 *
 *
 * $Log:	dsp_cache.c,v $
 * Revision 7.2  91/02/22  09:39:16  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:12  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:27  mrose
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
#include "quipu/dua.h"
#include "quipu/list.h"
#include "quipu/entry.h"
#include "quipu/common.h"
#include "quipu/dsargument.h"
#include "quipu/dsap.h"

extern time_t timenow;
extern LLog * log_dsap;
extern int local_cache_size;
extern Attr_Sequence as_merge_aux ();
extern Entry local_find_entry_aux();

Entry cache_dsp_entry (ptr)
EntryInfo      *ptr;
{
	/* assumes entry passed is complete */

	Entry           make_path ();
	Entry		eptr;

	Attr_Sequence   asptr;
	extern AttributeType at_acl;

	struct DSError  error;
	DN              dnptr;
	char		aclfound = FALSE;

	for (asptr = ptr->ent_attr; asptr != NULLATTR; asptr = asptr->attr_link) {
		if (asptr->attr_type == at_acl) {
			aclfound = TRUE;
			break; 
		}
	}

	if (!aclfound) {
		LLOG (log_dsap,LLOG_NOTICE,("No ACL in dsp_cache"));
		return NULLENTRY;		/* don't cache if no acl */
	}

	for (dnptr = ptr->ent_dn; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
		;

	if ((eptr = local_find_entry_aux (ptr->ent_dn, FALSE)) != NULLENTRY) {
		if ((eptr->e_data == E_TYPE_CACHE_FROM_MASTER) ||
		    (eptr->e_data == E_TYPE_CONSTRUCTOR)) {	
			as_free (eptr->e_attributes);
			eptr->e_attributes = as_cpy(ptr->ent_attr);
			eptr->e_complete = TRUE;
			eptr->e_data = E_TYPE_CACHE_FROM_MASTER;
			eptr->e_age = timenow;
		} else {
			return NULLENTRY;
		}
	} else {
		local_cache_size++;
		eptr = make_path (ptr->ent_dn);
		eptr->e_name = rdn_cpy (dnptr->dn_rdn);
		eptr->e_complete = TRUE;
		eptr->e_data = E_TYPE_CACHE_FROM_MASTER;
		eptr->e_attributes = as_cpy(ptr->ent_attr);
		eptr->e_age = timenow;
	}

	if (unravel_attribute (eptr,&error) == NOTOK) {
		/* Keep name, but throw away attributes */
		local_cache_size--;
		eptr->e_data = E_TYPE_CONSTRUCTOR;
		eptr->e_complete = FALSE;
		as_free (eptr->e_attributes);
		eptr->e_attributes = NULLATTR;
		log_ds_error (&error);
		ds_error_free (&error);
		return NULLENTRY;
	}
	return (eptr);
}



dsp_cache (arg,res,ctx,binddn)
struct DSArgument *arg;
struct DSResult   *res;
char ctx;
DN binddn;
{
EntryInfo *ptr;
Entry entryptr;
Attr_Sequence as, eis_select (), attr_eis_select ();

    switch(arg->arg_type) {    
    case OP_READ:
	if (ctx == DS_CTX_X500_DAP)
	   if ((entryptr = cache_dsp_entry (&res->res_rd.rdr_entry)) != NULLENTRY) {
	    
		/* remove acl if DAP user not allowed it */
		as_free (res->res_rd.rdr_entry.ent_attr);
		if ((res->res_rd.rdr_entry.ent_attr = eis_select (
				arg->arg_rd.rda_eis, entryptr, 
				binddn, FALSE,
				res->res_rd.rdr_entry.ent_dn)) == NULLATTR) {
			/* TODO return error nosuchattributes */
			;
		}
	   } else {
	        as = res->res_rd.rdr_entry.ent_attr;
		if ((res->res_rd.rdr_entry.ent_attr = attr_eis_select (
				arg->arg_rd.rda_eis,
				as, binddn, 
				res->res_rd.rdr_entry.ent_dn)) == NULLATTR) {
			/* TODO return error nosuchattributes */
			;
		}
	        as_free (as);
	   }
	break;
    case OP_SEARCH:
 	if ((arg->arg_sr.sra_eis.eis_allattributes == TRUE) &&
 	    (arg->arg_sr.sra_eis.eis_infotypes == EIS_ATTRIBUTESANDVALUES)) {
	 	for (ptr = res->res_sr.CSR_entries; ptr != NULLENTRYINFO; ptr = ptr->ent_next) 
 			(void) cache_dsp_entry (ptr);
	}
     	break;
    case OP_LIST:
	if (ctx == DS_CTX_QUIPU_DSP)
		cache_list (res->res_ls.lsr_subordinates,
 		    res->res_ls.lsr_limitproblem,
 		    arg->arg_ls.lsa_object,
 		    arg->arg_ls.lsa_common.ca_servicecontrol.svc_sizelimit);
     	break;

    /* the following change an entry - the easiest thing is to
       deleted the cached entry and start again */
    case OP_ADDENTRY:
	delete_cache (arg->arg_ad.ada_object);
	break;
    case OP_REMOVEENTRY:
	delete_cache (arg->arg_rm.rma_object);
    	break;
    case OP_MODIFYENTRY:
	delete_cache (arg->arg_me.mea_object);
	break;
    case OP_MODIFYRDN:
	delete_cache (arg->arg_mr.mra_object);
	break;
    default:    
	break;
    }
}
