/* ds_compare.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_compare.c,v 7.3 91/02/22 09:38:47 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_compare.c,v 7.3 91/02/22 09:38:47 mrose Interim $
 *
 *
 * $Log:	ds_compare.c,v $
 * Revision 7.3  91/02/22  09:38:47  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:53:41  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:45:40  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:06  mrose
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
#include "quipu/compare.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern LLog * log_dsap;
extern Attr_Sequence entry_find_type();

static attribute_not_cached ();

do_ds_compare (arg, error, result, binddn, target, di_p, dsp)
    struct ds_compare_arg       *arg;
    struct ds_compare_result    *result;
    struct DSError              *error;
    DN                          binddn;
    DN                          target;
    struct di_block		**di_p;	
    char			dsp;
{
Entry  entryptr;
register Attr_Sequence  as;
Attr_Sequence ias = NULLATTR;
register AV_Sequence tmp;
struct acl_info  * acl;
register int i;
int retval;
DN realtarget;

	DLOG (log_dsap,LLOG_TRACE,("ds_compare"));

	if (!dsp)
		target = arg->cma_object;

	if (target == NULLDN) {
		error->dse_type = DSE_NAMEERROR;
		error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		error->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_ERROR_REMOTE);
	}

	switch(find_entry(target, &(arg->cma_common), binddn, NULLDNSEQ, FALSE, &(entryptr), error, di_p, OP_COMPARE))
	{
	case DS_OK:
	    /* Filled out entryptr - carry on */
	    break;
	case DS_CONTINUE:
	    /* Filled out di_p - what do we do with it ?? */
	    return(DS_CONTINUE);

	case DS_X500_ERROR:
	    /* Filled out error - what do we do with it ?? */
	    return(DS_X500_ERROR);
	default:
	    /* SCREAM */
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_compare() - find_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	/* Strong authentication  */
	if ((retval = check_security_parms((caddr_t) arg,
			_ZCompareArgumentDataDAS,
			&_ZDAS_mod,
			arg->cma_common.ca_security,
			arg->cma_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		return (DS_ERROR_REMOTE);
	}

	realtarget = get_copy_dn(entryptr);

	if (arg->cma_purported.ava_type == NULLTABLE_ATTR) {
		int res = invalid_matching (arg->cma_purported.ava_type,error,realtarget);
		dn_free (realtarget);
		return res;
	}

	if (check_acl (dsp ? NULLDN : binddn,ACL_COMPARE,entryptr->e_acl->ac_entry, realtarget) == NOTOK) {
		if (dsp && (check_acl (binddn,ACL_COMPARE,entryptr->e_acl->ac_entry, realtarget) == OK)) {
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		} else {
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		}
	}

	if ((as = as_find_type (entryptr->e_attributes,arg->cma_purported.ava_type)) == NULLATTR) {
	    if ((as = entry_find_type (entryptr, arg->cma_purported.ava_type)) == NULLATTR) {
		if (attribute_not_cached (entryptr,binddn,grab_oid(arg->cma_purported.ava_type),realtarget,ACL_COMPARE)) {
			int res = referral_dsa_info(realtarget,NULLDNSEQ,FALSE,entryptr,error,di_p,
					arg->cma_common.ca_servicecontrol.svc_options & SVC_OPT_PREFERCHAIN);
			dn_free (realtarget);
			return res;
		}

		dn_free (realtarget);
		error->dse_type = DSE_ATTRIBUTEERROR;
		error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn(entryptr);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_NOSUCHATTRIBUTE;
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy(arg->cma_purported.ava_type);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
		error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
		return (DS_ERROR_REMOTE);
	    }
	} else {
		/* see if there is an 'always' attribute */
		Attr_Sequence ptr;
		if (entryptr->e_iattr) {
			for(ptr = entryptr->e_iattr->i_always; ptr != NULLATTR; ptr=ptr->attr_link) {
				if (  (i = AttrT_cmp (ptr->attr_type,arg->cma_purported.ava_type)) <= 0) {
					if ( i == 0 ) 
						ias = ptr;
					break;
				}
			}
		}
		   
	}

	result->cmr_object = NULLDN;

again:;

	acl =  as->attr_acl;

	if (check_acl (dsp ? NULLDN : binddn,ACL_COMPARE, acl,realtarget) == NOTOK) {
		if (dsp && (check_acl (binddn,ACL_COMPARE, acl, realtarget) == OK)) {
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		} else {
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		}
	}

	result->cmr_iscopy = entryptr->e_data;
	result->cmr_common.cr_requestor = NULLDN;

	/* if no error and NOT SVC_OPT_DONTDEREFERENCEALIASES then */
	/* the alias will have been derefeferenced -signified by   */
	/* NO_ERROR !!! */
	if (error->dse_type == DSE_NOERROR) {
		result->cmr_common.cr_aliasdereferenced =  FALSE;
	} else {
		result->cmr_common.cr_aliasdereferenced =  TRUE;
		if (result->cmr_object == NULLDN)
			result->cmr_object = get_copy_dn (entryptr);
	}
		
	for (tmp = as->attr_value; tmp != NULLAV; tmp = tmp->avseq_next) {
	  i = AttrV_cmp (&tmp->avseq_av, arg->cma_purported.ava_value);
	  switch (i) {
	    case 0 :
		result->cmr_matched= TRUE;
		dn_free (realtarget);
		return (DS_OK);
	    case 1:
	    case -1:
	    case 2:
		break;
	    default:
		error->dse_type = DSE_ATTRIBUTEERROR;
		error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (entryptr);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_INAPPROPRIATEMATCHING;
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy(as->attr_type);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = AttrV_cpy(arg->cma_purported.ava_value);
		error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
		dn_free (realtarget);
		return (NOTOK);
	  }
	}

	if (ias) {
		/* try again with inherited attribute */
		as = ias;
		ias = NULLATTR;
		goto again;
	}

	dn_free (realtarget);
	result->cmr_matched= FALSE;
	return (DS_OK);

}

invalid_matching (at,error,dn)
AttributeType at;
struct DSError *error;
DN dn;
{
	error->dse_type = DSE_ATTRIBUTEERROR;
	error->ERR_ATTRIBUTE.DSE_at_name = dn_cpy (dn);
	error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_INAPPROPRIATEMATCHING;
	error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (at);
	error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
	error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
	return (DS_ERROR_REMOTE);
}


static attribute_not_cached (ptr,dn,at,target,level)
Entry ptr;
DN dn;
OID at;
DN target;
int level;
{
register struct acl_attr * aa;
register struct oid_seq * oidptr;

	/* FACT: the attribute is not present in the entry.
	 * PROBLEM: should it be ?
	 * 	Return TRUE if yes.
         */

	if (dn == NULLDN)
		return FALSE;	/* Not in cache implies not publicly readable... */

	if ((ptr->e_data == E_DATA_MASTER) || (ptr->e_data == E_TYPE_SLAVE))
		return FALSE;

	/* see if more than cached data is required */	
	if (ptr->e_acl->ac_attributes == NULLACL_ATTR)
		return FALSE;

	for ( aa = ptr->e_acl->ac_attributes; aa!=NULLACL_ATTR; aa=aa->aa_next)
		for ( oidptr=aa->aa_types;oidptr != NULLOIDSEQ; oidptr=oidptr->oid_next)
			if (oid_cmp (oidptr->oid_oid,at) == 0) {
				/* The attribute is in the attribute ACL list */
				/* Would a referral help the DUA ? */
				if (check_acl (dn,level,aa->aa_acl,target) == NOTOK) 
					return FALSE;
				else 
					return TRUE;
			}	

	if (check_acl (NULLDN,ACL_READ,ptr->e_acl->ac_default,target) == NOTOK) 
		if (check_acl (dn,ACL_READ,ptr->e_acl->ac_default,target) == NOTOK) 
			return TRUE;

	return FALSE;

}
