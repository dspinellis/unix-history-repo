/* ds_read.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_read.c,v 7.3 91/02/22 09:38:56 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_read.c,v 7.3 91/02/22 09:38:56 mrose Interim $
 *
 *
 * $Log:	ds_read.c,v $
 * Revision 7.3  91/02/22  09:38:56  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:53:50  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:45:48  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:14  mrose
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
#include "quipu/read.h"
#include "quipu/policy.h"
#include "quipu/connection.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern LLog * log_dsap;

Attr_Sequence eis_select ();
Attr_Sequence dsa_eis_select ();
extern Attr_Sequence entry_find_type();
static cant_use_cache();
static attribute_not_cached ();
extern AttributeType at_control;
extern unsigned bind_policy;
extern unsigned strong_policy;
extern DN mydsadn;
extern struct di_block * di_alloc();

do_ds_read (arg, error, result, binddn, target, di_p, dsp, quipu_ctx)
    struct ds_read_arg          *arg;
    struct ds_read_result       *result;
    struct DSError              *error;
    DN                          binddn;
    DN                          target;
    struct di_block		**di_p;
    char 			dsp;
    char 			quipu_ctx;
{
Entry  entryptr;
int retval;
int authenticated;
DN realtarget;

	DLOG (log_dsap,LLOG_TRACE,("ds_read"));

	if (!dsp)
		target = arg->rda_object;

	if (!dsp && dsa_read_control(arg,result))
		return (DS_OK);

	if (target == NULLDN) {
		/* can't read from the root */
		error->dse_type = DSE_NAMEERROR;
		error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		error->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_ERROR_REMOTE);
	}

	switch(find_entry(target,&(arg->rda_common),binddn,NULLDNSEQ,FALSE,&(entryptr), error, di_p, OP_READ))
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
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_read() - find_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	realtarget = get_copy_dn (entryptr);

	/* User can be authenticated by using strong authentication for this
	 * operation, or by using the credntials from the bind.
	 */

	authenticated = 0;
	
	/* Credentials provided in the ds_bind */
	if ((bind_policy & POLICY_ACCESS_READ) && (!dsp) && (binddn != NULLDN))
		authenticated = 1;

	/* Strong authentication  */
	/* If it's there, check it, even if you won't believe it anyway */
	if ((retval = check_security_parms((caddr_t) arg,
			_ZReadArgumentDataDAS,
			&_ZDAS_mod,
			arg->rda_common.ca_security,
			arg->rda_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		dn_free (realtarget);
		return (DS_ERROR_REMOTE);
	}

	if ((strong_policy & POLICY_ACCESS_READ) && 
		(arg->rda_common.ca_sig != (struct signature *) 0))
		authenticated = 1;

	/* entry has got a full list of attributes,  eventually
	   select one required */
	if (check_acl (authenticated ? binddn : NULLDN,ACL_READ,entryptr->e_acl->ac_entry, realtarget) == NOTOK) {
		if (dsp && (check_acl (binddn,ACL_READ,entryptr->e_acl->ac_entry, realtarget) == OK)) {
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

	if (entryptr->e_dsainfo && need_pseudo_dsa(entryptr,arg)) {

		/* Its a 7.0 or better DSA.
		 * pseudo attributes asked for.
		 * special routing req'd 
                 */

		if (dn_cmp (realtarget, mydsadn) == 0) {
			/* Its me - generate result */

			if ((result->rdr_entry.ent_attr = dsa_eis_select (
				arg->rda_eis,entryptr, dsp ? NULLDN : binddn,
				quipu_ctx, realtarget)) != NULLATTR)
				goto out;

			error->dse_type = DSE_ATTRIBUTEERROR;
			error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (entryptr);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
			if (arg->rda_eis.eis_select != NULLATTR)
				error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy(arg->rda_eis.eis_select->attr_type);
			else
				error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = NULLAttrT;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);

		} else {
			/* make referral to DSA in this entry */
			(*di_p) = di_alloc();
			(*di_p)->di_type = DI_TASK;
			(*di_p)->di_dn = realtarget;
			(*di_p)->di_target = dn_cpy(realtarget);
			(*di_p)->di_reftype = RT_UNDEFINED;
			(*di_p)->di_rdn_resolved = CR_RDNRESOLVED_NOTDEFINED;
			(*di_p)->di_aliasedRDNs = CR_NOALIASEDRDNS;
			(*di_p)->di_entry = entryptr;
			entryptr->e_refcount++;
			(*di_p)->di_state = DI_COMPLETE;
			
			return DS_CONTINUE;
		}
			   
	} 

	if (cant_use_cache (entryptr,binddn,arg->rda_eis,realtarget)) {
		int res =  referral_dsa_info(realtarget,NULLDNSEQ,FALSE,entryptr,error,di_p, 
			arg->rda_common.ca_servicecontrol.svc_options & SVC_OPT_PREFERCHAIN);
		dn_free (realtarget);
		return res;
	}

	if (dsp && (eis_check (arg->rda_eis,entryptr, binddn) != OK)) {
		/* Can only send public things over DSP - but user is entitled to more */
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
		dn_free (realtarget);
		return (DS_ERROR_REMOTE);
	}

	if ((result->rdr_entry.ent_attr = eis_select (arg->rda_eis,entryptr, dsp ? NULLDN : binddn, quipu_ctx, realtarget)) == NULLATTR)
		if (! arg->rda_eis.eis_allattributes) {
			error->dse_type = DSE_ATTRIBUTEERROR;
                        error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (entryptr);
                        error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
			if (arg->rda_eis.eis_select != NULLATTR)
	                        error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy(arg->rda_eis.eis_select->attr_type);
			else
	                        error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = NULLAttrT;
                        error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
                        error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		}

out:;
	result->rdr_entry.ent_dn = realtarget;
	result->rdr_entry.ent_iscopy = entryptr->e_data;
	result->rdr_entry.ent_age = (time_t) 0;
	result->rdr_entry.ent_next = NULLENTRYINFO;
	result->rdr_common.cr_requestor = NULLDN;
	/* if no error and NOT SVC_OPT_DONTDEREFERENCEALIASES then */
	/* the alias will have been derefeferenced -signified by   */
	/* NO_ERROR !!! */
	result->rdr_common.cr_aliasdereferenced = (error->dse_type == DSE_NOERROR) ? FALSE : TRUE;
	return (DS_OK);

}

static cant_use_cache (ptr,dn,eis,target)
Entry ptr;
DN dn;
EntryInfoSelection eis;
DN target;
{
register Attr_Sequence as;
char dfltacl = FALSE;

	if (dn == NULLDN) 
		return FALSE;

	if ((ptr->e_data == E_DATA_MASTER) || (ptr->e_data == E_TYPE_SLAVE))
		return FALSE;

	/* see if more than cached data is required */	

	if (eis.eis_allattributes) {
		struct acl_attr * aa;
		struct oid_seq * oidptr;
		/* look for attr acl */
		/* see if any attributes use can see */

		if (check_acl (NULLDN,ACL_READ,ptr->e_acl->ac_default,target) == NOTOK) 
			if (check_acl (dn,ACL_READ,ptr->e_acl->ac_default,target) == OK) 
				return TRUE;

		if (ptr->e_acl->ac_attributes == NULLACL_ATTR)
			return FALSE;

		for ( aa = ptr->e_acl->ac_attributes; aa!=NULLACL_ATTR; aa=aa->aa_next)
			for ( oidptr=aa->aa_types;oidptr != NULLOIDSEQ; oidptr=oidptr->oid_next)
				/* The attribute is in the attribute ACL list */
				/* Would a referral help the DUA ? */
				if (check_acl (NULLDN,ACL_READ,aa->aa_acl,target) == NOTOK) 
					if (check_acl (dn,ACL_READ,aa->aa_acl,target) == OK) 
						return TRUE;

	} else {
		/* for each attribute in eis.eis_select, see is user
		   entitled to it. */

		if (check_acl (NULLDN,ACL_READ,ptr->e_acl->ac_default,target) == NOTOK) 
			if (check_acl (dn,ACL_READ,ptr->e_acl->ac_default,target) == OK) 
				dfltacl = TRUE;

		for(as=eis.eis_select; as != NULLATTR; as=as->attr_link) {
			if (entry_find_type (ptr, as->attr_type) == NULLATTR) 
				if (attribute_not_cached (ptr,dn,grab_oid(as->attr_type),target,ACL_READ,dfltacl))
					return TRUE;

		}
	}
	return FALSE;
}

static attribute_not_cached (ptr,dn,at,target,level,dfltacl)
Entry ptr;
DN dn;
OID at;
DN target;
int level;
char dfltacl;
{
register struct acl_attr * aa;
register struct oid_seq * oidptr;

	/* see if more than cached data is required */	
	if (ptr->e_acl->ac_attributes == NULLACL_ATTR)
		return (dfltacl);

	for ( aa = ptr->e_acl->ac_attributes; aa!=NULLACL_ATTR; aa=aa->aa_next)
		for ( oidptr=aa->aa_types;oidptr != NULLOIDSEQ; oidptr=oidptr->oid_next)
			if (oid_cmp (oidptr->oid_oid,at) == 0) {
				/* The attribute is in the attribute ACL list */
				/* Would a referral help the DUA ? */
				if (check_acl (NULLDN,level,aa->aa_acl,target) == NOTOK) 
					if (check_acl (dn,level,aa->aa_acl,target) == OK) 
						return TRUE;
				return FALSE;
			}	
	return (dfltacl);

}


static Attr_Sequence  dsa_control_info()
{
extern int slave_edbs;
extern int master_edbs;
extern int local_master_size;
extern int local_slave_size;
extern int local_cache_size;
char buffer [LINESIZE];
Attr_Sequence as;

	(void) sprintf (buffer,"%d Master entries (in %d EDBs), %d Slave entries (in %d EDBs), %d Cached entries",
		local_master_size,master_edbs,local_slave_size,slave_edbs,local_cache_size);

	as=as_comp_alloc();
	as->attr_acl = NULLACL_INFO;
	as->attr_type = at_control;
	as->attr_link = NULLATTR;
        if ((as->attr_value = str2avs (buffer,as->attr_type)) == NULLAV) {
                as_free (as);
                return (NULLATTR);
	}

	return (as);
}

dsa_read_control (arg,result)
    struct ds_read_arg          *arg;
    struct ds_read_result       *result;
{
extern DN mydsadn;

	if ((arg->rda_eis.eis_allattributes) || 
		(arg->rda_eis.eis_infotypes == EIS_ATTRIBUTETYPESONLY))
		return FALSE;

	if ((arg->rda_eis.eis_select == NULLATTR)
	   || (arg->rda_eis.eis_select->attr_link != NULLATTR))
		return FALSE;

	if (AttrT_cmp (at_control,arg->rda_eis.eis_select->attr_type) != 0)
		return FALSE;

	if ((result->rdr_entry.ent_attr = dsa_control_info()) == NULLATTR)
		return FALSE;

	/* Fiddle DN - for DUA caching !!! */
	result->rdr_entry.ent_dn = dn_cpy (mydsadn);

	result->rdr_entry.ent_iscopy = FALSE;
	result->rdr_entry.ent_age = (time_t) 0;
	result->rdr_entry.ent_next = NULLENTRYINFO;
	result->rdr_common.cr_requestor = NULLDN;
	result->rdr_common.cr_aliasdereferenced = FALSE;

	return TRUE;
}




need_pseudo_dsa (eptr,arg)
Entry eptr;
struct ds_read_arg *arg;
{
Attr_Sequence as;

	if (quipu_ctx_supported (eptr) != 2)
		return FALSE;

	if (!quipu_version_7 (eptr))
		return FALSE;

	if ((arg->rda_common.ca_servicecontrol.svc_options & SVC_OPT_DONTUSECOPY) != 0)
		return TRUE;

	if (arg->rda_eis.eis_allattributes)
		return FALSE;

	for (as = arg->rda_eis.eis_select; as!= NULLATTR; as=as->attr_link) {
		if (check_avs_schema (as->attr_type, eptr->e_oc) != OK)
			return TRUE;
	}

	return FALSE;  
}

