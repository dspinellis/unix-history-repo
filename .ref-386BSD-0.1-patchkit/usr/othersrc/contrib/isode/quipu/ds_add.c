/* ds_add.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_add.c,v 7.4 91/02/22 09:38:41 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_add.c,v 7.4 91/02/22 09:38:41 mrose Interim $
 *
 *
 * $Log:	ds_add.c,v $
 * Revision 7.4  91/02/22  09:38:41  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:53:36  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:45:37  mrose
 * sync
 * 
 * Revision 7.1  90/01/11  18:37:19  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:17:03  mrose
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


#include "quipu/config.h"
#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/add.h"
#include "quipu/malloc.h"
#ifdef TURBO_AVL
#include "quipu/turbo.h"
#endif
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern Entry database_root;
extern LLog * log_dsap;
extern int local_master_size;
extern DN mydsadn;

extern AttributeType at_masterdsa;
extern AttributeType at_slavedsa;
extern AttributeType at_objectclass;

#ifdef TURBO_INDEX
extern AttributeType *turbo_index_types;
#endif

do_ds_addentry (arg, error, binddn,target,di_p,dsp)
    struct ds_addentry_arg      *arg;
    struct DSError              *error;
    DN                          binddn;
    DN                          target;
    struct di_block		**di_p;
    char dsp;
{
Entry  entryptr,ptr;
register DN  dntop, dn = NULLDN;
DN  trail = NULLDN;
extern Entry database_root;
ContinuationRef cont_ref_parent ();
char * new_version ();
int retval;
extern int read_only;
#ifdef TURBO_AVL
extern int	entry_cmp();
#endif

	DLOG (log_dsap,LLOG_TRACE,("ds_add"));

	if (!dsp)
		target = arg->ada_object;

	/* stop aliases being dereferenced */
	arg->ada_common.ca_servicecontrol.svc_options |= SVC_OPT_DONTDEREFERENCEALIAS;

	error ->dse_type = DSE_NOERROR;
	/* first of all see if entry exists */

	if (target == NULLDN) {
		error->dse_type = DSE_NAMEERROR;
		error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		error->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_ERROR_REMOTE);
	}

	switch (find_entry (target,&(arg->ada_common),binddn,NULLDNSEQ,TRUE,&entryptr, error, di_p, OP_ADDENTRY)) 
	{
	case DS_OK:
		error->dse_type = DSE_UPDATEERROR;
		error->ERR_UPDATE.DSE_up_problem = DSE_UP_ALREADYEXISTS;
		return(DS_ERROR_REMOTE);
	case DS_CONTINUE:
	    /* Filled out di_p - what do we do with it ?? */
	    return(DS_CONTINUE);
	case DS_X500_ERROR:
	    /* Filled out error - what do we do with it ?? */
	    if ((error->dse_type != DSE_NAMEERROR) || (error->ERR_NAME.DSE_na_problem != DSE_NA_NOSUCHOBJECT)) {
		return(DS_X500_ERROR);
	    }
	    ds_error_free (error);  /* not interested - know it does not exist */
	    break;
	default:
	    /* SCREAM */
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_read() - find_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	/* object does not exist, so create it */

	/* Strong authentication  */
	if ((retval = check_security_parms((caddr_t) arg, 
				_ZAddEntryArgumentDataDAS, 
				&_ZDAS_mod,
				arg->ada_common.ca_security,
				arg->ada_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		return (DS_ERROR_REMOTE);
	}

	DLOG (log_dsap,LLOG_TRACE,("add - find parent"));

	if ((dntop = dn_cpy(target)) != NULLDN)
		for (dn=dntop; dn->dn_parent != NULLDN; dn=dn->dn_parent)
			trail = dn;

	if (trail == NULLDN) {
		dntop = NULLDN;
		entryptr = database_root;
		if (entryptr->e_data != E_DATA_MASTER) {
			error->dse_type = DSE_REFERRAL;
        		error->ERR_REFERRAL.DSE_ref_prefix = NULLDN;
			if ((error->ERR_REFERRAL.DSE_ref_candidates = cont_ref_parent (NULLDN)) == NULLCONTINUATIONREF) {
				error->dse_type = DSE_SERVICEERROR;
				error->ERR_SERVICE.DSE_sv_problem = DSE_SV_INVALIDREFERENCE;
			}
			return (DS_ERROR_CONNECT);
		}
	} else {
		trail->dn_parent = NULLDN;
		switch(find_child_entry(dntop,&(arg->ada_common),binddn,NULLDNSEQ,TRUE,&(entryptr), error, di_p))
		{
		case DS_OK:
		    /* Filled out entryptr - carry on */
		    break;
		case DS_CONTINUE:
		    /* Filled out di_p - what do we do with it ?? */
		    /* When add returns DS_CONTINUE the target must be changed */
		    return(DS_CONTINUE);

		case DS_X500_ERROR:
		    /* Filled out error - what do we do with it ?? */
		    return(DS_X500_ERROR);
		default:
		    /* SCREAM */
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_add() - find_child_entry failed"));
		    return(DS_ERROR_LOCAL);
		}
	}

	if ( read_only || ((entryptr->e_parent != NULLENTRY) && (entryptr->e_parent->e_lock))) {
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		dn_free (dntop);
		dn_free (dn);
		return (DS_ERROR_REMOTE);
	}

	/* not prepared to accept operation over DSP */
	if (dsp) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
		dn_free (dntop);
		dn_free (dn);
		return (DS_ERROR_REMOTE);
	}

	DLOG (log_dsap,LLOG_TRACE,("add - acl"));
	if (check_acl (binddn,ACL_ADD,entryptr->e_acl->ac_child,dntop) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		dn_free (dntop);
		dn_free (dn);
		return (DS_ERROR_REMOTE);
	}

	DLOG (log_dsap,LLOG_TRACE,("add - default"));

	DATABASE_HEAP;

	ptr = get_default_entry (entryptr);
	ptr->e_name = rdn_cpy (dn->dn_rdn);
	ptr->e_attributes = as_cpy (arg->ada_entry);
 
	modify_attr (ptr,binddn);

	DLOG (log_dsap,LLOG_TRACE,("add - unravel"));
	if (unravel_attribute (ptr,error) != OK) {
		dn_free (dntop);
		dn_free (dn);
		entry_free (ptr);
		GENERAL_HEAP;
		return (DS_ERROR_REMOTE);
	}

	if ( ! check_oc_hierarchy(ptr->e_oc)) {
		error->dse_type = DSE_UPDATEERROR;
		error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
		dn_free (dntop);
		dn_free (dn);
		entry_free (ptr);
		GENERAL_HEAP;
		return (DS_ERROR_REMOTE);
	}

	DLOG (log_dsap,LLOG_TRACE,("add - schema"));
	if (check_schema (ptr,NULLATTR,error) != OK) {
		dn_free (dntop);
		dn_free (dn);
		entry_free (ptr);
		GENERAL_HEAP;
		return (DS_ERROR_REMOTE);
	}

	GENERAL_HEAP;

	dn_free (dn);
	dn_free (dntop);

#ifdef TURBO_AVL
	if ( (!ptr->e_leaf) && (!ptr->e_external) && (!ptr->e_children)) {
#else
	if ( (!ptr->e_leaf) && (!ptr->e_external) && (!ptr->e_child)) {
#endif
		AV_Sequence avs;
		for (avs = ptr->e_master; avs != NULLAV; avs=avs->avseq_next) 
			if (dn_cmp ((DN)avs->avseq_av.av_struct, mydsadn) == 0) {
				create_null_edb (ptr);
				break;
			} 
		if (avs == NULLAV)
			ptr->e_allchildrenpresent = FALSE;
	}

#ifdef TURBO_AVL
	/* add the entry */
	DATABASE_HEAP;
	(void) avl_insert(&entryptr->e_children, (caddr_t) ptr, entry_cmp, avl_dup_error);
	GENERAL_HEAP;
#endif
		
	if (entryptr->e_leaf) {

#ifdef COMPAT_6_0

		/* Turn leaf into non leaf, and add child */
		/* Temporary until managemnet tools do it */

		Attr_Sequence newas;
		
		if (entryptr->e_data != E_DATA_MASTER) {
			DN dn_found;
			struct dn_seq	* dn_stack = NULLDNSEQ;
			int res;

			dn_found = get_copy_dn (entryptr);
			res = constructor_dsa_info(dn_found,dn_stack,TRUE,entryptr,error,di_p);
			dn_free (dn_found);
			entry_free (ptr);
			switch (res) {
			case DS_CONTINUE:
				return(DS_CONTINUE);
			case DS_X500_ERROR:
				return(DS_CONTINUE);
			default:
			    	return(DS_ERROR_LOCAL);
			}
			/* NOTREACHED */
		}

#ifndef TURBO_AVL
		entryptr->e_child = ptr;
#endif
		/* add master and slave attributes */

		DATABASE_HEAP;

		if ((entryptr->e_parent->e_slave == NULLAV) && (entryptr->e_parent->e_master == NULLAV)) {
			extern char * mydsaname;
			entryptr->e_master = str2avs (mydsaname,at_masterdsa);
			newas = as_comp_new (AttrT_cpy(at_masterdsa),entryptr->e_master,NULLACL_INFO);
			entryptr->e_attributes = as_merge (entryptr->e_attributes,newas);
		} else {
			if ((entryptr->e_master = avs_cpy(entryptr->e_parent->e_master)) != NULLAV) {
				newas = as_comp_new (AttrT_cpy(at_masterdsa),entryptr->e_master,NULLACL_INFO);
				entryptr->e_attributes = as_merge (entryptr->e_attributes,newas);
			}
			if ((entryptr->e_slave = avs_cpy (entryptr->e_parent->e_slave)) != NULLAV) {
				newas = as_comp_new (AttrT_cpy(at_slavedsa),entryptr->e_slave,NULLACL_INFO);
				entryptr->e_attributes = as_merge (entryptr->e_attributes,newas);
			}
		}
		/* add new QuipuNonLeaf objectclass */

		/* see if OC inherited */
		if (as_find_type(entryptr->e_attributes,at_objectclass) == NULLATTR) {
			/* OC inherited - pull down */
			newas = as_comp_new (AttrT_cpy(at_objectclass),avs_cpy(entryptr->e_oc),NULLACL_INFO);
			entryptr->e_attributes = as_merge (entryptr->e_attributes,newas);
		}

		newas = as_comp_new (AttrT_cpy(at_objectclass),str2avs(NONLEAFOBJECT,at_objectclass),NULLACL_INFO);
		entryptr->e_attributes = as_merge (entryptr->e_attributes,newas);

		if (entryptr->e_parent != NULLENTRY) {
			if (entryptr->e_parent->e_edbversion)
				free (entryptr->e_parent->e_edbversion);
			entryptr->e_parent->e_edbversion = new_version();
		}
		if (entryptr->e_edbversion)
			free (entryptr->e_edbversion);
		entryptr->e_edbversion = new_version();
		ptr->e_edbversion = new_version();
		entryptr->e_allchildrenpresent = 2;	/* Subtree ! */

		modify_attr (entryptr,binddn);
		if (unravel_attribute (entryptr,error) != OK) 
			fatal (-31,"serious schema error");

#ifdef TURBO_INDEX
		turbo_add2index(ptr);		/* add new entry to index */
		turbo_add2index(entryptr);	/* add parent to index */
#endif

#ifdef TURBO_DISK
		/* write the new entry */
		if (turbo_write(ptr) == NOTOK)
			fatal(-32,"add turbo_write (2) failure - check database");
#else
		if (journal (ptr) == NOTOK)
			fatal (-32,"add journal (2) failure - check database");
#endif

		entryptr->e_leaf = FALSE;

#ifdef TURBO_DISK
		/* rewrite the parent as well */
		if (turbo_write(entryptr) == NOTOK)
			fatal(-31,"add parent turbo_write failed - check database");
#else
		if (journal (entryptr) != OK)
			fatal (-31,"add parent journal failed - check database");
#endif

		GENERAL_HEAP;

		local_master_size++;
		return (OK);

#else	/* COMPAT_6_0 */

		/* Management tools will take care of the situation properly */

		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		return(DS_ERROR_REMOTE) ;

#endif	/* COMPAT_6_0 */

#ifdef TURBO_AVL
	}
#else
	} else {
		ptr->e_sibling = entryptr->e_child;
		entryptr->e_child = ptr;
	}
#endif

	if (ptr->e_parent != NULLENTRY) {
		if (ptr->e_parent->e_edbversion)
			free (ptr->e_parent->e_edbversion);
		ptr->e_parent->e_edbversion = new_version();
	}

#ifdef TURBO_INDEX
	turbo_add2index(ptr);
#endif

#ifdef TURBO_DISK
	if (turbo_write(ptr) == NOTOK)
		fatal(-32,"add turbo_write failure - check database");
#else
	if (journal (ptr) == NOTOK)
		fatal (-32,"add journal failure - check database");
#endif

	local_master_size++;
	return (DS_OK);
}
