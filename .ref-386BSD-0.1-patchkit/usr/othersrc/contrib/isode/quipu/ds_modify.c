/* ds_modify.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_modify.c,v 7.5 91/03/09 11:56:44 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_modify.c,v 7.5 91/03/09 11:56:44 mrose Exp $
 *
 *
 * $Log:	ds_modify.c,v $
 * Revision 7.5  91/03/09  11:56:44  mrose
 * update
 * 
 * Revision 7.4  91/02/22  09:38:51  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:53:46  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:45:45  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:20:16  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:10  mrose
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
#include "quipu/modify.h"
#include "quipu/malloc.h"
#include "config.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

static check_remove_values ();
static check_remove_type ();
extern Entry database_root;
extern LLog * log_dsap;
extern Attr_Sequence entry_find_type();
extern DN mydsadn;

extern AttributeType at_control;
extern AttributeType at_acl;
extern AttributeType at_objectclass;

#ifdef TURBO_AVL
extern int inherit_link();
#endif

Entry  nulledb;

static AttributeValue nonleafav = NULLAttrV;

struct	acl *acl_list;

int updateerror;

do_ds_modifyentry (arg, error, binddn, target, di_p, dsp)
    struct ds_modifyentry_arg   *arg;
    struct DSError              *error;
    DN                          binddn;
    DN                          target;
    struct di_block		**di_p;
    char		        dsp;
{
Entry  entryptr;
Entry  real_entry;
struct entrymod *eptr;
Entry  entry_cpy ();
int    remove = NOTOK;
int    retval;
extern int read_only;
char * new_version ();
Attr_Sequence as;

	DLOG (log_dsap,LLOG_TRACE,("ds_modifyentry"));

	if (!dsp)
		target = arg->mea_object;

	/* stop aliases being dereferenced */
	arg->mea_common.ca_servicecontrol.svc_options |= SVC_OPT_DONTDEREFERENCEALIAS;

	/* check for control sequence */
	if (!dsp && arg->mea_changes
		&& (arg->mea_changes->em_type == EM_ADDATTRIBUTE)) {
		if ( AttrT_cmp (arg->mea_changes->em_what->attr_type,at_control) == 0) {
			int res;
			res = dsa_control (arg->mea_changes->em_what,error,binddn);
			return (res);
		}
	}

	if (target == NULLDN) {
		error->dse_type = DSE_NAMEERROR;
		error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		error->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_ERROR_REMOTE);
	}

	switch(find_entry(target,&(arg->mea_common),binddn,NULLDNSEQ,TRUE,&(real_entry), error, di_p, OP_MODIFYENTRY))
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
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_modify() - find_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	/* Strong authentication  */
	if ((retval = check_security_parms((caddr_t) arg,
			_ZModifyEntryArgumentDataDAS,
			&_ZDAS_mod,
			arg->mea_common.ca_security,
			arg->mea_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		return (DS_ERROR_REMOTE);
	}

	if (read_only || real_entry->e_parent->e_lock) {
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNAVAILABLE;
		return (DS_ERROR_REMOTE);
	}

	/* not prepared to accept operation over DSP */
	if (dsp) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
		return (DS_ERROR_REMOTE);
	}

	DATABASE_HEAP;
	entryptr = entry_cpy (real_entry);
	acl_list = real_entry->e_acl;
	GENERAL_HEAP;

	if (check_acl (binddn, ACL_ADD, acl_list->ac_entry,target) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		entry_free (entryptr);
		return (DS_ERROR_REMOTE);
	}

	if (check_acl (binddn, ACL_WRITE, acl_list->ac_entry,target) == OK)
		remove = OK;

	nulledb = NULLENTRY;

	for (eptr = arg->mea_changes; eptr!=NULLMOD; eptr=eptr->em_next) {
		switch (eptr->em_type) {
		   case EM_ADDVALUES:
			if (mod_add_value (entryptr,eptr->em_what,error,binddn,target,real_entry) != OK) {
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			break;
		   case EM_ADDATTRIBUTE:
			if (add_attribute (entryptr,eptr->em_what,error,binddn,target) != OK) {
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			break;
		   case EM_REMOVEATTRIBUTE:
			/* must not do this if attribute is rdn */
			if (check_remove_type (entryptr->e_name,eptr->em_what->attr_type) == NOTOK) {
				error->dse_type = DSE_UPDATEERROR;
				error->ERR_UPDATE.DSE_up_problem = updateerror;
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			if (remove == OK) {
			   if (remove_attribute (entryptr,eptr->em_what->attr_type,error,binddn,target,real_entry) != OK) {
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			   }
			} else {
				error->dse_type = DSE_SECURITYERROR;
				error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			break;
		   case EM_REMOVEVALUES:
			if (check_remove_values (entryptr->e_name, eptr->em_what) == NOTOK) {
				error->dse_type = DSE_UPDATEERROR;
				error->ERR_UPDATE.DSE_up_problem = updateerror;
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			if (remove == OK) {
			   if (remove_value (entryptr,eptr->em_what,error,binddn,target,real_entry) != OK) {
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			   }
			} else {
				error->dse_type = DSE_SECURITYERROR;
				error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
				entry_free (entryptr);
				return (DS_ERROR_REMOTE);
			}
			break;
		}
	}

	/* check the last value of an attribute has not been removed */
	for (as = entryptr->e_attributes; as!=NULLATTR; as=as->attr_link)
		if (as->attr_value == NULLAV) {
			error->dse_type = DSE_ATTRIBUTEERROR;
			error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (entryptr);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_CONSTRAINTVIOLATION;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (as->attr_type);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			entry_free (entryptr);
			return (DS_ERROR_REMOTE);
		}
	
	DATABASE_HEAP;
	modify_attr (entryptr,binddn);
	if (unravel_attribute (entryptr,error) != OK) {
		GENERAL_HEAP;
		entry_free (entryptr);
		return (DS_ERROR_REMOTE);
	} else 	if ( ! check_oc_hierarchy(entryptr->e_oc)) {
		error->dse_type = DSE_UPDATEERROR;
		error->ERR_UPDATE.DSE_up_problem = DSE_UP_OBJECTCLASSVIOLATION;
		GENERAL_HEAP;
		entry_free (entryptr);
		return (DS_ERROR_REMOTE);
	} else if (check_schema (entryptr,NULLATTR,error) == OK) {
		GENERAL_HEAP;

		/* Check user has not prevented further modification by themselves ! */
		if ((acl_list != entryptr->e_acl)
			&& (acl_cmp (acl_list,entryptr->e_acl) != 0)) { 
			if ((as = entry_find_type (entryptr,at_acl)) == NULLATTR) {	
				LLOG(log_dsap,LLOG_NOTICE,("Attempt to remove ACL"));
				error->dse_type = DSE_SERVICEERROR;
				error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
				return (DS_ERROR_REMOTE);
				
			}
			if ((check_acl (binddn, ACL_WRITE, as->attr_acl,target) == NOTOK) ||
				(check_acl (binddn, ACL_WRITE, entryptr->e_acl->ac_entry,target) == NOTOK)) {
				entry_free (entryptr);
				LLOG(log_dsap,LLOG_NOTICE,("Not modifying due to future access problem"));
				error->dse_type = DSE_SERVICEERROR;
				error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
				return (DS_ERROR_REMOTE);
			}
		}


		if (nulledb) {
			AV_Sequence avs;

		        for (avs = entryptr->e_master; avs != NULLAV; avs=avs->avseq_next) {
		                if (avs->avseq_av.av_struct == NULL)
		                        continue;

				if (dn_cmp ((DN)avs->avseq_av.av_struct,mydsadn) == 0) {
					create_null_edb (nulledb);
					break;
				}
			}

			if (avs == NULLAV)
				entryptr->e_allchildrenpresent = FALSE;
		}

		write_dsa_entry (entryptr);

#ifdef TURBO_AVL
                /*
                 * changes made OK, so add new entry into tree.
                 * instead of inserting entryptr (the ptr to the
                 * modified version of the entry), we free the
                 * old contents of the old entry and copy in the
                 * new data.  this saves us from having to update
                 * all parent pointers in child nodes.
                 */

#ifdef TURBO_INDEX
                /* delete old entry from index first */
                turbo_index_delete(real_entry);
#endif

                if (entryptr->e_parent == NULLENTRY) {
                        entry_replace(database_root, entryptr);
                } else {
                        entry_replace(real_entry, entryptr);
                }
		entry_free (entryptr);
		entryptr = real_entry;

                if (unravel_attribute(real_entry, error) != OK)
                        return(DS_ERROR_REMOTE);

		if (real_entry->e_parent != NULLENTRY) {
			if (real_entry->e_parent->e_edbversion)
				free (real_entry->e_parent->e_edbversion);
			real_entry->e_parent->e_edbversion = new_version();
		}

                (void) avl_apply(real_entry->e_children, inherit_link, 
		    (caddr_t) real_entry, NOTOK, AVL_PREORDER);

#ifdef TURBO_INDEX
		/* add the new modified entry to the index */
		turbo_add2index(real_entry);
#endif

#else
		/* changes made OK, so add new entry into tree */
		if (entryptr->e_parent == NULLENTRY) {
			database_root = entryptr;
			entry_free (real_entry);
			if (entryptr->e_data == E_TYPE_SLAVE) 
				/* Our copy of DSA entry - don't rewrite EDB */
				return DS_OK;
		} else {
			entryptr->e_parent->e_child = entryptr;
			entry_free (real_entry);
			/* now alter all parent pointers */
			for (real_entry = entryptr->e_child; real_entry!=NULLENTRY; real_entry=real_entry->e_sibling) {
				real_entry->e_parent = entryptr;
				set_inheritance (real_entry);
			}

			if (entryptr->e_data == E_TYPE_SLAVE) 
				/* Our copy of DSA entry - don't rewrite EDB */
				return DS_OK;

			if (entryptr->e_parent->e_edbversion)
				free (entryptr->e_parent->e_edbversion);
			entryptr->e_parent->e_edbversion = new_version();
		}
#endif /* TURBO_AVL */

#ifdef TURBO_DISK
		if (turbo_write(entryptr) != OK)
			fatal (-33,"modify rewrite failed - check database");
#else
		if (journal (entryptr) != OK)
			fatal (-33,"modify rewrite failed - check database");
#endif

		return (DS_OK);
	} else {
		entry_free (entryptr);
		return (DS_ERROR_REMOTE);
	}
}

remove_attribute (eptr,at,error,requestor,dn,real_entry)
Entry eptr,real_entry;
AttributeType at;
struct DSError *error;
DN requestor,dn;
{
register Attr_Sequence as, trail= NULLATTR, real_as;

	DLOG (log_dsap,LLOG_DEBUG,("remove attribute"));

	for  (as=eptr->e_attributes; as!=NULLATTR; as=as->attr_link) {
		if ((AttrT_cmp (as->attr_type,at)) == 0)
			break;
		trail = as;
	}
	if ((as == NULLATTR) || 
	   ((real_as = as_find_type (real_entry->e_attributes,at)) == NULLATTR)) {
		if (entry_find_type(real_entry,at) == NULLATTR) {
			error->dse_type = DSE_ATTRIBUTEERROR;
	 		error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (at);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			return (DS_ERROR_REMOTE);
		} else {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't remove inherited attribute"));
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
			return (DS_ERROR_REMOTE);
		}
	   }

	if (check_acl(requestor,ACL_WRITE,real_as->attr_acl,dn) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_ERROR_REMOTE);
	}

	if (trail == NULLATTR) {
		/* first in sequence */
		eptr->e_attributes = as->attr_link;
		as_comp_free (as);
	} else
		as_delnext (trail);

	return (OK);
}


static check_remove_type (rdn,at)
register RDN rdn;
register AttributeType at;
{

	if ( AttrT_cmp (at,at_objectclass) == 0) {
		updateerror = DSE_UP_NOOBJECTCLASSMODS;
		return (NOTOK);
        }

	/* check attribute type is not distinguished */

	for (; rdn!=NULLRDN; rdn=rdn->rdn_next)
		if (AttrT_cmp (rdn->rdn_at,at) == 0) {
			updateerror = DSE_UP_NOTONRDN;
			return (NOTOK);
		}

	return (OK);
}

static check_remove_values (rdn,as)
register RDN rdn;
register Attr_Sequence as;
{
register AV_Sequence as_avs;

	/* check that the value trying to remove is not distinguished */
	for (; rdn!=NULLRDN; rdn=rdn->rdn_next)
		if (AttrT_cmp (rdn->rdn_at,as->attr_type) == 0)
			for (as_avs=as->attr_value; as_avs!=NULLAV; as_avs=as_avs->avseq_next)
				if (AttrV_cmp (&rdn->rdn_av,&as_avs->avseq_av) == 0) {
					updateerror = DSE_UP_NOTONRDN;
					return (NOTOK);
				}
	return (OK);
}



remove_value (eptr,rmas,error,requestor,dn,real_entry)
Entry eptr, real_entry;
Attr_Sequence rmas;
struct DSError *error;
DN requestor,dn;
{
register Attr_Sequence as,real_as;
register AV_Sequence rmavs,avs,trail = NULLAV;
int i;

	DLOG (log_dsap,LLOG_DEBUG,("remove attribute value"));

	if (((as = as_find_type (eptr->e_attributes,rmas->attr_type)) == NULLATTR) 
	   || (((real_as = as_find_type (real_entry->e_attributes,rmas->attr_type)) == NULLATTR))) {

		if (((as = entry_find_type (eptr,rmas->attr_type)) == NULLATTR) 
		   || (((real_as = entry_find_type (real_entry,rmas->attr_type)) == NULLATTR))) {
			error->dse_type = DSE_ATTRIBUTEERROR;
			error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (rmas->attr_type);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			return (DS_ERROR_REMOTE);
		   } else {
			/* inherited - ignore removal if allowed */
			if (check_acl(requestor,ACL_WRITE,real_as->attr_acl,dn) == NOTOK) {
				LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't remove inherited value"));
				error->dse_type = DSE_SECURITYERROR;
				error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
				return (NOTOK);
			}
			return OK;
	   	   }
	   }

	if (check_acl(requestor,ACL_WRITE,real_as->attr_acl,dn) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (NOTOK);
	}

	for (rmavs=rmas->attr_value; rmavs != NULLAV; rmavs = rmavs->avseq_next) {

	   for (avs=as->attr_value; avs!=NULLAV; avs=avs->avseq_next) {
		if ((i = AttrV_cmp(&avs->avseq_av,&rmavs->avseq_av)) == 0)
			break;
		if (i == -2) {
			error->dse_type = DSE_ATTRIBUTEERROR;
			error->ERR_ATTRIBUTE.DSE_at_name = NULLDN;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_INAPPROPRIATEMATCHING;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (as->attr_type);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = AttrV_cpy(&rmas->attr_value->avseq_av);
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			return (DS_ERROR_REMOTE);
		}
		trail = avs;
	   }

	   if (avs == NULLAV) {
		error->dse_type = DSE_ATTRIBUTEERROR;
		error->ERR_ATTRIBUTE.DSE_at_name = get_copy_dn (eptr);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what =DSE_AT_NOSUCHATTRIBUTE;
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (rmas->attr_type);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = AttrV_cpy (&rmas->attr_value->avseq_av);
		error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
		return (DS_ERROR_REMOTE);
	   }

	   /* SPT addition for empty non-leaves */
	   /* Removing the nonLeafObject objectclass makes it a leaf again,
	    * if this is allowed. */

	   if (nonleafav == NULLAttrV)
		nonleafav = str2AttrV(NONLEAFOBJECT, str2syntax("objectClass"));

	   if (AttrT_cmp(as->attr_type, at_objectclass) == 0 &&
	       AttrV_cmp(&avs->avseq_av, nonleafav ) == 0)
	   {
#ifdef TURBO_AVL
		Entry chld;
		   
		chld = (Entry) avl_getone(eptr->e_children);
		if (!eptr->e_leaf && chld && chld->e_data == E_DATA_MASTER) 
#else
		if ((!eptr->e_leaf && eptr->e_child && 
		        (eptr->e_child->e_data == E_DATA_MASTER))
#endif
		{
			LLOG (log_dsap, LLOG_EXCEPTIONS,("Can't remove non leaf object class - children below"));
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_NOTONNONLEAF;
			return(DS_ERROR_REMOTE) ;
		}

		/*After all that, we should be able to remove stuff... */
		eptr->e_leaf = TRUE ;	/* Make it a leaf */
					/* Let it continue, & remove attrV */

		/* Also have to remove the EDB File that was generated there */
	   }

	   if (trail == NULLAV) {
		/* first in sequence */
		as->attr_value = avs->avseq_next;
		avs_comp_free (avs);
	   } else
		avs_delnext (trail);
	}

	return (OK);
}

add_attribute (eptr,newas,error,requestor,dn)
Entry eptr;
Attr_Sequence newas;
struct DSError *error;
DN requestor,dn;
{
struct acl_attr * aa;
struct acl_info * ai = NULLACL_INFO;
struct oid_seq * oidptr;

	DLOG (log_dsap,LLOG_DEBUG,("add attribute"));

	if (entry_find_type (eptr,newas->attr_type) != NULLATTR) {
		error->dse_type = DSE_ATTRIBUTEERROR;
		error->ERR_ATTRIBUTE.DSE_at_name = dn_cpy (dn);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_TYPEORVALUEEXISTS;
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (newas->attr_type);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
		error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
		DLOG (log_dsap,LLOG_DEBUG,("add exists error"));
		return (NOTOK);
	}

	for ( aa = acl_list->ac_attributes; aa!=NULLACL_ATTR; aa=aa->aa_next) {
		for ( oidptr=aa->aa_types;oidptr != NULLOIDSEQ; oidptr=oidptr->oid_next) {
			if (oid_cmp (oidptr->oid_oid,grab_oid(newas->attr_type)) == 0) {
				ai = aa->aa_acl;
				break;
			}
		}
		if (ai != NULLACL_INFO)
			break;
	}
	if (ai == NULLACL_INFO)
		ai = acl_list->ac_default;

	if (check_acl(requestor,ACL_WRITE,ai,dn) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		DLOG (log_dsap,LLOG_DEBUG,("add acl failed"));
		return (NOTOK);
	}

	DATABASE_HEAP;
	eptr->e_attributes = as_merge (as_cpy(newas),eptr->e_attributes);
	GENERAL_HEAP;

	return (OK);
}


mod_add_value (eptr,newas,error,requestor,dn,real_entry)
Entry eptr,real_entry;
Attr_Sequence newas;
struct DSError *error;
DN requestor,dn;
{
register Attr_Sequence as;
AV_Sequence avs;
char * dn2edbfile();

	DLOG (log_dsap,LLOG_DEBUG,("add value"));

	if ( (as = entry_find_type (real_entry,newas->attr_type)) == NULLATTR) {
		error->dse_type = DSE_ATTRIBUTEERROR;
		error->ERR_ATTRIBUTE.DSE_at_name = dn_cpy (dn);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_NOSUCHATTRIBUTE;
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (newas->attr_type);
		error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = NULLAttrV;
		error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
		return (NOTOK);
	}

	if (check_acl(requestor,ACL_WRITE,as->attr_acl,dn) == NOTOK) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		DLOG (log_dsap,LLOG_DEBUG,("add acl failed"));
		return (NOTOK);
	}

	if ( as_find_type (real_entry->e_attributes,newas->attr_type) == NULLATTR) {
		/* its inherited - add into the entry itself */
		DATABASE_HEAP;
		eptr->e_attributes = as_merge (as_cpy(newas),eptr->e_attributes);
		GENERAL_HEAP;
		return (OK);
	}

	for (avs=as->attr_value; avs != NULLAV; avs=avs->avseq_next)
		if (AttrV_cmp(&avs->avseq_av,&newas->attr_value->avseq_av) == 0) {
			error->dse_type = DSE_ATTRIBUTEERROR;
			error->ERR_ATTRIBUTE.DSE_at_name = dn_cpy (dn);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_what = DSE_AT_TYPEORVALUEEXISTS;
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_type = AttrT_cpy (newas->attr_type);
			error->ERR_ATTRIBUTE.DSE_at_plist.DSE_at_value = AttrV_cpy (&newas->attr_value->avseq_av);
			error->ERR_ATTRIBUTE.DSE_at_plist.dse_at_next = DSE_AT_NOPROBLEM;
			DLOG (log_dsap,LLOG_DEBUG,("add value exists error"));
			return (NOTOK);
		}

	   if (nonleafav == NULLAttrV)
		nonleafav = str2AttrV(NONLEAFOBJECT, str2syntax("objectClass"));
		
	if ((AttrT_cmp(as->attr_type, at_objectclass) == 0) &&
	    AttrV_cmp(&newas->attr_value->avseq_av, nonleafav) == 0)
	{
		nulledb = eptr;
		/* will sort out the EDB file later if needed */
	}

	DATABASE_HEAP;
	eptr->e_attributes = as_merge (as_cpy(newas),eptr->e_attributes);
	GENERAL_HEAP;

	return (OK);
}


create_null_edb (eptr)
Entry eptr;
{
DN	save_dn ;
char   *filename, *dn2edbfile();
Entry	empty_entry;
#ifdef TURBO_DISK
char	gfname[1024];
#endif

	eptr->e_leaf = FALSE ;
#ifdef TURBO_AVL
	eptr->e_children = NULLAVL ;
#else
	eptr->e_child = NULLENTRY ;
#endif
	eptr->e_allchildrenpresent = 2 ;

	empty_entry = get_default_entry (eptr);	
	empty_entry->e_data = E_DATA_MASTER ;

	if (eptr->e_parent->e_edbversion)
		free (eptr->e_parent->e_edbversion);
	eptr->e_parent->e_edbversion = new_version();

	save_dn = get_copy_dn(eptr) ;

	if ((filename = dn2edbfile(save_dn)) == NULLCP)
	{
		fatal(-33, "SPT: ds_modify: 1 creating new NLN out failed.\n");
	}
#ifdef TURBO_DISK
	strcpy(gfname, filename);
	strcat(gfname, ".gdbm");
	if (turbo_writeall(eptr) != OK) {
		fatal(-33, "create_null_edb: turbo_writeall failed.\n");
	}
#else
	if (write_edb(empty_entry, filename) != OK)
	{
		(void) unlink (filename);
		fatal(-33, "SPT: ds_modify: 2 writing new NLN out failed.\n") ;
	}
#endif

	dn_free (save_dn);
	free ((char *)empty_entry);
}
