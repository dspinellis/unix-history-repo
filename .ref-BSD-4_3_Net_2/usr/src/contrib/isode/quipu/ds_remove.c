/* ds_remove.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_remove.c,v 7.2 91/02/22 09:38:57 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_remove.c,v 7.2 91/02/22 09:38:57 mrose Interim $
 *
 *
 * $Log:	ds_remove.c,v $
 * Revision 7.2  91/02/22  09:38:57  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:52  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:15  mrose
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
#include "quipu/remove.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern LLog * log_dsap;
extern int local_master_size;
#ifdef TURBO_AVL
extern int entry_cmp();
#endif

do_ds_removeentry (arg, error, binddn, target, di_p, dsp)
    struct ds_removeentry_arg      *arg;
    struct DSError              *error;
    DN                          binddn;
    DN				target;
    struct di_block		**di_p;
    char 			dsp;
{
Entry  entryptr, delent = NULLENTRY;
char * new_version ();
int retval;
extern int read_only;

	DLOG (log_dsap,LLOG_TRACE,("ds remove entry"));

	if (!dsp)
		target = arg->rma_object;

	/* stop aliases being dereferenced */
	arg->rma_common.ca_servicecontrol.svc_options |= SVC_OPT_DONTDEREFERENCEALIAS;

	if (target == NULLDN) {
		error->dse_type = DSE_NAMEERROR;
		error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		error->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_ERROR_REMOTE);
	}

	switch(find_entry(target,&(arg->rma_common),binddn,NULLDNSEQ,TRUE,&(entryptr), error, di_p, OP_REMOVEENTRY))
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
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_remove() - find_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	/* entry found, so remove it  */

	/* Strong authentication  */
	if ((retval = check_security_parms((caddr_t) arg,
			_ZRemoveEntryArgumentDataDAS,
			&_ZDAS_mod,
			arg->rma_common.ca_security,
			arg->rma_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		return (DS_ERROR_REMOTE);
	}

	if (read_only || entryptr->e_parent->e_lock) {
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

	if ( ! (isleaf(entryptr))) {
		error->dse_type = DSE_UPDATEERROR;
		error->ERR_UPDATE.DSE_up_problem = DSE_UP_NOTONNONLEAF;
		return (DS_ERROR_REMOTE);
	}

	if ( ((entryptr->e_parent->e_data == E_TYPE_CONSTRUCTOR) && (check_acl (binddn,ACL_WRITE,entryptr->e_acl->ac_entry, target) == NOTOK))
	     || (check_acl (binddn, ACL_WRITE, entryptr->e_parent->e_acl->ac_child, target) == NOTOK)) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_ERROR_REMOTE);
	}


#ifdef TURBO_AVL
	if (avl_onenode(entryptr->e_parent->e_children)) {
#else
	if (entryptr->e_sibling == NULLENTRY) {
#endif
#ifdef NOTANYMORE

/*
   We have been asked to remove the last node at one level,
   This means the parent has become a leaf.  The parent however may be
   held in another DSA - which will need updating.
   Also the edbinfo attribute will need modifing.
   Leave till later...
*/
		if (entryptr->e_parent->e_data != E_DATA_MASTER) {
			error->dse_type = DSE_UPDATEERROR;
			error->ERR_UPDATE.DSE_up_problem = DSE_UP_AFFECTSMULTIPLEDSAS;
		} else {
			error->dse_type = DSE_SERVICEERROR;
			error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		}
		return (DS_ERROR_REMOTE);
#endif

#ifdef TURBO_AVL
#ifdef TURBO_INDEX
		/* delete index references to this node */
		turbo_index_delete(entryptr);
#endif
		/* removed node in core */
		(void) avl_delete(&entryptr->e_parent->e_children,(caddr_t) entryptr,entry_cmp);
#else
		entryptr->e_parent->e_child = NULLENTRY ;
#endif /* TURBO_AVL */
		entryptr->e_parent->e_allchildrenpresent = 2 ;

		if (entryptr->e_parent->e_edbversion)
			free (entryptr->e_parent->e_edbversion);
		entryptr->e_parent->e_edbversion = new_version();

#ifdef TURBO_DISK
		/* remove node on disk */
		if (turbo_delete(entryptr) != OK)
			fatal(-35, "turbo delete failed - check database");
#else
		{
		DN      save_dn ;
		char   *filename ;
		Entry   empty_entry = get_default_entry (NULLENTRY) ;
		char   *dn2edbfile() ;

		empty_entry->e_data = E_DATA_MASTER ;

		save_dn = get_copy_dn(entryptr->e_parent) ;

		if ((filename = dn2edbfile(save_dn)) == NULLCP)
		{
			fatal(-33, "SPT: ds_modify: 1 creating new NLN out failed.\n") ;
		}
		if (write_edb(empty_entry, filename) != OK)
		{
			(void) unlink (filename);
			fatal(-33, "SPT: ds_modify: 2 writing new NLN out failed.\n") ;
		}

		dn_free (save_dn);
		}
#endif /* TURBO_DISK */
		entry_free (entryptr);
		local_master_size--;
		return (DS_OK);

	}
	else
	{
		/* removed node node in core */
#ifdef TURBO_AVL
#ifdef TURBO_INDEX
		/* delete index references to this node */
		turbo_index_delete(entryptr);
#endif
		(void) avl_delete(&entryptr->e_parent->e_children,(caddr_t) entryptr,entry_cmp);
#else
		entryptr->e_parent->e_child = entryptr->e_sibling;
#ifndef  TURBO_DISK
		delent = entryptr;
		entryptr = entryptr->e_sibling;
#endif
#endif
	}

	if (entryptr->e_parent->e_edbversion)
		free (entryptr->e_parent->e_edbversion);
	entryptr->e_parent->e_edbversion = new_version();

	/* remove node on disk */
#ifdef TURBO_DISK
	if (turbo_delete(entryptr) != OK)
		fatal (-35,"turbo remove failed - check database");
#else
	if (journal (entryptr) != OK) 
		fatal (-35,"remove rewrite fail - check database");
#endif

	if (delent)
		entry_free (delent);
	else
		entry_free (entryptr);
	local_master_size--;
	return (DS_OK);

}
