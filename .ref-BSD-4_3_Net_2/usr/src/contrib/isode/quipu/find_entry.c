/* find_entry.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/find_entry.c,v 7.4 91/03/09 11:56:56 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/find_entry.c,v 7.4 91/03/09 11:56:56 mrose Exp $
 *
 *
 * $Log:	find_entry.c,v $
 * Revision 7.4  91/03/09  11:56:56  mrose
 * update
 * 
 * Revision 7.3  91/02/22  09:39:22  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:54:19  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:46:12  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:40  mrose
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
#include "quipu/commonarg.h"
#include "quipu/entry.h"
#include "quipu/ds_error.h"
#include "quipu/connection.h"
#ifdef TURBO_AVL
#include "quipu/turbo.h"
#endif

extern Entry database_root;
extern LLog * log_dsap;
extern time_t timenow;
extern time_t cache_timeout;
extern DN  mydsadn;
extern struct di_block * di_alloc();
extern int dn_print ();

int	  find_entry (object,ca,acl_who,dn_stack,master,ent_p,err,di_p,optype)
DN		  object;
common_args	* ca;
DN		  acl_who;
struct dn_seq	* dn_stack;
int		  master;
Entry		* ent_p;
struct DSError	* err;
struct di_block	**di_p;
int optype;
{
int deref = FALSE;
extern time_t cache_timeout;
DN dn_found;
int res;

	DLOG (log_dsap,LLOG_TRACE,("find_entry"));
	err->dse_type = DSE_NOERROR;

	if ((ca->ca_servicecontrol.svc_options & SVC_OPT_DONTDEREFERENCEALIAS) == 0) 
		deref = TRUE;

	if ((ca->ca_servicecontrol.svc_options & SVC_OPT_DONTUSECOPY) != 0)
		master = TRUE;

	switch(really_find_entry(object,deref,dn_stack,master,ent_p,err,di_p))
	{
	case DS_OK:
	    DLOG(log_dsap, LLOG_DEBUG, ("find_entry - rfe: OK"));
	    /* Have set up ent_p continue processing */
	    break;

	case DS_CONTINUE:
	    DLOG(log_dsap, LLOG_DEBUG, ("find_entry - rfe: CONT"));
#ifdef DEBUG
	    di_list_log((*di_p));
#endif
	    /* Have set up di_blocks of DSAs to be questioned */
	    return(DS_CONTINUE);

	case DS_X500_ERROR:
	    DLOG(log_dsap, LLOG_DEBUG, ("find_entry - rfe: X500_ERROR"));
	    /* Have set up an error */
	    return(DS_X500_ERROR);

	default:
	    /* Scream */
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("really_find_entry failed in find_entry 1"));
	    return(DS_ERROR_LOCAL);
	}

	dn_found = get_copy_dn (*ent_p);

	/* if the returned entry is a CONSTRUCTOR, return a referral */
	if ((*ent_p)->e_data == E_TYPE_CONSTRUCTOR) {
	        DLOG(log_dsap, LLOG_DEBUG, ("find_entry - constructor"));
		res = constructor_dsa_info(dn_found,dn_stack,FALSE,(*ent_p),err,di_p);
		dn_free (dn_found);
		return (res);
	}

	/* if the returned entry is a COPY, - check service controls */
	if (((*ent_p)->e_data != E_DATA_MASTER) && (master)) {

		/* DSAs are special for read/modify */
		if ((optype == OP_READ) || 
		    (optype == OP_MODIFYRDN) ||
		    (optype == OP_MODIFYENTRY)) {
			if ((quipu_ctx_supported (*ent_p) == 2) && (quipu_version_7 (*ent_p))) {
				if (dn_cmp (dn_found, mydsadn) == 0) 
					goto out;
mk_ref:;
				(*di_p) = di_alloc();
				(*di_p)->di_type = DI_TASK;
				(*di_p)->di_dn = dn_found;
				(*di_p)->di_target = dn_cpy(dn_found);
				(*di_p)->di_reftype = RT_UNDEFINED;
				(*di_p)->di_rdn_resolved = CR_RDNRESOLVED_NOTDEFINED;
				(*di_p)->di_aliasedRDNs = CR_NOALIASEDRDNS;
				(*di_p)->di_entry = *ent_p;
				(*ent_p)->e_refcount++;
				(*di_p)->di_state = DI_COMPLETE;
				return DS_CONTINUE;
			}
		}

	        DLOG(log_dsap, LLOG_DEBUG, ("find_entry - slave master needed"));
		res = constructor_dsa_info(dn_found,dn_stack,TRUE,(*ent_p),err,di_p);
		dn_free (dn_found);
		return (res);

	} else if ( ((optype == OP_MODIFYRDN) || (optype == OP_MODIFYENTRY)) &&
		  (quipu_ctx_supported (*ent_p) == 2) && 
		   quipu_version_7 (*ent_p) &&
		  (dn_cmp (dn_found, mydsadn) != 0))
			goto mk_ref;

#ifdef WRONG_BEHAVIOUR

	/* if this is right, we need to make sure that dsa_info */
	/* pick ups the correct external reference */

	if ((*ent_p)->e_external && 
	    ((*ent_p)->e_reftype != RT_NONSPECIFICSUBORDINATE)) {
		res = constructor_dsa_info(dn_found,dn_stack,TRUE,(*ent_p),err,di_p);
		dn_free (dn_found);
		return (res);
	}
#endif

	if (((*ent_p)->e_data == E_TYPE_CACHE_FROM_MASTER) && 
	        (timenow - (*ent_p)->e_age > cache_timeout)) {
	        DLOG(log_dsap, LLOG_DEBUG, ("find_entry - cache timed out"));
		res = constructor_dsa_info(dn_found,dn_stack,TRUE,(*ent_p),err,di_p);
		delete_cache (dn_found);
		dn_free (dn_found);
		return (res);
	}

out:;
	dn_free (dn_found);

	if ((*ent_p)->e_parent == NULLENTRY)
	{
		DLOG(log_dsap, LLOG_DEBUG, ("find_entry: (*ent_p)->e_parent is NULLENTRY"));
		return (DS_OK);     /* no acl for root entry */
	}

	if (check_acl (acl_who,ACL_DETECT, (*ent_p)->e_parent->e_acl->ac_child, object) == NOTOK) {
		err->dse_type = DSE_SECURITYERROR;
		err->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_X500_ERROR);
	}

	if (check_acl (acl_who,ACL_DETECT, (*ent_p)->e_acl->ac_entry, object) == NOTOK) {
		err->dse_type = DSE_SECURITYERROR;
		err->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_X500_ERROR);
	}

	return (DS_OK);
}

int	  find_child_entry (object,ca,acl_who,dn_stack,master,ent_p,err,di_p)
DN                        object;
common_args             * ca;
DN    			  acl_who;
struct dn_seq		* dn_stack;
int			  master;
Entry			* ent_p;
struct DSError          * err;
struct di_block		**di_p;
{
/* this is very similar to find_entry(), except a top level */
/* constructor is allowed */
int deref = FALSE;
int res;
DN dn_found;
#ifdef TURBO_AVL
Entry	akid;
#endif

	DLOG (log_dsap,LLOG_DEBUG,("find_child_entry"));
	err->dse_type = DSE_NOERROR;

	if ((ca->ca_servicecontrol.svc_options & SVC_OPT_DONTDEREFERENCEALIAS) == 0)
		deref = TRUE;

	if ((ca->ca_servicecontrol.svc_options & SVC_OPT_DONTUSECOPY) != 0)
		master = TRUE;

	switch(really_find_entry(object,deref,dn_stack,master,ent_p,err,di_p))
	{
	case DS_OK:
	    DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - rfe: OK"));
	    /* Have set up ent_p continue processing */
	    break;

	case DS_CONTINUE:
	    DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - rfe: CONTINUE"));
#ifdef DEBUG
	    di_list_log((*di_p));
#endif
	    /* Have set up di_blocks of DSAs to be questioned */
	    return(DS_CONTINUE);

	case DS_X500_ERROR:
	    /* Have set up an error */
	    DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - rfe: X500_ERROR"));
	    return(DS_X500_ERROR);

	default:
	    /* Scream */
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("really_find_entry failed in find_entry 1"));
	    return(DS_ERROR_LOCAL);
	}

	/* check to see if children OK */
#ifdef TURBO_AVL
	if ((*ent_p)->e_children != NULLAVL && (*ent_p)->e_allchildrenpresent
	    != FALSE) {
#else
	if (((*ent_p)->e_child != NULLENTRY) && ((*ent_p)->e_allchildrenpresent != FALSE)) {
#endif
	    	DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - children OK"));
#ifdef TURBO_AVL
		akid = (Entry) avl_getone((*ent_p)->e_children);
		switch (akid->e_data) {
#else
		switch ((*ent_p)->e_child->e_data) {
#endif
		case E_DATA_MASTER:
			DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - children masters"));
			break;
		case E_TYPE_SLAVE:
			/* see if we can use a copy ... */
			DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - children slaves"));
		    	if (master) {
				dn_found = get_copy_dn (*ent_p);
				res = constructor_dsa_info_aux(dn_found,dn_stack,master,(*ent_p),err,di_p);
				dn_free (dn_found);
				return (res);
		    	}
			break;
		default:
			DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - default"));
			dn_found = get_copy_dn (*ent_p);
			res = constructor_dsa_info_aux(dn_found,dn_stack,master,(*ent_p),err,di_p);
			dn_free (dn_found);
			return (res);
		}
	} else if ( (! isleaf(*ent_p)) || (*ent_p)->e_external) {
		DLOG(log_dsap, LLOG_DEBUG, ("find_child_entry - children NOTOK"));
		dn_found = get_copy_dn (*ent_p);
		res = constructor_dsa_info_aux(dn_found,dn_stack,master,(*ent_p),err,di_p);
		dn_free (dn_found);
		return (res);
	}

	if (check_acl (acl_who,ACL_DETECT, (*ent_p)->e_acl->ac_child, object) == NOTOK) {
		err->dse_type = DSE_SECURITYERROR;
		err->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_X500_ERROR);
		}

	return (DS_OK);
}

int	  really_find_entry (object, deref, dn_stack, master, ent_p, err, di_p)
DN		  object;
int		  deref;
struct dn_seq	* dn_stack;
int		  master;	/* Generate only master references - NB
				   does not imply returned entry is master */
Entry		* ent_p;
struct DSError	* err;
struct di_block	**di_p;
{
#ifdef TURBO_AVL
Entry parent;
Avlnode *kids;
int entryrdn_cmp ();
#else
register RDN a_rdn;
Entry  trail;
#endif
register RDN b_rdn;
DN     tdn, dn, dn_trail = NULLDN;
DN     aliasdn = NULLDN;
int rdns, aliases;

	DLOG (log_dsap,LLOG_TRACE,("really find entry"));

	if (deref == -2) {
		/* alias loop */
		err->dse_type = DSE_NAMEERROR;
		err->ERR_NAME.DSE_na_problem = DSE_NA_ALIASDEREFERENCE;
		err->ERR_NAME.DSE_na_matched = NULLDN;
		return (DS_X500_ERROR);
	}

	if (database_root == NULLENTRY) {
		LLOG (log_dsap,LLOG_NOTICE,("null root !!!"));
		return(dsa_info_parent(object, err, di_p, master));
	}

	if ((dn = object) == NULLDN) {
		DLOG(log_dsap,LLOG_DEBUG,("really_fe - DS_OK: database_root"));
		(*ent_p) = database_root;
		return (DS_OK);
	}

	b_rdn = dn->dn_rdn;
#ifdef TURBO_AVL
 	if ((kids = database_root->e_children) == NULLAVL) {
#else
	if (((*ent_p) = database_root->e_child) == NULLENTRY) {
#endif
		DLOG(log_dsap, LLOG_DEBUG, ("database->e_child == NULLENTRY"));
		return (no_reply_child (object,dn,dn_stack,master,database_root,err,di_p));
	}

#ifdef TURBO_AVL
 	parent = database_root;
#else
	a_rdn = (*ent_p)->e_name ;
#endif

	for(rdns = 1, aliases = 0 ; ; rdns++ ) { /* break or return out */
#ifdef TURBO_AVL
 		*ent_p = (Entry) avl_find(kids, (caddr_t) b_rdn, entryrdn_cmp);
 		if ( *ent_p == NULLENTRY ) {
 			int res = no_reply_edb(object, dn_trail, dn_stack,
 			    master, parent, err, di_p);
			if (res == DS_CONTINUE) 
				di_rdns (*di_p,rdns,aliases);
			if (aliasdn)
				dn_free (aliasdn);
			return res;
		}
#else
		trail = NULLENTRY;
		while (rdn_cmp (a_rdn, b_rdn) != OK) {
			trail = (*ent_p);
			(*ent_p) = (*ent_p)->e_sibling ;
			if ( (*ent_p) == NULLENTRY ) {
				int res = no_reply_edb (object,dn_trail,dn_stack,master,trail->e_parent,err,di_p);
				if (res == DS_CONTINUE) 
					di_rdns (*di_p,rdns,aliases);

				if (aliasdn)
					dn_free (aliasdn);
				return res;
			}
			a_rdn = (*ent_p)->e_name ;
		}

		/* make found element first in list - optimistaion */
		if (trail != NULLENTRY) {  /* NOT already the first */
			trail->e_sibling = (*ent_p)->e_sibling;
			(*ent_p)->e_sibling = (*ent_p)->e_parent->e_child;
			(*ent_p)->e_parent->e_child = (*ent_p);
		}
#endif /* TURBO_AVL */

		if ( (*ent_p)->e_alias != NULLDN )
			/* got an alias entry */
			if (deref != FALSE) {
				Entry	  new_entry;
				int	  new_deref;
				DN 	  t_aliasdn;

				err->dse_type = DSE_NAMEERROR;
				new_deref = (deref == -1) ? -2 : -1;
				switch(really_find_entry ((*ent_p)->e_alias,new_deref,dn_stack,master,&(new_entry),err,di_p))
				{
				case DS_OK:
				    DLOG(log_dsap, LLOG_DEBUG, ("rfe:rfe:OK"));
				    (*ent_p) = new_entry;
				    t_aliasdn = aliasdn;
				    aliasdn = get_copy_dn(new_entry);
				    aliases = 0;
				    for (tdn=aliasdn; tdn != NULLDN; tdn=tdn->dn_parent) 
					    aliases++;

				    tdn = dn->dn_parent;
				    if (aliasdn == NULLDN)
					    dn = aliasdn = dn_cpy(tdn);
				    else {
					    for (dn = aliasdn; 
						 dn->dn_parent != NULLDN;
						 dn = dn->dn_parent)
						    ;
					    dn->dn_parent = dn_cpy(tdn);
				    }
				    if (t_aliasdn)
					dn_free (t_aliasdn);

				    object = aliasdn;
				    break;
				case DS_CONTINUE:
				    DLOG(log_dsap, LLOG_DEBUG, ("rfe:rfe:CONT"));
#ifdef DEBUG
	        			di_list_log((*di_p));
#endif
					di_rdns (*di_p,rdns,aliases);

				        if (aliasdn)
						dn_free (aliasdn);
					return(DS_CONTINUE);
				case DS_X500_ERROR:
				    DLOG(log_dsap, LLOG_DEBUG, ("rfe:rfe:X500ERR"));
					if ((err->dse_type == DSE_NAMEERROR) && 
						( err->ERR_NAME.DSE_na_problem == DSE_NA_ALIASDEREFERENCE)) {
						if (err->ERR_NAME.DSE_na_matched == NULLDN) {
							DN tmp_dn;
							tmp_dn = dn->dn_parent;
							dn->dn_parent = NULLDN;
							err->ERR_NAME.DSE_na_matched = dn_cpy(object);
							dn->dn_parent = tmp_dn;
							pslog (log_dsap,LLOG_EXCEPTIONS,"Alias deref Problem",dn_print,(caddr_t)err->ERR_NAME.DSE_na_matched);
						}
						if (aliasdn)
							dn_free (aliasdn);
						return (DS_X500_ERROR);
					} else {
						ds_error_free (err);
						err->dse_type = DSE_NAMEERROR;
						err->ERR_NAME.DSE_na_problem = DSE_NA_ALIASPROBLEM;
						err->ERR_NAME.DSE_na_matched = dn_cpy((*ent_p)->e_alias);
						pslog (log_dsap,LLOG_EXCEPTIONS,"Alias Problem",dn_print,(caddr_t)err->ERR_NAME.DSE_na_matched);
						if (aliasdn)
							dn_free (aliasdn);
						return (DS_X500_ERROR);
					}
				default:
				    if (aliasdn)
					dn_free (aliasdn);
				    DLOG(log_dsap, LLOG_DEBUG, ("rfe:rfe:localerror"));
					return(DS_ERROR_LOCAL);
				}
				

			} else if ( dn->dn_parent == NULLDN) {
				DLOG(log_dsap,LLOG_DEBUG,("really_fe - DS_OK: ?1"));
				if (aliasdn)
					dn_free (aliasdn);
				return(DS_OK);
			} else {
				/* alias on route - error in this case */
				DN tmp_dn;
				err->dse_type = DSE_NAMEERROR;
				err->ERR_NAME.DSE_na_problem = DSE_NA_ALIASDEREFERENCE;
				tmp_dn = dn->dn_parent;
				dn->dn_parent = NULLDN;
				err->ERR_NAME.DSE_na_matched = dn_cpy(object);
				dn->dn_parent = tmp_dn;
				pslog (log_dsap,LLOG_EXCEPTIONS,"Alias deref(2) Problem",dn_print,(caddr_t)err->ERR_NAME.DSE_na_matched);
				if (aliasdn)
					dn_free (aliasdn);
				return (DS_X500_ERROR);
			}


		if (dn->dn_parent == NULLDN) {
			DLOG(log_dsap,LLOG_DEBUG,("really_fe - DS_OK: ?2"));
			if (aliasdn)
				dn_free (aliasdn);
			return (DS_OK);
		}

#ifdef TURBO_AVL
		if ((*ent_p)->e_children == NULLAVL) {
#else
		if ( (*ent_p)->e_child == NULLENTRY ) {
#endif
			int res = no_reply_child (object,dn,dn_stack,master,(*ent_p),err,di_p);
		        if (res == DS_CONTINUE) 
				di_rdns (*di_p,rdns,aliases);

			if (aliasdn)
				dn_free (aliasdn);
			return res;						
		}

		dn_trail = dn;
		dn = dn->dn_parent;
		b_rdn = dn->dn_rdn;

#ifdef TURBO_AVL
		kids = (*ent_p)->e_children;
		parent = *ent_p;
#else
		(*ent_p) = (*ent_p)->e_child;
		a_rdn = (*ent_p)->e_name;
#endif
	}
	/* NOTREACHED */
}


int	  referral_dsa_info (object,dn_stack,master,ptr,err,di_p,chain)
DN		  object;
struct dn_seq	* dn_stack;
int		  master;
Entry		  ptr;
struct DSError	* err;
struct di_block	**di_p;
char chain;
{
int ret;
struct di_block     * di_tmp;

	DLOG (log_dsap,LLOG_TRACE,("referral dsa_info"));
	/* generate a referral to a DUA if possible */

	if (ptr != NULLENTRY)
		ptr=ptr->e_parent;

	if ((ret = constructor_dsa_info_aux(object,dn_stack,master,ptr,err,di_p)) != DS_CONTINUE)
		return ret;

	/* Try to make a referral - if not schedule a chain !!! */
	if (chain)
		return DS_CONTINUE;

	/* PROBLEM: The following will get the best referral from our point 
	 * of view.  This may not be the same from the DUAs point of view !!!
         */
	sort_dsa_list (di_p);
        for(di_tmp= *di_p; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
        {
                if(di_tmp->di_state == DI_DEFERRED)
                        continue;

                if(di2cref(di_tmp, err, DS_CTX_X500_DAP) == OK)
                    return (DS_X500_ERROR);	/* return the referral !! */
        }
	return DS_CONTINUE;

}

int	  constructor_dsa_info (object,dn_stack,master,ptr,err,di_p)
DN		  object;
struct dn_seq	* dn_stack;
int		  master;
Entry		  ptr;
struct DSError	* err;
struct di_block	**di_p;
{
	DLOG (log_dsap,LLOG_TRACE,("constructor dsa_info"));

	if (ptr != NULLENTRY)
		ptr=ptr->e_parent;
		
	return(constructor_dsa_info_aux(object,dn_stack,master,ptr,err,di_p));
}

int	  constructor_dsa_info_aux(object,dn_stack,master,ptr,err,di_p)
DN		  object;
struct dn_seq	* dn_stack;
int		  master;
Entry		  ptr;
struct DSError	* err;
struct di_block	**di_p;
{
	DLOG (log_dsap,LLOG_TRACE,("construct dsa_info aux"));

	/* follow entry back, until something that is not a CONSTRUCTOR */

	for (; ptr!= NULLENTRY; ptr=ptr->e_parent)
		if ((ptr->e_data == E_DATA_MASTER) ||
		    (ptr->e_data == E_TYPE_SLAVE) ||
	    	    ((ptr->e_data == E_TYPE_CACHE_FROM_MASTER) &&
		     (timenow - ptr->e_age < cache_timeout))) {
			if ( (ptr->e_master == NULLAV) && (ptr->e_slave == NULLAV))
				continue ;
			return(dsa_info_new(object,dn_stack,master,ptr,err,di_p));
		}

	return(dsa_info_parent(object,err,di_p,master));
}

int	  no_reply_child (object,dn,dn_stack,master,entryptr,err,di_p)
DN		  object;
DN		  dn; 	/* tail - not matched */
struct dn_seq	* dn_stack;
int		  master;
Entry		  entryptr;
struct DSError	* err;
struct di_block	**di_p;
{
DN dn_tmp;
	
	DLOG (log_dsap,LLOG_TRACE,("no reply child"));

	if (isleaf(entryptr))
	{
		DLOG (log_dsap,LLOG_DEBUG,("definate NO"));
		if (dn != NULLDN) {
			dn_tmp = dn->dn_parent;
			dn->dn_parent = NULLDN;
		} else
			object = NULLDN;
		err->dse_type = DSE_NAMEERROR;
		err->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		err->ERR_NAME.DSE_na_matched = dn_cpy (object);
		if (dn != NULLDN)
			dn->dn_parent = dn_tmp;

		return(DS_X500_ERROR);
	}
	
	return(constructor_dsa_info_aux (object, dn_stack, master, entryptr, err, di_p));
}

int	  no_reply_edb (object,dn,dn_stack,master,entryptr,err,di_p)
DN		  object;
DN		  dn; 	/* tail - not matched */
struct dn_seq	* dn_stack;
int		  master;
Entry		  entryptr;
struct DSError	* err;
struct di_block	**di_p;
{
DN dn_tmp;
#ifdef TURBO_AVL
Entry akid;
#endif

	DLOG (log_dsap,LLOG_TRACE,("no reply edb"));

	if (isleaf(entryptr)) {
		DLOG (log_dsap,LLOG_DEBUG,("definate NO"));
		if (dn != NULLDN) {
			dn_tmp = dn->dn_parent;
			dn->dn_parent = NULLDN;
		} else
			object = NULLDN;
		err->dse_type = DSE_NAMEERROR;
		err->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		err->ERR_NAME.DSE_na_matched = dn_cpy (object);
		if (dn != NULLDN)
			dn->dn_parent = dn_tmp;

		return(DS_X500_ERROR);
	}

#ifdef TURBO_AVL
	if (entryptr->e_children == NULLAVL) {
#else
	if (entryptr->e_child == NULLENTRY) {
#endif
		return(constructor_dsa_info(object,dn_stack,master,entryptr,err,di_p));
	}

#ifdef TURBO_AVL
	akid = (Entry) avl_getone(entryptr->e_children);
	if ((akid->e_data == E_DATA_MASTER)
	    || ((! master) && (akid->e_data == E_TYPE_SLAVE)) ) {
#else
	if ((entryptr->e_child->e_data == E_DATA_MASTER)
		|| ((! master) && (entryptr->e_child->e_data == E_TYPE_SLAVE))) {
#endif
		DLOG (log_dsap,LLOG_DEBUG,("definate NO"));
		if (dn != NULLDN) {
			dn_tmp = dn->dn_parent;
			dn->dn_parent = NULLDN;
		} else
			object = NULLDN;
		err->dse_type = DSE_NAMEERROR;
		err->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
		err->ERR_NAME.DSE_na_matched = dn_cpy (object);
		if (dn != NULLDN)
			dn->dn_parent = dn_tmp;
		return(DS_X500_ERROR);
	}

	/* build a referral */
	return(constructor_dsa_info_aux(object,dn_stack,master,entryptr,err,di_p));
}
