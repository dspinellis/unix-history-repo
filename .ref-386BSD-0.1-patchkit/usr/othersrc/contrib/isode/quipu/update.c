/* update.c - write EDB back to disk after modify */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/update.c,v 7.6 91/03/09 11:57:16 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/update.c,v 7.6 91/03/09 11:57:16 mrose Exp $
 *
 *
 * $Log:	update.c,v $
 * Revision 7.6  91/03/09  11:57:16  mrose
 * update
 * 
 * Revision 7.5  91/02/22  09:40:12  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/10/17  11:55:02  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:46:49  mrose
 * sync
 * 
 * Revision 7.2  90/03/15  11:19:14  mrose
 * quipu-sync
 * 
 * Revision 7.1  89/12/19  16:20:54  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:18  mrose
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
#include "tailor.h"
#include "quipu/read.h"
#include "quipu/dua.h"
#include "quipu/connection.h"
#ifdef TURBO_AVL
#include "quipu/turbo.h"
extern int parent_link();
#endif

#ifndef NO_STATS
extern LLog * log_stat;
#endif
extern int dn_print ();
extern char *new_version ();

extern LLog * log_dsap;
extern char remote_lookup;
extern int local_slave_size;
extern int slave_edbs;
extern Entry database_root;
extern DN mydsadn;
extern Attr_Sequence dsa_real_attr;
extern Entry local_find_entry_aux();

struct oper_act *	oper_alloc();
struct oper_act *	make_get_edb_op();

#ifndef TURBO_DISK
/* routine name is historic - not significant */

journal (myentry)
Entry myentry;
{
char * filename, *dn2edbfile();
char savefile [LINESIZE], newfile[LINESIZE];
DN dn;
extern char * treedir;
extern char * parse_file;
Entry liststart;
extern int errno;

	if (myentry == NULLENTRY) {
		LLOG (log_dsap,LLOG_FATAL,("update edb problem"));
		return NOTOK;
	}

	if ((myentry->e_parent != NULLENTRY) && (myentry->e_parent->e_leaf)) {
		liststart = myentry;
		dn = get_copy_dn (liststart->e_parent);
		if ((filename = dn2edbfile (dn)) == NULLCP) {
			dn_free (dn);
			return NOTOK;
		}
		myentry->e_parent->e_leaf = FALSE;  /* not a leaf now !! */

		if ( write_edb(liststart,filename) != OK) {
			dn_free (dn);
			return NOTOK;
		}
	} else {
		if (myentry->e_parent == NULLENTRY) {
			liststart = myentry;
			dn = NULLDN;
		} else {
#ifdef TURBO_AVL
			liststart = (Entry) avl_getone(myentry->e_parent->e_children);
#else
			liststart = myentry->e_parent->e_child;
#endif
	 		dn = get_copy_dn (liststart->e_parent);
		}
		if ((filename = dn2edbfile (dn)) == NULLCP) {
			dn_free (dn);
			return NOTOK;
		}

		(void) strcpy (newfile,filename);
		(void) strcat (newfile,".new");

		if ( write_edb(liststart,newfile) != OK) {
		    (void) unlink (newfile);
		    dn_free (dn);
		    return NOTOK;
		}

		(void) strcpy (savefile,filename);
		(void) strcat (savefile,".bak");

		(void) unlink (savefile);
		if (link (filename, savefile) == NOTOK)
		    SLOG (log_dsap, LLOG_EXCEPTIONS, savefile,
			  ("unable to link %s to", filename));
		if (rename (newfile, filename) == NOTOK) {
		    SLOG (log_dsap, LLOG_EXCEPTIONS, filename,
			  ("unable to rename %s to", newfile));
		    if (link (savefile, filename) == NOTOK
			    && !fileexists (filename))
			LLOG (log_dsap, LLOG_EXCEPTIONS,
			      ("and couldn't get old file back - PANIC!!!")) 
		    dn_free (dn);
		    return NOTOK;
		}
	}

	dn_free (dn);

	return OK;
}
#endif TURBO_DISK

modify_attr (eptr,who)
Entry eptr;
DN who;
{
AttributeType at;
AttributeValue av;
AV_Sequence avs;
Attr_Sequence as, old, entry_find_type();
extern int	  no_last_mod;

	if (no_last_mod)
		return;

	at = AttrT_new (LAST_MOD_OID);

	av = AttrV_alloc ();
	av->av_syntax = str2syntax ("UTCTime");
	av->av_struct = (caddr_t) new_version();

	avs = avs_comp_new(av);

	if ((old = as_find_type (eptr->e_attributes,at)) == NULLATTR) {
		as = as_comp_new (at,avs,NULLACL_INFO);
		eptr->e_attributes = as_merge (eptr->e_attributes,as);
	} else {
		avs_free (old->attr_value);
		old->attr_value = avs;
		AttrT_free (at);
	}

	at = AttrT_new (MOD_BY_OID);

	av = AttrV_alloc ();
	av->av_syntax = str2syntax ("DN");
	av->av_struct = (caddr_t) dn_cpy (who);

	avs = avs_comp_new(av);

	/* Is it inherited ? */
	set_inheritance (eptr);
	if ((old = entry_find_type (eptr,at)) != NULLATTR) 
		/* possibly... */
		if (as_find_type (eptr->e_attributes,at) == NULLATTR)
			/* yes ! */
			if (avs_cmp(avs, old->attr_value) == 0) {
				/* No need to change it ! */
				avs_free (avs);
				return;
			}

	if (old == NULLATTR) {
		as = as_comp_new (at,avs,NULLACL_INFO);
		eptr->e_attributes = as_merge (eptr->e_attributes,as);
	} else {
		avs_free (old->attr_value);
		old->attr_value = avs;
		AttrT_free (at);
	}
}

static allowed_to_send (a,b)
register DN  a,b;
{
	/* Return TRUE if the DNs are the same */
	/* Return TRUE if all components of 'a' match, but 'b' has one extra */
	/* False otherwise */

	if ((a == NULLDN) || (b == NULLDN))
		return FALSE;

	for (; a != NULLDN && b != NULLDN ; a = a->dn_parent, b = b->dn_parent)
		if ( dn_comp_cmp (a,b) == NOTOK)
			return FALSE;

	if (( b == NULLDN) || (b->dn_parent == NULLDN))
		return TRUE;
	else
		return FALSE;

}


do_get_edb (arg,error,result,binddn)
struct getedb_arg *arg;
struct DSError	  *error;
struct getedb_result *result;
DN binddn;
{
Entry eptr;
Entry my_entry;
AV_Sequence avs;
struct edb_info * dsainfo;
char proceed = FALSE;
struct dn_seq	* dnseq;
struct di_block	* di;
#ifdef TURBO_AVL
Entry	akid;
#endif

	DLOG (log_dsap,LLOG_DEBUG,("getedb '%s'",arg->ga_version));

	switch(really_find_entry (arg->ga_entry,FALSE,NULLDNSEQ,FALSE,&(eptr),error,&(di)))
	{
	case DS_OK:
	    /*
	    *  Entry has been found and returned via eptr.
	    *  Go through and process this entry.
	    */
	    break;

	case DS_CONTINUE:
	    /*
	    * Get edb operations should never generate referrals.
	    * Free the di_blocks generated and return an error.
	    */
	    error->dse_type = DSE_SERVICEERROR;
	    error->ERR_SERVICE.DSE_sv_problem = DSE_SV_CHAININGREQUIRED;
	    return (DS_X500_ERROR);

	case DS_X500_ERROR:
	    /* something wrong with the request - error should be filled out */
	    return(DS_X500_ERROR);

	default:
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_get_edb() - really_find_entry() failed"));
	    error->dse_type = DSE_SERVICEERROR;
	    error->ERR_SERVICE.DSE_sv_problem = DSE_SV_DITERROR;
	    return (DS_X500_ERROR);
	}

	if ((my_entry = local_find_entry_aux (mydsadn,TRUE)) == NULLENTRY)
		fatal (84,"my entry has gone - no getedb");

	/* Check we will send to this DSA */
	for (avs = my_entry->e_dsainfo->dsa_attr ; avs != NULLAV; avs=avs->avseq_next) {
		if (avs->avseq_av.av_struct == NULL)
			continue;
		dsainfo = (struct edb_info *) avs->avseq_av.av_struct;
		if (dn_cmp(dsainfo->edb_name,arg->ga_entry) == 0) {
			for (dnseq=dsainfo->edb_allowed; dnseq!=NULLDNSEQ; dnseq=dnseq->dns_next) {
				if (allowed_to_send(dnseq->dns_dn,binddn)) {
					proceed = TRUE;
					break;
				}
			}
		}
		if (proceed)
			break;
	}
					
	if (!proceed) {
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		return (DS_ERROR_REMOTE);
	}

#ifdef TURBO_AVL
	akid = (Entry) avl_getone(eptr->e_children);
	if (akid == NULLENTRY || (akid->e_data != E_DATA_MASTER
	    && akid->e_data != E_TYPE_SLAVE)) {
#else
	if ((eptr->e_child == NULLENTRY) 
		|| ((eptr->e_child->e_data != E_DATA_MASTER) 
			&& (eptr->e_child->e_data != E_TYPE_SLAVE))) {
#endif
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_DITERROR;
		return (DS_X500_ERROR);
	}

	if (eptr->e_edbversion != NULLCP) {
		DLOG(log_dsap, LLOG_DEBUG, ("edb_ver = %s", eptr->e_edbversion));

		if (lexequ (arg->ga_version,eptr->e_edbversion) == 0) {
			result->gr_version = eptr->e_edbversion;
#ifdef TURBO_AVL
			result->gr_edb = NULLAVL;
#else
			result->gr_edb = NULLENTRY;
#endif
			result->gr_next = NULL_GETRESULT;
			return (DS_OK);
		}
	} else 
		eptr->e_edbversion = new_version();

	result->gr_version = eptr->e_edbversion;
#ifdef TURBO_AVL
	result->gr_edb = eptr->e_children;
#else
	result->gr_edb = eptr->e_child;
#endif
	result->gr_next = NULL_GETRESULT;
	return (DS_OK);
}

slave_update () 
{ 
extern time_t lastedb_update, timenow;

	(void) update_aux (NULLDN, 0); 
	shadow_update ();
	lastedb_update = timenow;
}

update_aux (dn, isroot)
DN	dn;
int	isroot;
{
Entry my_entry, make_path();
Entry find_sibling();
extern DN mydsadn;
struct edb_info * dsainfo;
Entry eptr;
static char *version = NULLCP;
AV_Sequence avs;
AV_Sequence avs_head;
int		  success;

	DLOG (log_dsap,LLOG_TRACE,("slave update"));

	if ((my_entry = local_find_entry_aux (mydsadn,TRUE)) == NULLENTRY)
		fatal (82,"Can't update slaves - my entry has gone");

	avs_head = avs_cpy(my_entry->e_dsainfo->dsa_attr);

	for (avs = avs_head ; avs != NULLAV; avs=avs->avseq_next)
	{
		if (avs->avseq_av.av_struct == NULL)
			continue;
		dsainfo = (struct edb_info *) avs->avseq_av.av_struct;
		if (dsainfo->edb_getfrom == NULLDN)
			continue;  /* not an EDB to update */
		if ((dn || isroot) && dn_cmp (dn, dsainfo -> edb_name) != OK)
		    continue;	   /* not an EDB this time */

		if ((eptr = local_find_entry (dsainfo->edb_name,FALSE)) == NULLENTRY) {
			version = "0000000000Z";
			eptr = make_path (dsainfo->edb_name);
		} 
		else
		{
			if((version = eptr->e_edbversion) == NULLCP)
			{
			    LLOG(log_dsap, LLOG_NOTICE, ("update_aux: edbversion was NULLCP"));
			    version = "0000000000Z";
			}
		}

		success = send_get_edb(version, dsainfo->edb_name, dsainfo->edb_getfrom);

		if(dn || isroot)
		    break;

		dsa_wait (0);	/* accept any results of previous ops */

	}
	avs_free (avs_head);

	return((dn || isroot) ? success : OK);
}

int	 send_get_edb (version,dn,from)
char	* version;
DN dn,from;
{
struct di_block		* di;
struct DSError		  error;
struct oper_act		* on;
char    buffer[BUFSIZ];
PS	    ps;

	switch(get_dsa_info(from, NULLDNSEQ, &(error), &(di)))
	{
	case DS_OK:
	    /*
	    *  di is a completed dsa info block
	    *  Make a get_edb operation from it, attempt to send the operation
	    *  and link the operation onto the global list of get_edb
	    *  operations.
	    */
	    if (ps = ps_alloc (str_open)) {
		if (str_setup (ps, buffer, sizeof buffer, 1) != NOTOK) {
		    ps_printf (ps, "contact ");
		    dn_print (ps, from, EDBOUT);
		    ps_printf (ps, " for ");
		    if (dn)
			dn_print (ps, dn, EDBOUT);
		    *ps -> ps_ptr = NULL;

		    LLOG (log_dsap, LLOG_NOTICE, ("%s", buffer));
		}

		(void) ps_free (ps);
	    }

#ifdef DEBUG
	    DLOG(log_dsap, LLOG_DEBUG, ("send_get_edb - get_dsa_info OK:"));
	    di_list_log(di);
#endif
	    if((on = make_get_edb_op(dn, version, di)) == NULLOPER)
	    {
		/* Flake out screaming */
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("make_get_edb_op failed for send_get_edb"));
		return(NOTOK);
	    }

#ifdef PARA_GETEDB	    

	    /* Do getEDBs in parallel as in 6.0 */

	    if(oper_chain(on) != OK)
	    {
		LLOG(log_dsap, LLOG_NOTICE, ("Could not chain a getEDB operation"));
		return(NOTOK);
	    }

	    on->on_next_task = get_edb_ops;
	    get_edb_ops = on;
#else

	    /* Do getEDBs serially */
	    schedule_operation (on);

#endif

	    return(OK);

	case DS_CONTINUE:
	    /*
	    *  di is a deferred dsa info block
	    *  make the operation and suspend waiting for the di_block to be 
	    *  woken up.
	    */
#ifdef DEBUG
	    DLOG(log_dsap, LLOG_DEBUG, ("send_get_edb - get_dsa_info CONT:"));
	    di_list_log(di);
#endif
	    if((on = make_get_edb_op(dn, version, di)) == NULLOPER)
	    {
		/* Flake out screaming */
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("make_get_edb_op failed for send_get_edb"));
		return(NOTOK);
	    }

	    on->on_state = ON_DEFERRED;

	    on->on_next_task = get_edb_ops;
	    get_edb_ops = on;

	    if (ps = ps_alloc (str_open)) {
		if (str_setup (ps, buffer, sizeof buffer, 1) != NOTOK) {
		    ps_printf (ps, "contact ");
		    dn_print (ps, from, EDBOUT);
		    ps_printf (ps, " for ");
		    if (dn)
			dn_print (ps, dn, EDBOUT);
		    *ps -> ps_ptr = NULL;

		    LLOG (log_dsap, LLOG_NOTICE, ("%s", buffer));
		}

		(void) ps_free (ps);
	    }

	    return(OK);

	case DS_X500_ERROR:
	    /* Error encountered generating di_block */
	    LLOG(log_dsap, LLOG_NOTICE, ("send_get_edb - get_dsa_info returned X500 ERROR"));
	    log_ds_error (&error);
	    ds_error_free (&error);
	    return(NOTOK);

	default:
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("send_get_edb - get_dsa_info unexpected return"));
	    return(NOTOK);
	}
	/* NOTREACHED */
}

#ifdef TURBO_AVL

static Entry	g_parent;
static int 	g_entry_cnt;

/*
 * unravel_edb - called from avl_apply on a new slave edb to unravel the
 * new entries and link them to their new parent node, which should be
 * set in g_parent before the call.
 */

static unravel_edb(e, error)
Entry           e;
struct DSError  *error;
{
        e->e_parent = g_parent;
        if (unravel_attribute(e, error) != OK)
                return(NOTOK);

	shadow_entry (e);
        turbo_add2index(e);
	return OK;
}

/*
 * make_parent - called from link_child to set the parent of a node.
 * it has to happen in this grossly inefficient way because make_parent
 * is called from avl_apply.
 */

static link_child(e, oldkids)
Entry   e;
Avlnode *oldkids;
{
        Entry   old_entry;
        int     entryrdn_cmp();

        g_entry_cnt++;

        /* find the old entry the new one is replacing */
        old_entry = (Entry) avl_find(oldkids, (caddr_t) e->e_name, entryrdn_cmp);
        if (old_entry == NULLENTRY)
                return(OK);

        e->e_leaf = old_entry->e_leaf;
        e->e_allchildrenpresent = old_entry->e_allchildrenpresent;
        e->e_children = old_entry->e_children;

        turbo_index_delete(old_entry);
        turbo_add2index(e);

        /* link children to their new parent */
        (void) avl_apply(e->e_children, parent_link, (caddr_t) e, NOTOK, AVL_PREORDER);

        if (old_entry->e_edbversion != NULLCP)
                e->e_edbversion = strdup(old_entry->e_edbversion);

        return(OK);
}

#endif /* TURBO_AVL */

process_edb(on)
struct oper_act	* on;
{
extern DN mydsadn;
Entry make_path(), find_sibling();
Entry eptr;
#ifdef TURBO_AVL
Avlnode	*newkids;
int	entry_free();
#else
Entry new_entry, old_entry, temp, sibl, next;
int entry_cnt = 0;
#endif
struct DSError  error;
struct getedb_result	* result = &(on->on_resp.di_result.dr_res.dcr_dsres.res_ge);
char got_subtree = TRUE;

	if ((eptr = local_find_entry (on->on_req.dca_dsarg.arg_ge.ga_entry,FALSE)) == NULLENTRY) {
		pslog (log_dsap,LLOG_EXCEPTIONS,"Updating something which does not exist !!!",
			dn_print,(caddr_t)on->on_req.dca_dsarg.arg_ge.ga_entry);
		return;
	}

#ifdef TURBO_AVL
	if ((newkids = result->gr_edb) == NULLAVL) {
#else
	if ((new_entry = result->gr_edb) == NULLENTRY) {
#endif
		DLOG (log_dsap, LLOG_NOTICE,("  EDBs are the same (%d): %s",on->on_id,on->on_getedb_ver));
		return;
	} else {
	        DLOG (log_dsap, LLOG_NOTICE,("  EDB updated from (%d): %s to: %s", on->on_id,on->on_getedb_ver, result->gr_version));
	}

#ifndef NO_STATS
	{
	DN tmp_dn;
	tmp_dn = get_copy_dn (eptr);
	pslog (log_stat,LLOG_NOTICE,"Slave update",dn_print,(caddr_t)tmp_dn);
	dn_free (tmp_dn);
	}
#endif

	if (eptr->e_edbversion)
		free (eptr->e_edbversion);

	if (result->gr_version == NULLCP) {
		eptr->e_edbversion = "Unknown";
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("EDBRES: NULL version"));
	} else
		eptr->e_edbversion = strdup (result->gr_version);

#ifdef TURBO_AVL
	/*
         * now unravel the attributes, linking all nodes to eptr, their
         * parent node.
         */

	g_parent = eptr;
	if (avl_apply(newkids, unravel_edb, (caddr_t) &error, NOTOK, AVL_INORDER)
	    == NOTOK) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Error in new EDB - continuing with old"));
		log_ds_error(&error);
		return;		/* ??? should we free newkids here ??? */
	}

	/*
	 * the new edb is now unravelled and linked to its parent.  now go
	 * through and link children of entries in the old edb that also 
	 * exist in the new edb to their new parent.  link_child updates
	 * g_entry_cnt with the number of entries in the new edb.
	 */

	g_entry_cnt = 0;
	(void) avl_apply(newkids, link_child, (caddr_t) eptr->e_children, NOTOK,
	    AVL_INORDER);

#else

	for (temp = new_entry; temp != NULLENTRY; temp=temp->e_sibling) {
		temp->e_parent = eptr;
		if (unravel_attribute (temp,&error) != OK) {
			LLOG (log_dsap,LLOG_EXCEPTIONS, ("Error in new EDB - continuing with old"));
			log_ds_error (&error);
			return;
		}
		shadow_entry (temp);
	}

	for (temp = new_entry; temp != NULLENTRY; temp=temp->e_sibling) {
		entry_cnt++;
		if ((old_entry = find_sibling (temp->e_name,eptr->e_child)) != NULLENTRY) {
			temp->e_leaf = old_entry->e_leaf;
			if ((temp->e_allchildrenpresent = old_entry->e_allchildrenpresent) != 2)
				got_subtree = FALSE;
			temp->e_child = old_entry->e_child;
			for (sibl = temp->e_child; sibl != NULLENTRY; sibl=sibl->e_sibling) {
				sibl->e_parent = temp;
				set_inheritance (sibl);
			}
			if (old_entry->e_edbversion != NULLCP)
				temp->e_edbversion = strdup (old_entry->e_edbversion);
		} else if ( ! temp->e_leaf) {
			got_subtree = FALSE;
			temp->e_allchildrenpresent = FALSE;
		}
	}
#endif /* TURBO_AVL */

	dsa_wait (0);	/* progress any other connections before writing EDB */

#ifdef TURBO_AVL
	if (eptr->e_children == NULLAVL)
#else
	if (eptr->e_child == NULLENTRY)
#endif
		slave_edbs++;

#ifdef TURBO_AVL
	/*
	 * now free up entries from the old edb and update the size of
	 * our cache. avl_free returns the number of nodes freed.
	 */

	local_slave_size -= avl_free(eptr->e_children, entry_free);
#else
	for (temp = eptr->e_child; temp != NULLENTRY; temp=next) {
		next = temp->e_sibling;
		local_slave_size--;
		entry_free (temp);
	}
#endif

#ifdef TURBO_AVL
	eptr->e_children = newkids;
	local_slave_size += g_entry_cnt;
#else
	eptr->e_child = new_entry;
	local_slave_size += entry_cnt;
#endif
	eptr->e_leaf = FALSE;
	eptr->e_allchildrenpresent = (got_subtree ? 2 : 1);

#ifdef TURBO_DISK
	if (turbo_writeall(eptr) != OK)
		fatal (-79,"Lost old EDB, can't write new one!!!");
#else
#ifdef TURBO_AVL
	if (journal ((Entry)avl_getone(eptr->e_children)) != OK)
#else
	if (journal (new_entry) != OK) 
#endif
		fatal (-79,"Lost old EDB, can't write new one !!!");
#endif

	if ((eptr = local_find_entry_aux (mydsadn,TRUE)) == NULLENTRY)
		fatal (-80,"My entry has disappeared from the DIT !!!");
	else if (as_cmp (eptr->e_attributes, dsa_real_attr) != 0) {
		LLOG (log_dsap, LLOG_NOTICE, ("Slave Copy out of date with my entry - re-applying modify"));
		as_free (eptr->e_attributes);
		eptr->e_attributes = as_cpy (dsa_real_attr);
		if (unravel_attribute (eptr,&error) != OK) 
			fatal (-81,"real DSA entry bad error");
	}
}

/*
*  get_edb_fail_wakeup suffices for both fail and error conditions
*  arising on a get edb operation.
*/
get_edb_fail_wakeup(on)
struct oper_act	* on;
{
    struct oper_act	* on_tmp;
    struct oper_act	**on_p;

    DLOG(log_dsap, LLOG_TRACE, ("get_edb_fail_wakeup"));

    if (on -> on_resp.di_type == DI_ERROR) {
	    pslog (log_dsap,LLOG_EXCEPTIONS,"Remote getEDB error",dn_print,
		    (caddr_t) on->on_req.dca_dsarg.arg_ge.ga_entry);
	    log_ds_error (& on -> on_resp.di_error.de_err);
    }

    on_p = &(get_edb_ops);
    for(on_tmp = get_edb_ops; on_tmp != NULLOPER; on_tmp = on_tmp->on_next_task)
    {
	if(on_tmp == on)
	    break;

	on_p = &(on_tmp->on_next_task);
    }

    if(on_tmp != NULLOPER)
    {
	(*on_p) = on_tmp->on_next_task;
    }
    else
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("get_edb_fail_wakeup - op escaped from get_edb_ops (the global list)"));
    }

    oper_conn_extract(on);
    oper_free(on);
}

struct oper_act	* make_get_edb_op(dn, version, di)
DN		  dn;
char		* version;
struct di_block	* di;
{
struct di_block	* di_tmp;
struct oper_act	* on_tmp;
struct getedb_arg	* arg;

	DLOG(log_dsap, LLOG_TRACE, ("make_get_edb_op"));

	if((on_tmp = oper_alloc()) == NULLOPER)
	{
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("make_get_edb_op - out of memory"));
		return(NULLOPER);
	}

	on_tmp->on_type = ON_TYPE_GET_EDB;
	on_tmp->on_arg = &(on_tmp->on_req);
	on_tmp->on_req.dca_dsarg.arg_type = OP_GETEDB;
	on_tmp->on_getedb_ver = version;

	arg = &(on_tmp->on_req.dca_dsarg.arg_ge);

	arg->ga_entry = dn_cpy(dn);
	arg->ga_version = strdup(version);
	DLOG(log_dsap, LLOG_NOTICE, ("EDBARG: ver = %s", arg->ga_version));

	on_tmp->on_dsas = di;
	for(di_tmp=di; di_tmp!=NULL_DI_BLOCK; di_tmp=di_tmp->di_next)
	{
#ifdef DEBUG
	    DLOG(log_dsap, LLOG_DEBUG, ("Linking a di_block to this op"));
	    di_log(di_tmp);
#endif
	    di_tmp->di_type = DI_OPERATION;
	    di_tmp->di_oper = on_tmp;
	}

	return(on_tmp);
}
