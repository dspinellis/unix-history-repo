/* shadow.c - spot shadowing of entries */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/shadow.c,v 7.2 91/03/09 11:57:05 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/shadow.c,v 7.2 91/03/09 11:57:05 mrose Exp $
 *
 *
 * $Log:	shadow.c,v $
 * Revision 7.2  91/03/09  11:57:05  mrose
 * update
 * 
 * Revision 7.1  91/02/22  09:39:50  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/01  18:08:35  mrose
 * *** empty log message ***
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
#include "quipu/read.h"
#include "quipu/dua.h"
#include "quipu/connection.h"
#include "quipu/malloc.h"

#ifndef NO_STATS
extern LLog * log_stat;
#endif
extern LLog * log_dsap;
static struct dn_seq * shades = NULLDNSEQ;
static struct dn_seq * dn_shades = NULLDNSEQ;
extern struct dn_seq * dn_seq_push();
extern struct di_block * di_alloc();
extern struct oper_act * oper_alloc();
extern DN mydsadn;
extern LLog * log_dsap;
extern Entry entry_cpy ();
extern Entry database_root;
extern char * new_version ();
extern int dn_print ();
extern Attr_Sequence as_comp_cpy ();
extern Attr_Sequence entry_find_type ();
extern struct access_point * ap_cpy();
extern time_t timenow;
extern Entry make_path();
extern Entry local_find_entry_aux();

extern AttributeType at_subord;
extern AttributeType at_xref;
extern AttributeType at_nssr;
extern AttributeType at_objectclass;
extern Attr_Sequence cpy_as_comp();
extern short syntax_dn;

typedef struct _atlist {
	AttributeType  at;
	struct _atlist *next;
} *atlist;
#define NULLATL (atlist)NULL
atlist at_list = NULLATL;

shadow_entry (eptr)
Entry eptr;
{
DN dn, ndn;
Attr_Sequence as;
AV_Sequence avs;
atlist at;

	/* All MASTER entries get passed through here. */
	/* See if it need shadowing, if so, add to shades */

   if (eptr->e_data == E_DATA_MASTER) {	
	if ((eptr->e_dsainfo) &&
	    (quipu_ctx_supported (eptr) == 2) &&
	    (quipu_version_7 (eptr))) {
		/* Its a version 7 quipuDSA */

	    dn = get_copy_dn (eptr);

	    if (dn_cmp (dn, mydsadn) == 0) {
		    dn_free (dn);
		    return;  /* can't shadow myself ! */
	    }
	
	    if ( check_dnseq (shades, dn) == NOTOK)
		    shades = dn_seq_push (dn,shades);

	    dn_free (dn);

	} else if ((eptr->e_external) && (eptr->e_reftype != RT_NONSPECIFICSUBORDINATE)) {
		
	    dn = get_copy_dn (eptr);

	    if ( check_dnseq (shades, dn) == NOTOK)
		    shades = dn_seq_push (dn,shades);

	    dn_free (dn);
	} 
    }

    for (as = eptr->e_attributes; as != NULLATTR; as=as->attr_link)
       if (as->attr_type->oa_syntax == syntax_dn)
	  for (at = at_list; at != NULLATL; at=at->next)
	     if (as->attr_type == at->at) {
		for (avs = as->attr_value; avs != NULLAV; avs = avs->avseq_next) {
		   ndn = (DN)avs->avseq_av.av_struct;
		   if (check_dnseq (dn_shades, ndn) == NOTOK)
			dn_shades = dn_seq_push (ndn,dn_shades);
	        }
	     }
}

shadow_attribute (s)
char * s;
{
AttributeType at;
atlist new_atl;

	if (( at = AttrT_new (s)) == NULLAttrT)
	      LLOG (log_dsap, LLOG_EXCEPTIONS, ("Unknown shadow attr %s",s));
	else {
	      new_atl = (atlist) smalloc (sizeof (*new_atl));
	      new_atl->at = at;
	      new_atl->next = at_list;
	      at_list = new_atl;
	}
}

shadow_update ()
{
struct dn_seq * dnseq;
struct oper_act	* op;
static struct ds_read_arg sarg =
{
	default_common_args,
	NULLDN,
	{       /* entry info selection */
		TRUE,
		NULLATTR,
		EIS_ATTRIBUTESANDVALUES
	}
};
Entry eptr;
DN tdn;
struct access_point * aps;
struct DSError err;

	DLOG(log_dsap, LLOG_TRACE, ("shadow_update"));

	for (dnseq = dn_shades; dnseq != NULLDNSEQ; dnseq = dnseq -> dns_next) {
		if ((eptr = local_find_entry (dnseq -> dns_dn,FALSE)) == NULLENTRY) {
			/* aliases !!! */
			if ((eptr = local_find_entry (dnseq -> dns_dn,TRUE)) == NULLENTRY)
				eptr = make_path (dnseq -> dns_dn);
			else if ( eptr -> e_alias )
				eptr = make_path (eptr -> e_alias);
		}

		else if ((eptr->e_data == E_TYPE_SLAVE) || 
			   (eptr->e_data == E_DATA_MASTER)) 
			continue;

		if ( check_dnseq (shades, dnseq -> dns_dn) == NOTOK)
			shades = dn_seq_push (dnseq -> dns_dn,shades);
	}

	dn_seq_free (dn_shades);	
	dn_shades = NULLDNSEQ;

	for (dnseq = shades; dnseq != NULLDNSEQ; dnseq = dnseq -> dns_next) {

		if((op = oper_alloc()) == NULLOPER)
			return;

		op -> on_type = ON_TYPE_SHADOW;
		op -> on_arg = &(op -> on_req);
		op -> on_req.dca_dsarg.arg_type = OP_READ;

		op -> on_req.dca_dsarg.arg_rd = sarg;	  /* struct copy */
		op -> on_req.dca_dsarg.arg_rd.rda_object = 
			dn_cpy (dnseq -> dns_dn);

		op -> on_req.dca_charg.cha_originator = 
			dn_cpy (mydsadn);

		op -> on_req.dca_charg.cha_reftype = RT_SUBORDINATE;

		op -> on_req.dca_charg.cha_progress.op_resolution_phase = 
			OP_PHASE_PROCEEDING;

		op -> on_req.dca_charg.cha_progress.op_nextrdntoberesolved = -1;
		for (tdn = dnseq -> dns_dn ; tdn != NULLDN ; 
		     			     tdn = tdn -> dn_parent)
			op -> 
			on_req.dca_charg.cha_progress.op_nextrdntoberesolved++;

		op -> on_req.dca_charg.cha_trace = 
			(struct trace_info *) malloc (sizeof (struct trace_info));
		op -> on_req.dca_charg.cha_trace -> ti_dsa = 
			dn_cpy (mydsadn);
		op -> on_req.dca_charg.cha_trace -> ti_target = 
			dn_cpy (dnseq -> dns_dn);
		op -> on_req.dca_charg.cha_trace -> ti_progress = 
			op -> on_req.dca_charg.cha_progress;
		op -> on_req.dca_charg.cha_trace -> ti_next = NULLTRACEINFO;

		op -> on_dsas = NULL_DI_BLOCK;

		if ((eptr = local_find_entry_aux (dnseq -> dns_dn,FALSE)) == NULLENTRY) {
		    if ((eptr = local_find_entry_aux (dnseq -> dns_dn,TRUE)) == NULLENTRY) {
			pslog (log_dsap,LLOG_EXCEPTIONS,"Shadow entry missing",
			       dn_print, (caddr_t) dnseq -> dns_dn);
			oper_free (op);
			continue;
		    }
		} else if ( eptr -> e_external ) {
			op -> on_dsas = di_alloc();
			op -> on_dsas -> di_type = DI_TASK;

			op -> on_dsas -> di_rdn_resolved = op ->
				on_req.dca_charg.cha_progress.op_nextrdntoberesolved;
			op -> on_dsas -> di_aliasedRDNs = CR_NOALIASEDRDNS;

			op -> on_dsas -> di_oper = op;
			op -> on_dsas -> di_type = DI_OPERATION;

			op -> on_dsas -> di_target = dn_cpy (dnseq -> dns_dn);
			op -> on_dsas -> di_reftype = eptr-> e_reftype;
			aps = ap_cpy ((struct access_point *) eptr ->
				       e_reference -> avseq_av.av_struct);
			op -> on_dsas -> di_dn = dn_cpy (aps->ap_name);
			op -> on_dsas -> di_accesspoints = aps;
			op -> on_dsas -> di_state = DI_ACCESSPOINT;

		} else if ((eptr->e_data == E_TYPE_SLAVE) || 
			   (eptr->e_data == E_DATA_MASTER)) {
			op -> on_dsas = di_alloc();
			op -> on_dsas -> di_type = DI_TASK;

			op -> on_dsas -> di_rdn_resolved = op ->
				on_req.dca_charg.cha_progress.op_nextrdntoberesolved;
			op -> on_dsas -> di_aliasedRDNs = CR_NOALIASEDRDNS;

			op -> on_dsas -> di_oper = op;
			op -> on_dsas -> di_type = DI_OPERATION;
			op -> on_dsas -> di_dn = dn_cpy (dnseq -> dns_dn);
			op -> on_dsas -> di_target = dn_cpy (dnseq -> dns_dn);
			op -> on_dsas -> di_reftype = RT_SUBORDINATE;
			op -> on_dsas -> di_entry = eptr;
			eptr -> e_refcount++;
			op -> on_dsas -> di_state = DI_COMPLETE;
		} else 
		        (void) constructor_dsa_info (dnseq -> dns_dn, NULLDNSEQ,
				    TRUE, eptr, &err, &(op -> on_dsas) );

		if ( op -> on_dsas )
			schedule_operation (op);
		else 
			oper_free (op);
	}
}


shadow_fail_wakeup (on)
struct oper_act * on;
{
    struct oper_act	* on_tmp;
    struct oper_act	**on_p;

    DLOG (log_dsap, LLOG_NOTICE, ("Shadow fail wakeup"));

    if (on -> on_resp.di_type == DI_ERROR) {
	    pslog (log_dsap,LLOG_EXCEPTIONS,"Remote shadow error",dn_print,
		   (caddr_t) on -> on_req.dca_dsarg.arg_rd.rda_object);
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
	LLOG(log_dsap, LLOG_EXCEPTIONS, 
	     ("subtask_fail_wakeup - op escaped from get_edb_ops (the global list)"));
    }

    oper_conn_extract(on);
    oper_free(on);
}

#ifdef TURBO_AVL
/* ARGSUSED */
inherit_link(e, parent)
Entry   e;
Entry   parent;
{
	set_inheritance (e);
        return(OK);
}
#endif

process_shadow (on)
struct oper_act * on;
{
#ifndef TURBO_AVL
Entry trail = NULLENTRY;
#endif
Entry eptr, ne;
struct DSError err;
Attr_Sequence new_as, as, tas;
DN dn;

	DLOG (log_dsap, LLOG_TRACE, ("Process shadow"));

	dn = on -> on_resp.di_result.dr_res.dcr_dsres.res_rd.rdr_entry.ent_dn;
	
	if ((eptr = local_find_entry_aux (dn,FALSE)) == NULLENTRY) 
		/* aliases on route !!! */
		if ((eptr = local_find_entry_aux (dn,TRUE)) == NULLENTRY) {
			pslog (log_dsap,LLOG_EXCEPTIONS,"Shadow has gone",
			       dn_print, (caddr_t) dn);
			return;
		}

	new_as = on -> 
		 on_resp.di_result.dr_res.dcr_dsres.res_rd.rdr_entry.ent_attr;

	if (eptr -> e_external) {
		/* Add in Quipu attributes */
		
		if (  ((as = entry_find_type (eptr, at_subord)) == NULLATTR) 
	           && ((as = entry_find_type (eptr, at_xref)) != NULLATTR)
	           && ((as = entry_find_type (eptr, at_nssr)) != NULLATTR)) {
			LLOG (log_dsap, LLOG_EXCEPTIONS, (
				        "external reference missing"));
			return;
		}
		new_as = as_merge (new_as, cpy_as_comp (as));

		if ((as = as_find_type (new_as, at_objectclass)) == NULLATTR) {
			LLOG (log_dsap, LLOG_EXCEPTIONS, (
				        "no objectclass in shadow entry"));
			on -> on_resp.di_result.dr_res.
				dcr_dsres.res_rd.rdr_entry.ent_attr = new_as;
			return;
		}

		tas = as_comp_new (AttrT_cpy(at_objectclass),
			  str2avs(EXTERNOBJECT,at_objectclass),NULLACL_INFO);
		new_as = as_merge (new_as,tas);

		on -> on_resp.di_result.dr_res.
			dcr_dsres.res_rd.rdr_entry.ent_attr = new_as;
		
	}

	if (as_cmp (eptr->e_attributes, new_as)	== 0) {
		DLOG (log_dsap, LLOG_NOTICE, ("Shadow: no change"));
		eptr->e_age = timenow;
		return;
	}

#ifndef TURBO_AVL
	if (eptr->e_parent == NULLENTRY) 
		ne = database_root->e_child;
	else	
		ne = eptr->e_parent->e_child;

	/* bring shadow entry to the top */
	for (; ne != eptr; ne=ne->e_sibling) 
		trail = ne;

	if (trail) {
		trail->e_sibling = eptr->e_sibling;
		eptr->e_sibling = eptr->e_parent->e_child;
		eptr->e_parent->e_child = eptr;
	}
#endif

	DATABASE_HEAP;
	ne = entry_cpy (eptr);
	GENERAL_HEAP;

	as_free ( ne -> e_attributes );
	ne -> e_attributes = as_cpy ( new_as );

	if (ne -> e_data == E_TYPE_CONSTRUCTOR) {
		ne -> e_data = E_TYPE_CACHE_FROM_MASTER;
		new_cacheEDB (dn);
	}

#ifdef TURBO_AVL
#ifdef TURBO_INDEX
                turbo_index_delete(eptr);
#endif

                if (unravel_attribute(ne, &err) != OK) {
			pslog (log_dsap,LLOG_EXCEPTIONS,"shadow: unravel failure",
			       dn_print, (caddr_t) dn);
			log_ds_error (&err);
			ds_error_free (&err);
			entry_free (ne);
			return;
		} else 	if ( ! check_oc_hierarchy(ne->e_oc)) {
			pslog (log_dsap,LLOG_EXCEPTIONS,"shadow: objectclass failure",
			       dn_print, (caddr_t) dn);
			entry_free (ne);
			return;

		} else if (check_schema (ne,NULLATTR,&err) != OK) {
			pslog (log_dsap,LLOG_EXCEPTIONS,"shadow: schema failure",
			       dn_print, (caddr_t) dn);
			log_ds_error (&err);
			ds_error_free (&err);
			entry_free (ne);
			return;
		}

                if (ne->e_parent == NULLENTRY) {
                        entry_replace(database_root, ne);
                } else {
                        entry_replace(eptr, ne);
                }

		entry_free (ne);
		ne = eptr;

                if (unravel_attribute(eptr, &err) != OK) {
			log_ds_error (&err);
			ds_error_free (&err);
			return;
		}
		(void) avl_apply(eptr->e_children, inherit_link, 
				 (caddr_t) eptr, NOTOK, AVL_PREORDER);

		if (eptr->e_parent->e_edbversion)
			free (eptr->e_parent->e_edbversion);
		eptr->e_parent->e_edbversion = new_version();

#ifdef TURBO_INDEX
		/* add the new modified entry to the index */
		turbo_add2index(eptr);
#endif
#else
	if (unravel_attribute (ne,&err) != OK) {
		log_ds_error (&err);
		ds_error_free (&err);
		entry_free (ne);
		return;
	} else 	if ( ! check_oc_hierarchy(ne->e_oc)) {
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("shadow: objectclass failure"));
		entry_free (ne);
		return;

	} else if (check_schema (ne,NULLATTR,&err) != OK) {
		log_ds_error (&err);
		ds_error_free (&err);
		entry_free (ne);
		return;
	}

	/* slot into tree */
	if (ne->e_parent == NULLENTRY) {
		database_root = ne;
		entry_free (eptr);
	} else {
		entry_free (eptr);
		/* now alter all parent pointers */
		ne->e_parent->e_child = ne;
		for (eptr = ne->e_child; eptr!=NULLENTRY; eptr=eptr->e_sibling) {
			eptr->e_parent = ne;
			set_inheritance (eptr);
		}

	}
	
#endif

#ifdef TURBO_DISK
	if (turbo_write(ne) != OK)
		fatal (-33,"shadow rewrite failed - check database");
#else
	if (journal (ne) != OK)
		fatal (-33,"shadow rewrite failed - check database");
#endif
#ifndef NO_STATS
	pslog (log_stat,LLOG_TRACE,"Shadow update",dn_print, (caddr_t) on ->
	       on_resp.di_result.dr_res.dcr_dsres.res_rd.rdr_entry.ent_dn);
#endif

	ne->e_age = timenow;

}


