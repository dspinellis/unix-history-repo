/* ds_search.c - DSA search of the directory */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_search.c,v 7.8 91/02/22 09:38:59 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_search.c,v 7.8 91/02/22 09:38:59 mrose Interim $
 *
 *
 * $Log:	ds_search.c,v $
 * Revision 7.8  91/02/22  09:38:59  mrose
 * Interim 6.8
 * 
 * Revision 7.7  90/11/20  15:28:42  mrose
 * cjr
 * 
 * Revision 7.6  90/10/17  11:53:54  mrose
 * sync
 * 
 * Revision 7.5  90/07/09  14:45:51  mrose
 * sync
 * 
 * Revision 7.4  90/04/18  08:49:46  mrose
 * 6.2
 * 
 * Revision 7.3  90/03/15  11:18:51  mrose
 * quipu-sync
 * 
 * Revision 7.2  90/01/11  23:55:52  mrose
 * lint
 * 
 * Revision 7.1  89/12/19  16:20:21  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:16  mrose
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
#include "quipu/list.h"      /* to get LSR # defs */
#include "quipu/ds_search.h"
#include "config.h"
#ifdef TURBO_AVL
#include "quipu/turbo.h"
#endif
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern LLog * log_dsap;

#ifndef NO_STATS
extern LLog * log_stat;
extern int dn_print ();
static PS filter_ps;
#endif

EntryInfo *filterentry();
static EntryInfo *filterchildren();
static test_avs();
static apply_search();
static substr_search();
static aux_substr_search();
static check_filteritem_presrch ();
static check_filter_presrch ();
static check_filterop_presrch ();
static check_filteritem ();
static check_filter ();
static check_filterop ();
struct ds_search_task * st_done ();
static do_base ();

extern Entry database_root;
int size;
char qctx;
extern int search_level;
IFP approxfn();
IFP av_cmp_fn();
#ifdef TURBO_INDEX
static int	optimized_filter;
extern int	optimized_only;
#endif

#ifndef	NBBY
#define	NBBY	8
#endif
static int big_size = 0;
static int timelimit;
extern time_t time();
extern time_t timenow;
extern int admin_size;

Attr_Sequence eis_select ();
extern Attr_Sequence entry_find_type();

do_ds_search (arg, error, result, dnbind, target, local, refer, di_p, 
		dsp, quipu_ctx, tktime, entryonly)
    register struct ds_search_arg        *arg;
    struct ds_search_result     *result;
    struct DSError              *error;
    DN                          dnbind;
    DN                          target;
    struct ds_search_task       ** local,
			 	** refer;
    struct di_block		** di_p;
    char 			dsp;
    char 			quipu_ctx;
    time_t			tktime;
    char			entryonly;
{
extern time_t admin_time;
int ismanager = FALSE;
int retval;

	qctx = quipu_ctx;

	if ((timelimit = tktime) == (time_t) 0) {
	    register int    i;

	    for (i = NBBY * sizeof timelimit - 1; i > 0; i--)
		timelimit <<= 1, timelimit |= 1;
	}

	if (!dsp)
		ismanager = manager (dnbind);

	if (ismanager && big_size == 0) {
	    register int    i;

	    for (i = NBBY * sizeof big_size - 1; i > 0; i--)
		big_size <<= 1, big_size |= 1;
	}

	if (*local == NULL_ST) {
		DLOG (log_dsap,LLOG_TRACE,("ds_search"));

		if (!dsp)
			target = arg->sra_baseobject;

		/* Put local stuff straight into result structure (dangerous) */
		result->srr_correlated = TRUE;
		result->srr_un.srr_unit = (struct ds_search_unit *) calloc(1, sizeof(struct ds_search_unit));
		result->CSR_cr = NULLCONTINUATIONREF;

		*local = st_alloc();
		(*local)->st_baseobject = dn_cpy (target);
		if ((*local)->st_entryonly = entryonly)		/* assign */
			(*local)->st_subset = SRA_BASEOBJECT;
		else
			(*local)->st_subset = arg->sra_subset;
		(*local)->st_alias = NULLDN;
		(*local)->st_bind = NULLDN;
		(*local)->st_save = NULL_ST;

 		if (ismanager) {
 		    if (((*local)->st_size = arg->sra_common.ca_servicecontrol.svc_sizelimit) == SVC_NOSIZELIMIT)
 			(*local)->st_size = big_size;
 		} else if (((*local)->st_size = MIN(admin_size,arg->sra_common.ca_servicecontrol.svc_sizelimit)) == SVC_NOSIZELIMIT)
 			(*local)->st_size = admin_size;

		(*local)->st_next = NULL_ST;

		result->CSR_entries = NULLENTRYINFO;

#ifndef NO_STATS
		if ((filter_ps = ps_alloc(str_open)) == NULLPS) {
			st_comp_free (*local);
			*local = NULL_ST;
			return (DS_ERROR_LOCAL);
		} if (str_setup (filter_ps,NULLCP, BUFSIZ, 0) == NOTOK) {
			st_comp_free (*local);
			*local = NULL_ST;
			return (DS_ERROR_LOCAL);
		}
#endif
#ifdef TURBO_INDEX
		optimized_filter = TRUE;
#endif

		if (arg->sra_filter == NULLFILTER) {
			/* set the default */
			arg->sra_filter = filter_alloc ();
			arg->sra_filter->flt_next = NULLFILTER;
			arg->sra_filter->flt_type = FILTER_AND;
			arg->sra_filter->FUFILT = NULLFILTER;	
		}

		if (check_filter_presrch (arg->sra_filter,error,target) != OK) {
#ifndef NO_STATS
			ps_free (filter_ps);
#endif
			st_comp_free (*local);
			*local = NULL_ST;
			return (DS_ERROR_REMOTE);
		} else {
			Entry entryptr;
#ifndef NO_STATS
			*filter_ps->ps_ptr = 0;
			switch ((*local)->st_subset) {
			case SRA_ONELEVEL:
				LLOG (log_stat, LLOG_TRACE, ("Search onelevel %s",filter_ps->ps_base));
				break;
			case SRA_WHOLESUBTREE:	
				LLOG (log_stat, LLOG_TRACE, ("Search subtree %s",filter_ps->ps_base));
				break;
			default:	
				LLOG (log_stat, LLOG_TRACE, ("Search base %s",filter_ps->ps_base));
				break;
			}
			ps_free (filter_ps);
#endif

#ifdef TURBO_INDEX
			if ((! optimized_filter) && optimized_only) {
                                LLOG(log_dsap, LLOG_EXCEPTIONS, ("Non-optimized filter not allowed"));
                                error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
                                *local = NULL_ST;
                                return( DS_ERROR_REMOTE );
                        }
                        (*local)->st_optimized = optimized_filter;
#endif

			if ((arg->sra_subset == SRA_ONELEVEL) || 
				(arg->sra_subset == SRA_WHOLESUBTREE))
			{
				switch(find_child_entry((*local)->st_baseobject,&(arg->sra_common),dnbind,NULLDNSEQ,FALSE,&(entryptr), error, di_p))
				{
				case DS_OK:
				    /* Filled out entryptr - carry on */
				    break;
				case DS_CONTINUE:
				    /* Filled out di_p - what do we do with it ?? */
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_CONTINUE);

				case DS_X500_ERROR:
				    /* Filled out error - what do we do with it ?? */
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_X500_ERROR);
				default:
				    /* SCREAM */
				    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_search() - find_child_entry failed 1"));
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_ERROR_LOCAL);
				}
			}
			else
			{
				if ((*local)->st_baseobject == NULLDN) {
					error->dse_type = DSE_NAMEERROR;
					error->ERR_NAME.DSE_na_problem = DSE_NA_NOSUCHOBJECT;
					error->ERR_NAME.DSE_na_matched = NULLDN;
					st_comp_free (*local);
					*local = NULL_ST;
					return (DS_ERROR_REMOTE);
		 		}
				 
				switch(find_entry((*local)->st_baseobject,&(arg->sra_common),dnbind,NULLDNSEQ,FALSE,&(entryptr), error, di_p,OP_SEARCH))
				{
				case DS_OK:
				    /* Filled out entryptr - carry on */
				    break;
				case DS_CONTINUE:
				    /* Filled out di_p - what do we do with it ?? */
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_CONTINUE);

				case DS_X500_ERROR:
				    /* Filled out error - what do we do with it ?? */
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_X500_ERROR);
				default:
				    /* SCREAM */
				    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_search() - find_entry failed 1"));
				    st_comp_free (*local);
				    *local = NULL_ST;
				    return(DS_ERROR_LOCAL);
				}
			}

			/* if no error and NOT SVC_OPT_DONTDEREFERENCEALIASES then */
			/* the alias will have been derefeferenced -signified by   */
			/* NO_ERROR !!! */
			if (error->dse_type == DSE_NOERROR) {
				result->CSR_object = NULLDN;
				result->CSR_common.cr_aliasdereferenced =  FALSE;
			} else {
				result->CSR_common.cr_aliasdereferenced =  TRUE;
				result->CSR_object = get_copy_dn (entryptr);
			}

			/* Strong authentication  */
			if ((retval = check_security_parms((caddr_t) arg,
				_ZSearchArgumentDataDAS,
				&_ZDAS_mod,
				arg->sra_common.ca_security,
				arg->sra_common.ca_sig, &dnbind)) != 0)
			{
				error->dse_type = DSE_SECURITYERROR;
				error->ERR_SECURITY.DSE_sc_problem = retval;
				st_comp_free (*local);
				*local = NULL_ST;
				return (DS_ERROR_REMOTE);
			}

			/* Do we have the entire subtree, if so allow */
			/* authenticated search iff dap		      */

			if (!dsp && (entryptr->e_allchildrenpresent == 2))
				(*local)->st_bind = dnbind;
			
			/* one final check - will we allow such searched in this DSA ? */
			if (arg->sra_subset == SRA_WHOLESUBTREE) {
				DN dn;
				int x = 0;
				for (dn = (*local)->st_baseobject; dn!= NULLDN; dn=dn->dn_parent, x++)
					;
				if ( x < search_level ) {
					if ( ! ismanager ) {
						/* Too high */
						error->dse_type = DSE_SERVICEERROR;
						error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
						st_comp_free (*local);
						*local = NULL_ST;
						return (DS_ERROR_REMOTE);
					}
				}
				if (entryptr->e_data != E_TYPE_CONSTRUCTOR) {
					if ((*local)->st_baseobject != NULLDN) {
						if ((result->CSR_entries = filterentry (arg,entryptr,(*local)->st_bind)) != NULLENTRYINFO)
							(*local)->st_size--;
					}
				} else {
					do_base (entryptr,local);
				}
			}

			result->CSR_limitproblem = LSR_NOLIMITPROBLEM;
			return (DS_SUSPEND); /* yup - we will take the search on */
		}
	} else {

		DLOG (log_dsap,LLOG_TRACE,("ds_search continuing"));

		size = (*local)->st_size;

		if (apply_search (arg,error,result,local,refer,ismanager) == NOTOK) {
			st_free (local);
			st_free (refer);
			return (DS_ERROR_REMOTE); 
		}

		if (size < 0) {
		    st_free (local);
		    st_free (refer);
		    result -> CSR_limitproblem =
			arg -> sra_common.ca_servicecontrol.svc_sizelimit
				== SVC_NOSIZELIMIT
			    || arg -> sra_common.ca_servicecontrol.svc_sizelimit
				    > admin_size
			? LSR_ADMINSIZEEXCEEDED
			: LSR_SIZELIMITEXCEEDED;
		    /* should fill out a POQ */
		    return (DS_OK);
		}

		if (timelimit <= timenow) {
		    st_free (local);
		    st_free (refer);
		    result -> CSR_limitproblem =
			arg -> sra_common.ca_servicecontrol.svc_timelimit
				== SVC_NOTIMELIMIT
			    || arg -> sra_common.ca_servicecontrol.svc_timelimit
				    > admin_time
			? LSR_ADMINSIZEEXCEEDED
			: LSR_TIMELIMITEXCEEDED;
		    /* should fill out a POQ */
		    return (DS_OK);
		}

		if ((*local)->st_next == NULL_ST) {
			st_free (local);
			result->CSR_limitproblem = LSR_NOLIMITPROBLEM;
			(void) dsa_search_control(arg,result);
			return (DS_OK);
		}

		(*local) = st_done(local);
		(*local)->st_size = size;
		return (DS_SUSPEND);
	}

}

/* 
 * SEARCH TASK HANDLING 
 */

st_comp_free (st)
struct ds_search_task *st;
{
	dn_free (st->st_baseobject);
	dn_free (st->st_alias);
	if (st->st_save != NULL_ST)
		st_free (&st->st_save);
	free ((char *)st);
}

st_free (st)
struct ds_search_task **st;
{
struct ds_search_task *next;

	for (; (*st) != NULL_ST; (*st) = next) {
		next = (*st)->st_next;
		st_comp_free (*st);
	}
}

struct ds_search_task * st_done (st)
struct ds_search_task **st;
{
struct ds_search_task *next;

	if ((next = (*st)->st_next) == NULL_ST)
		return NULL_ST;
	next->st_save = (*st);
	(*st)->st_next = (*st)->st_save;
	(*st)->st_save = NULL_ST;
	return (next);
}



/*
 * CHECK FILTER BEFORE SEARCHING 
 */


static check_filter_presrch (fltr,error,dn)
    register Filter  fltr;
    struct DSError *error;
    DN dn;
{
	DLOG (log_dsap,LLOG_DEBUG,("in check filter aux"));

	switch (fltr->flt_type) {
	    case FILTER_ITEM:
		return (check_filteritem_presrch (&fltr->FUITEM,error,dn));
	    case FILTER_AND:
#ifndef NO_STATS
		ps_print (filter_ps,"& ");
#endif
		return(check_filterop_presrch (fltr->FUFILT,error,dn));
	    case FILTER_OR:
#ifndef NO_STATS
		ps_print (filter_ps,"| ");
#endif
		return(check_filterop_presrch (fltr->FUFILT,error,dn));
	    case FILTER_NOT:
#ifndef NO_STATS
		ps_print (filter_ps,"! ");
#endif
#ifdef TURBO_INDEX
		optimized_filter = FALSE;
#endif
		return(check_filter_presrch (fltr->FUFILT,error,dn));
	    default:
		LLOG (log_dsap,LLOG_EXCEPTIONS,("check_filter protocol error"));
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		return (NOTOK);
		}
	/* NOTREACHED */
}

static check_filterop_presrch (fltr,error,dn)
    register Filter  fltr;
    struct DSError * error;
    DN dn;
{
register Filter ptr;
int i;

#ifndef NO_STATS
		ps_print (filter_ps,"(");
#endif
#ifdef TURBO_INDEX
	if (fltr == NULLFILTER)
		optimized_filter = FALSE;
#endif
	DLOG (log_dsap,LLOG_DEBUG,("in filter op aux"));
	for (ptr=fltr; ptr!=NULLFILTER ; ptr=ptr->flt_next) {
		i = check_filter_presrch (ptr,error,dn);
		if (i != OK)
			return (NOTOK);
	}
#ifndef NO_STATS
		ps_print (filter_ps,")");
#endif
	return (OK);

}

static prepare_string (c)
caddr_t c;
{
register char * p;

	for (p = (char *) c; *p ; p++ )
		*p = chrcnv[*p];
}

static check_filteritem_presrch (fitem,error,dn)
    register struct filter_item *fitem;
    struct DSError * error;
    DN	dn;
{
int av_acl, av_update, av_schema, av_syntax;
extern char chrcnv[];
extern char nochrcnv[];

	DLOG (log_dsap,LLOG_DEBUG,("search: check filter item aux"));
	if (fitem == NULLFITEM) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("check_filter_item protocol error (1)"));
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		return (NOTOK);
	}

	switch ( fitem->fi_type) {
	    case FILTERITEM_APPROX:
		if (fitem->UNAVA.ava_type == NULLTABLE_ATTR)
			return (invalid_matching (fitem->UNAVA.ava_type,error,dn));

		if ( (fitem->fi_ifp = approxfn (fitem->UNAVA.ava_type->oa_syntax)) == NULLIFP)
				/* approx not suported for this type */
				/* so set it to equality */
			fitem->fi_type = FILTERITEM_EQUALITY;
				/* NO break - check equality is OK */
		else
			prepare_string (fitem->UNAVA.ava_value->av_struct);

	    case FILTERITEM_GREATEROREQUAL:
	    case FILTERITEM_LESSOREQUAL:
#ifdef TURBO_INDEX
		if (fitem->fi_type == FILTERITEM_GREATEROREQUAL
		    || fitem->fi_type == FILTERITEM_LESSOREQUAL)
			optimized_filter = FALSE;
		/* fall through */
#endif
	    case FILTERITEM_EQUALITY:
		if (fitem->UNAVA.ava_type == NULLTABLE_ATTR)
			return (invalid_matching (fitem->UNAVA.ava_type,error,dn));

		if (fitem->fi_type != FILTERITEM_APPROX)
			if ( (fitem->fi_ifp = av_cmp_fn (fitem->UNAVA.ava_type->oa_syntax)) == NULLIFP)
				return (invalid_matching (fitem->UNAVA.ava_type,error,dn));

		av_acl = str2syntax ("acl");
		av_schema = str2syntax ("schema");
		av_update = str2syntax ("edbinfo");
		av_syntax = fitem->UNAVA.ava_type->oa_syntax;

		if (( av_syntax == av_acl ) 
			|| (av_syntax == av_schema) 
			|| (av_syntax == av_update))
				return (invalid_matching (fitem->UNAVA.ava_type,error,dn));

#ifdef TURBO_INDEX
		if (turbo_isoptimized(fitem->UNAVA.ava_type) == 0)
			optimized_filter = FALSE;
#endif
		break;
	    case FILTERITEM_SUBSTRINGS:
		if (fitem->UNSUB.fi_sub_type == NULLTABLE_ATTR)
			return (invalid_matching (fitem->UNSUB.fi_sub_type,error,dn));

		av_syntax = fitem->UNSUB.fi_sub_type->oa_syntax;

		if (! sub_string(av_syntax))
			return (invalid_matching (fitem->UNSUB.fi_sub_type,error,dn));

		if ( case_exact_match (av_syntax) ) 
			fitem->UNSUB.fi_sub_match = &nochrcnv[0];
		else {
			AV_Sequence loopavs;
			fitem->UNSUB.fi_sub_match = &chrcnv[0];
			if (fitem->UNSUB.fi_sub_initial != NULLAV) 
				prepare_string (fitem->UNSUB.fi_sub_initial->avseq_av.av_struct);
			for (loopavs=fitem->UNSUB.fi_sub_any; loopavs!=NULLAV; loopavs=loopavs->avseq_next)
				prepare_string (loopavs->avseq_av.av_struct);
			if (fitem->UNSUB.fi_sub_final != NULLAV) 
				prepare_string (fitem->UNSUB.fi_sub_final->avseq_av.av_struct);
		}
#ifdef TURBO_INDEX
		if (fitem->UNSUB.fi_sub_initial == NULLAV ||
		    turbo_isoptimized(fitem->UNSUB.fi_sub_type) == 0) {
			optimized_filter = FALSE;
		}
		break;
#endif
	    case FILTERITEM_PRESENT:
#ifdef TURBO_INDEX
		if (turbo_isoptimized(fitem->UNAVA.ava_type) == 0)
			optimized_filter = FALSE;
#endif
		break;
	    default:
		LLOG (log_dsap,LLOG_EXCEPTIONS,("check_filter_item protocol error (2)"));
		error->dse_type = DSE_SERVICEERROR;
		error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
		return (NOTOK);
	}

#ifndef NO_STATS
	ps_print (filter_ps,"(");
	switch ( fitem->fi_type) {
	    case FILTERITEM_APPROX:
		AttrT_print (filter_ps,fitem->UNAVA.ava_type,EDBOUT);
		ps_print (filter_ps,"~=");
		AttrV_print (filter_ps,fitem->UNAVA.ava_value,EDBOUT);
		break;
	    case FILTERITEM_EQUALITY:
		AttrT_print (filter_ps,fitem->UNAVA.ava_type,EDBOUT);
		ps_print (filter_ps,"=");
		AttrV_print (filter_ps,fitem->UNAVA.ava_value,EDBOUT);
		break;
	    case FILTERITEM_GREATEROREQUAL:
		AttrT_print (filter_ps,fitem->UNAVA.ava_type,EDBOUT);
		ps_print (filter_ps,">=");
		AttrV_print (filter_ps,fitem->UNAVA.ava_value,EDBOUT);
		break;
	    case FILTERITEM_LESSOREQUAL:
		AttrT_print (filter_ps,fitem->UNAVA.ava_type,EDBOUT);
		ps_print (filter_ps,"<=");
		AttrV_print (filter_ps,fitem->UNAVA.ava_value,EDBOUT);
		break;
	    case FILTERITEM_SUBSTRINGS:
		AttrT_print (filter_ps,fitem->UNSUB.fi_sub_type,EDBOUT);
		ps_print (filter_ps,"=");
		avs_print_aux (filter_ps,fitem->UNSUB.fi_sub_initial,EDBOUT,"*");
		ps_print (filter_ps,"*");
		avs_print_aux (filter_ps,fitem->UNSUB.fi_sub_any,EDBOUT,"*");
		ps_print (filter_ps,"*");
		avs_print_aux (filter_ps,fitem->UNSUB.fi_sub_final,EDBOUT,"*");
		break;
	    case FILTERITEM_PRESENT:
		AttrT_print (filter_ps,fitem->UNTYPE,EDBOUT);
		ps_print (filter_ps,"=*");
		break;
	}
	ps_print (filter_ps,")");
#endif
	return (OK);
}

/* APPLY SEARCH TO ONE LEVEL */

static apply_search (arg,error,result,local,refer,ismanager)
    struct ds_search_arg	*arg;
    struct DSError              *error;
    struct ds_search_result     *result;
    struct ds_search_task	**local,
			        **refer;
    int	ismanager;
{
Entry entryptr;
EntryInfo  *einfo = NULLENTRYINFO;
struct di_block	* di_tmp;

	if ((*local)->st_subset == SRA_BASEOBJECT)
	{
		if ((*local)->st_baseobject == NULLDN) {
			LLOG (log_dsap,LLOG_NOTICE,("NULL Base in search ignored"));
				/* to stop poisoning... */
			return (DS_OK);
		}
		switch(find_entry((*local)->st_baseobject,&(arg->sra_common),NULLDN,NULLDNSEQ,FALSE,&(entryptr), error, &(di_tmp),OP_SEARCH))
		{
		case DS_OK:
		    /* Filled out entryptr - carry on */
		    break;
		case DS_CONTINUE:
		    /* Filled out di_p - what do we do with it ?? */
		    subtask_refer(arg, local, refer, ismanager, di_tmp);
		    return(DS_OK);

		case DS_X500_ERROR:
		    /* Filled out error - what do we do with it ?? */
		    /* The only problem can be alias error etc */
		    /* to stop poisoning return OK */	
		    log_ds_error (error);
		    ds_error_free (error);
		    return (DS_OK);
		default:
		    /* SCREAM */
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_search() - find_entry failed 2"));
		    return(DS_ERROR_LOCAL);
		}
	}
	else
	{
		switch(find_child_entry((*local)->st_baseobject,&(arg->sra_common),NULLDN,NULLDNSEQ,FALSE,&(entryptr), error, &(di_tmp)))
		{
		case DS_OK:
		    /* Filled out entryptr - carry on */
		    break;
		case DS_CONTINUE:
		    /* Filled out di_p - what do we do with it ?? */
		    subtask_refer(arg, local, refer, ismanager, di_tmp);
		    return(DS_OK);

		case DS_X500_ERROR:
		    /* Filled out error - what do we do with it ?? */
		    /* The only problem can be alias error etc */
		    /* to stop poisoning return OK */	
		    log_ds_error (error);
		    return (DS_OK);
		default:
		    /* SCREAM */
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_search() - find_child_entry failed 2"));
		    return(DS_ERROR_LOCAL);
		}

		if ((*local)->st_subset == SRA_WHOLESUBTREE) {
		 if (entryptr->e_data != E_TYPE_CONSTRUCTOR) {
		   if ((*local)->st_alias) {
			if ((einfo = filterentry (arg,entryptr,(*local)->st_bind)) != NULLENTRYINFO) {
				(*local)->st_size--;
				if (result->CSR_entries == NULLENTRYINFO)
					result->CSR_entries = einfo;
				else
					entryinfo_merge (result->CSR_entries,einfo);
			}
		   }
		 } else {
			do_base (entryptr,local);
		 }
		}
	}

	switch ((*local)->st_subset) {
		case SRA_BASEOBJECT:
			einfo = filterentry (arg,entryptr,(*local)->st_bind);
			break;
		case SRA_ONELEVEL:
		case SRA_WHOLESUBTREE:	
			einfo = filterchildren (arg,entryptr,local,refer,ismanager);
			break;
		default:
			LLOG (log_dsap,LLOG_EXCEPTIONS,("search protocol error"));
			error->dse_type = DSE_SERVICEERROR;
			error->ERR_SERVICE.DSE_sv_problem = DSE_SV_UNWILLINGTOPERFORM;
			return (DS_X500_ERROR);
		}

	if (einfo != NULLENTRYINFO)
		if (result->CSR_entries == NULLENTRYINFO)
			result->CSR_entries = einfo;
		else
			entryinfo_merge (result->CSR_entries,einfo);

	result->CSR_common.cr_requestor = NULLDN;
	return (DS_OK);
}

/* 
 * SEARCH CHILDREN
 */

#ifdef TURBO_AVL

/*
 * search_kid2 - called from search_kid via avl_apply to apply
 * a filter to each entry in a tree of sibling entries without
 * looking at levels below.
 */

static search_kid2(e, ska)
Entry                   e;
struct search_kid_arg   *ska;
{
        struct ds_search_task   *new_task;
        EntryInfo               *eptr;

        if (size < 0)
                return(NOTOK);

        if ((e->e_alias != NULLDN)
	    && (do_alias(ska->ska_arg, e, ska->ska_local) == OK))
                return(OK);

        eptr = filterentry(ska->ska_arg, e, (*ska->ska_local)->st_bind);

        if (eptr != NULLENTRYINFO && size != -1)
                if (*ska->ska_einfo == NULLENTRYINFO)
                        *ska->ska_einfo = eptr;
                else
                        entryinfo_merge(*ska->ska_einfo, eptr);

	if (! isleaf(e)) {
		new_task = st_alloc();
		new_task->st_save = NULL_ST;
		new_task->st_baseobject = get_copy_dn(e);
		new_task->st_size = 0;
		new_task->st_alias = NULLDN;
		new_task->st_bind = (*ska->ska_local)->st_bind;
		new_task->st_subset = SRA_WHOLESUBTREE;
#ifdef TURBO_INDEX
		new_task->st_optimized = (*ska->ska_local)->st_optimized;
#endif
		new_task->st_next = (*ska->ska_local)->st_next;
		new_task->st_entryonly = FALSE;
		(*ska->ska_local)->st_next = new_task;
	}

	if (ska->ska_tmp > SEARCH_DELTA_SIZE) {
		if (timelimit <= (timenow = time((time_t *) 0)))
			return(NOTOK);
		ska->ska_tmp = 0;
		ska->ska_domore = FALSE;
	}
	ska->ska_tmp++;

	return(OK);
}

/*
 * search_kid - called from filterchildren via avl_apply to apply
 * a filter to each entry in a tree of sibling entries, looking also
 * at one level below.
 */

static search_kid(e, ska)
Entry                   e;
struct search_kid_arg   *ska;
{
        struct ds_search_task   *new_task;
        EntryInfo               *eptr;

        if (size < 0)
                return(NOTOK);

        if ((e->e_alias != NULLDN)
	    && (do_alias(ska->ska_arg, e, ska->ska_local) == OK))
                return(OK);

        eptr = filterentry(ska->ska_arg, e, (*ska->ska_local)->st_bind);

        if (eptr != NULLENTRYINFO && size != -1)
                if (*ska->ska_einfo == NULLENTRYINFO)
                        *ska->ska_einfo = eptr;
                else
                        entryinfo_merge(*ska->ska_einfo, eptr);

	if (ska->ska_tmp > SEARCH_DELTA_SIZE) {
		if (timelimit <= (timenow = time((time_t *) 0)))
			return(NOTOK);
		ska->ska_tmp = 0;
		ska->ska_domore = FALSE;
	}
	ska->ska_tmp++;

        if ((*ska->ska_local)->st_subset == SRA_WHOLESUBTREE && (!isleaf(e))) {
                if ( check_acl( (*ska->ska_local)->st_bind, ACL_READ,
		    e->e_acl->ac_child, NULLDN ) == OK ) {
                        if ( ((e->e_children != NULLAVL)
                            && (e->e_allchildrenpresent == FALSE))
                            || (e->e_children == NULLAVL)) {
                                search_refer( ska->ska_arg, e, ska->ska_local,
                                    ska->ska_refer, ska->ska_ismanager);
/*
                        } else if (ska->ska_domore) {
*/
			} else {
				/*
				 * let other connections progress every
				 * SEARCH_DELTA_SIZE entries searched.
				 */
				if (! ska->ska_domore)
					dsa_wait(0);

				ska->ska_tmp = 0;
				ska->ska_domore = TRUE;
				(void) avl_apply(e->e_children, search_kid2,
				    (caddr_t) ska, NOTOK, AVL_INORDER);

				if (timelimit <= (timenow = time((time_t *)0)))
					return(NOTOK);

				if (ska->ska_domore)
					ska->ska_domore = (ska->ska_tmp <
					    (SEARCH_DELTA_SIZE / 5));
				if (! ska->ska_domore)
					dsa_wait(0);
			}
/*
It doesn't seem to make much sense to do this here with the avl's...
unless there's some reason calling dsa_wait() won't do the job.
			} else {
                                new_task = st_alloc();
                                new_task->st_save = NULL_ST;
                                new_task->st_baseobject = get_copy_dn(e);
                                new_task->st_size = 0;
                                new_task->st_alias = NULLDN;
				new_task->st_bind = (*ska->ska_local)->st_bind;
                                new_task->st_subset = SRA_WHOLESUBTREE;
#ifdef TURBO_INDEX
                                new_task->st_optimized =
                                    (*ska->ska_local)->st_optimized;
#endif
                                new_task->st_next = (*ska->ska_local)->st_next;
				new_task->st_entryonly = FALSE;
                                (*ska->ska_local)->st_next = new_task;
                        }
*/
                }
        }

	return(OK);
}

#endif /* TURBO_AVL */

static EntryInfo * filterchildren (arg,entryptr,local,refer,ismanager)
    struct ds_search_arg        *arg;
    Entry  entryptr;
    struct ds_search_task	**local,
			        **refer;
    int	   ismanager;
{
EntryInfo  *einfo = NULLENTRYINFO;
register int tmp = 0;
char     domore = TRUE;
#ifdef TURBO_AVL
Avlnode	*ptr;
struct search_kid_arg ska;
#ifdef TURBO_INDEX
extern Avlnode	*subtree_index;
extern Avlnode	*sibling_index;
int idn_cmp();
#endif
#else
EntryInfo  *eptr = NULLENTRYINFO;
register Entry cptr;
struct ds_search_task	* new_task;
register Entry ptr;
#endif

	DLOG (log_dsap,LLOG_DEBUG,("search: filter children"));

	if (entryptr == NULLENTRY)
		return (NULLENTRYINFO);

	if (isleaf(entryptr))
		return (NULLENTRYINFO);

	if (check_acl ((*local)->st_bind, ACL_READ, entryptr->e_acl->ac_child, (*local)->st_baseobject) == NOTOK) {
		return (NULLENTRYINFO);
	}

	if (entryptr->e_alias != NULLDN) {
		(void) do_alias (arg,entryptr,local);
		return (NULLENTRYINFO);
	}

#ifdef TURBO_AVL
	if ((ptr = entryptr->e_children) == NULLAVL
	    || entryptr->e_allchildrenpresent == FALSE) {
#else
	ptr = entryptr->e_child;
	if (((ptr != NULLENTRY) && (entryptr->e_allchildrenpresent == FALSE))
		|| (ptr == NULLENTRY)) {
#endif
		search_refer (arg,entryptr,local,refer,ismanager);
		return (NULLENTRYINFO);
	}

	/* search everything at this level */
#ifdef TURBO_AVL
        ska.ska_einfo = &einfo;
        ska.ska_arg = arg;
        ska.ska_local = local;
        ska.ska_refer = refer;
	ska.ska_tmp = tmp;
	ska.ska_domore = domore;
        ska.ska_ismanager = ismanager;

#ifdef TURBO_INDEX
	/* non optimized filter */
	if ((*local)->st_optimized == 0) {
		(void) avl_apply(ptr, search_kid, (caddr_t) &ska, NOTOK, AVL_INORDER);

	/* optimized filter & subtree search & subtree indexed */
	} else if (arg->sra_subset == SRA_WHOLESUBTREE
	    && get_subtree_index((*local)->st_baseobject)) {
		(void) turbo_subtree_search(entryptr, &ska);

	/* optimized filter & sibling search & siblings indexed */
	} else if (arg->sra_subset == SRA_ONELEVEL
	    && get_sibling_index((*local)->st_baseobject)) {
		(void) turbo_sibling_search(entryptr, &ska);

	/* optimized filter, but no index to search */
	} else {
		(void) avl_apply(ptr, search_kid, (caddr_t) &ska, NOTOK, AVL_INORDER);
	}
#else
		(void) avl_apply(ptr, search_kid, (caddr_t) &ska, NOTOK, AVL_INORDER);
#endif

	tmp = ska.ska_tmp;
	domore = ska.ska_domore;
#else
	for (tmp=0; (ptr != NULLENTRY) && (size >= 0) ; ptr=ptr->e_sibling,tmp++) {

		if ((ptr->e_alias != NULLDN) && 
			(do_alias (arg,ptr,local) == OK))
				continue;

		eptr = filterentry (arg,ptr,(*local)->st_bind);

		if ((eptr != NULLENTRYINFO) && (size != -1))
			if (einfo == NULLENTRYINFO)
				einfo =  eptr;
			else
				entryinfo_merge (einfo,eptr);

		if ( tmp > SEARCH_DELTA_SIZE ) {
			if (timelimit <= (timenow = time ((time_t *)0))) 
				return (einfo);
			tmp = 0;
			domore = FALSE;
		}
	}

	if (size < 0)
		return (einfo);

	if (domore)
		/* Heuristic - should tailor it eventually */
		domore = (tmp < (SEARCH_DELTA_SIZE / 5));

	if ((*local)->st_subset == SRA_WHOLESUBTREE) {
	    /* search below - or make pointers */
	    ptr=entryptr->e_child;

	    for (; (ptr != NULLENTRY) && (size >= 0) ; ptr=ptr->e_sibling) {

		if (isleaf(ptr))
			continue;

		if (domore) {
			/* search one more level */
			if (check_acl ((*local)->st_bind, ACL_READ, ptr->e_acl->ac_child, NULLDN) == NOTOK) 
				continue;
		
			cptr = ptr->e_child;

			if (((cptr != NULLENTRY) && (ptr->e_allchildrenpresent == FALSE))
				|| (cptr == NULLENTRY)) {
				search_refer (arg,ptr,local,refer,ismanager);
				continue;
				}

			/* search everything at this level */
			for (tmp=0; (cptr != NULLENTRY) && (size >= 0) ; cptr=cptr->e_sibling,tmp++) {


				if ((cptr->e_alias != NULLDN) && (do_alias (arg,cptr,local) == OK))
					continue;

				eptr = filterentry (arg,cptr,(*local)->st_bind);

				if ((eptr != NULLENTRYINFO) && (size != -1))
					if (einfo == NULLENTRYINFO)
						einfo =  eptr;
					else
						entryinfo_merge (einfo,eptr);

				if ( ! isleaf(cptr)) {
					new_task = st_alloc();
					new_task->st_save = NULL_ST;
					new_task->st_baseobject = get_copy_dn (cptr);
					new_task->st_size = 0;	/* fill in later */
					new_task->st_alias = NULLDN;
					new_task->st_bind = (*local)->st_bind;
					new_task->st_subset = SRA_WHOLESUBTREE;
					new_task->st_next = (*local)->st_next;
					new_task->st_entryonly = FALSE;
					(*local)->st_next = new_task;
				}

				if ( tmp > SEARCH_DELTA_SIZE ) {
					if (timelimit <= (timenow = time ((time_t *)0))) 
						return (einfo);
					tmp = 0;
				}
			}
			if (timelimit <= (timenow = time ((time_t *)0))) 
				return (einfo);

			dsa_wait (0);	/* progress any other connections */
		} else {
			new_task = st_alloc();
			new_task->st_save = NULL_ST;
			new_task->st_baseobject = get_copy_dn (ptr);
			new_task->st_size = 0;	/* fill in later */
			new_task->st_alias = NULLDN;
			new_task->st_bind = (*local)->st_bind;
			new_task->st_subset = SRA_WHOLESUBTREE;
			new_task->st_next = (*local)->st_next;
			new_task->st_entryonly = FALSE;
			(*local)->st_next = new_task;
		}
	    }
	}
#endif /* TURBO_AVL */

	return (einfo);
}

/* 
 * HANDLE ALIASES AND REFERRALS
 */

do_alias (arg,eptr,local)
    struct ds_search_arg        *arg;
    Entry eptr;
    struct ds_search_task	**local;
{
struct ds_search_task *new_task;
struct ds_search_task *st;
DN st_dn;

	if ( ! arg->sra_searchaliases) 
		return NOTOK;

	DLOG (log_dsap,LLOG_DEBUG,("alias in search path"));

	/* Check we have not been here before... */
	for ( st = (*local)->st_save; st != NULL_ST; st=st->st_next) {
		if (st->st_alias == NULLDN) 
			st_dn = st->st_baseobject;
		else 
			st_dn = st->st_alias;
		if (dn_cmp (eptr->e_alias, st_dn) == 0) {
		        LLOG (log_dsap,LLOG_TRACE,("local search - loop detected"));
			return OK;
		}
	}

	new_task = st_alloc();
	new_task->st_save = NULL_ST;
	new_task->st_baseobject = get_copy_dn (eptr);
	new_task->st_size = 0;	/* fill in later */
	new_task->st_alias = dn_cpy(eptr->e_alias);
	new_task->st_bind = (*local)->st_bind;
	new_task->st_entryonly = FALSE;

	switch ((*local)->st_subset) {
	case SRA_ONELEVEL:
		new_task->st_entryonly = TRUE;
		/* fall */
	case SRA_BASEOBJECT:
		new_task->st_subset = SRA_BASEOBJECT;
		break;
	case SRA_WHOLESUBTREE:
		new_task->st_subset = SRA_WHOLESUBTREE;
		break;
	}

	new_task->st_next = (*local)->st_next;
	(*local)->st_next = new_task;

	return (OK);
}

static do_base (eptr,local)
    Entry eptr;
    struct ds_search_task	**local;
{
struct ds_search_task *new_task;

	DLOG (log_dsap,LLOG_DEBUG,("Making baseobject search"));

	new_task = st_alloc();
	new_task->st_save = NULL_ST;
	new_task->st_baseobject = get_copy_dn (eptr); 
	new_task->st_size = 0;	/* fill in later */
	new_task->st_alias = NULLDN;
	new_task->st_bind = (*local)->st_bind;
	new_task->st_entryonly = TRUE;		/* If is a subtree search we are breaking protocol here */
						/* BUT... There is no other way to do it !!! */
	new_task->st_subset = SRA_BASEOBJECT;
	new_task->st_next = (*local)->st_next;
	(*local)->st_next = new_task;
}

search_refer(arg,entryptr,local,refer,ismanager)
    struct ds_search_arg        *arg;
    Entry  entryptr;
    struct ds_search_task	**local,
			        **refer;
    int	    ismanager;
{
struct ds_search_task	* new_task;
struct DSError		  error;
struct di_block		* di_tmp;
DN name;	

	name = get_copy_dn (entryptr);

	switch(dsa_info_new(name, NULLDNSEQ, FALSE, entryptr, &(error), &(di_tmp)))
	{
	case DS_OK:
	    /* A di_block ready for use */
	    break;
	case DS_CONTINUE:
	    /* A deferred di_block */
	    break;
	case DS_X500_ERROR:
	    /* An error */
       	    pslog (log_dsap,LLOG_EXCEPTIONS,"search_refer failed",dn_print,name);
	    log_ds_error(&(error));
	    ds_error_free(&(error));
	    dn_free (name);
	    return;
	default:
	    /* A local error - scream */
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("search_refer - dsa_info_new() failed"));
	    dn_free (name);
	    return;
	}

	DLOG (log_dsap,LLOG_DEBUG,("referral in search path"));

	new_task = st_alloc();
	new_task->st_save = NULL_ST;
	new_task->st_baseobject = name;
	new_task->st_subset = (*local)->st_subset;
	new_task->st_alias = NULLDN;
	new_task->st_bind = NULLDN;
	new_task->st_entryonly = (*local)->st_entryonly;
	if (ismanager) {
	    if ((new_task->st_size = arg->sra_common.ca_servicecontrol.svc_sizelimit) == SVC_NOSIZELIMIT)
		new_task->st_size = big_size;
	}
	else
	    if ((new_task->st_size = MIN(admin_size,arg->sra_common.ca_servicecontrol.svc_sizelimit)) == SVC_NOSIZELIMIT)
		new_task->st_size = admin_size;

	new_task->st_di = di_tmp;
	new_task->st_next = *refer;
	*refer = new_task;
}

/*
 * SEARCH ENTRY
 */  

EntryInfo * filterentry (arg,entryptr,binddn)
    struct ds_search_arg        *arg;
    register Entry entryptr;
    DN binddn;
{
register EntryInfo * einfo;

	DLOG (log_dsap,LLOG_DEBUG,("search: filter entry"));

	if (check_filter (arg->sra_filter,entryptr,binddn) != OK ) {
		DLOG (log_dsap,LLOG_DEBUG,("none found"));
		return (NULLENTRYINFO);
	}

	if (check_acl (binddn, ACL_READ, entryptr->e_acl->ac_entry, NULLDN) == NOTOK) 
		return (NULLENTRYINFO);

	einfo = entryinfo_alloc ();
	einfo->ent_dn = get_copy_dn (entryptr);

	einfo->ent_attr = eis_select (arg->sra_eis, entryptr, binddn, qctx && arg->sra_eis.eis_allattributes,einfo->ent_dn);

	einfo->ent_iscopy = entryptr->e_data;
	einfo->ent_age = (time_t) 0;
	einfo->ent_next = NULLENTRYINFO;
	size--;
	return (einfo);
}

/* 
 * TEST FILTER AGAINST SINGLE ENTRY
 */

static check_filter (fltr,entryptr,binddn)
    register Filter  fltr;
    register Entry  entryptr;
    DN binddn;
{
register int i;

	DLOG (log_dsap,LLOG_DEBUG,("in check filter"));
	switch (fltr->flt_type) {
	    case FILTER_ITEM:
		return (check_filteritem (&fltr->FUITEM,entryptr,binddn));
	    case FILTER_AND:
	    case FILTER_OR:
		return(check_filterop (fltr->FUFILT,entryptr,fltr->flt_type,binddn));
	    case FILTER_NOT:
		if ((i=check_filter (fltr->FUFILT,entryptr,binddn)) == OK)
			return NOTOK;
		else if ( i == NOTOK )
			return OK;
		else
			return i;
		}
	/* NOTREACHED */
}

static check_filterop (fltr,entryptr,op,binddn)
    register Filter  fltr;
    register Entry  entryptr;
    int op;
    DN  binddn;
{
register Filter ptr;
int result;

	DLOG (log_dsap,LLOG_DEBUG,("in filter op"));

	/* effect of applying logical operator to zero operands */
	if (op == FILTER_OR)
		result = NOTOK;
	else
		result = OK;

	for (ptr=fltr; ptr!=NULLFILTER ; ptr=ptr->flt_next)
		switch (check_filter (ptr,entryptr,binddn)) {
			case MAYBE:
/* Beware of 'Pathological NOT' here.
 * To comply with the December '88 X.500, should just drop through here.
 * For the security to work properly, also set result to MAYBE.
 */
				result = MAYBE;
				break;
			case OK:
				if (op == FILTER_OR) {
					DLOG (log_dsap,LLOG_DEBUG,("or ok"));
					return (OK);
				}
				break;
			case NOTOK:
				if (op == FILTER_AND) {
					DLOG (log_dsap,LLOG_DEBUG,("and not"));
					return (NOTOK);
				}
				break;
			case -2:
			default:
				return (-2);
		}


	return (result);
}

/* 
 * CHECK FILTER ITEM AGAINST ENTRY 
 */

static check_filteritem (fitem,entryptr,binddn)
    register struct filter_item *fitem;
    register Entry  entryptr;
    DN  binddn;
{
register Attr_Sequence as;
Attr_Sequence ias = NULLATTR;
AttributeType at;
Attr_Sequence ptr;
int i, res;

	DLOG (log_dsap,LLOG_DEBUG,("search: check filter item"));

	switch ( fitem->fi_type) {
	    case FILTERITEM_APPROX:
	    case FILTERITEM_EQUALITY:
	    case FILTERITEM_GREATEROREQUAL:
	    case FILTERITEM_LESSOREQUAL:
		at = fitem->UNAVA.ava_type;
		break;
	    case FILTERITEM_SUBSTRINGS:
		at = fitem->UNSUB.fi_sub_type;
		break;
	    case FILTERITEM_PRESENT:
		if ((as = entry_find_type (entryptr, fitem->UNTYPE)) == NULLATTR) 
			return NOTOK;
		else
			return OK;
	}

	if ((as = as_find_type (entryptr->e_attributes, at)) == NULLATTR) {
		if (entryptr->e_iattr) {
			for(ptr = entryptr->e_iattr->i_default; ptr != NULLATTR; ptr=ptr->attr_link) {
				if (  (i = AttrT_cmp (ptr->attr_type,at)) <= 0) {
					if ( i == 0 ) 
						as = ptr;
					break;
				}
			}
			if (as == NULLATTR)
			   for(ptr = entryptr->e_iattr->i_always; ptr != NULLATTR; ptr=ptr->attr_link) {
				if (  (i = AttrT_cmp (ptr->attr_type,at)) <= 0) {
					if ( i == 0 ) 
						as = ptr;
					break;
				}
			}
			if (as == NULLATTR)
				return MAYBE;
		} else
			return MAYBE;
	} else {
		/* see if there is an 'always' attribute */
		if (entryptr->e_iattr) {
			for(ptr = entryptr->e_iattr->i_always; ptr != NULLATTR; ptr=ptr->attr_link) {
				if (  (i = AttrT_cmp (ptr->attr_type,at)) <= 0) {
					if ( i == 0 ) 
						ias = ptr;
					break;
				}
			}
		}
		   
	}

	if ( check_acl (binddn,ACL_COMPARE,as->attr_acl,NULLDN) != OK)
		return MAYBE;  

	switch ( fitem->fi_type) {
	    case FILTERITEM_SUBSTRINGS:
		res = substr_search (fitem,as->attr_value);
		break;
	    case FILTERITEM_APPROX:
		res = (int)(*fitem->fi_ifp)(fitem,as->attr_value);
		break;
	    default:
		res = test_avs (fitem,as->attr_value,fitem->fi_type);
		break;
	}

	if ((res == OK) || (ias == NULLATTR))
		return res;

	if ( check_acl (binddn,ACL_COMPARE,ias->attr_acl,NULLDN) != OK)
		return MAYBE;  

	switch ( fitem->fi_type) {
	    case FILTERITEM_SUBSTRINGS:
		res = substr_search (fitem,ias->attr_value);
		break;
	    case FILTERITEM_APPROX:
		res = (int)(*fitem->fi_ifp)(fitem,ias->attr_value);
		break;
	    default:
		res = test_avs (fitem,ias->attr_value,fitem->fi_type);
		break;
	}

	return res;
}

static test_avs (fitem,avs,mode)
    register struct filter_item *fitem;
    register AV_Sequence avs;
    register int mode;
{

	for (; avs != NULLAV; avs=avs->avseq_next) {
		switch (((int)(*fitem->fi_ifp)(avs->avseq_av.av_struct, fitem->UNAVA.ava_value->av_struct))) {
			case 0:
				return (OK);
			case 1:
				if (mode == FILTERITEM_GREATEROREQUAL)
					return (OK);
				break;
			case -1:
				if (mode == FILTERITEM_LESSOREQUAL)
					return (OK);
				break;
			case 2:
				return (NOTOK);
			default:
				return (MAYBE);
		}
	}
	return (NOTOK);
}


/*
 * SUBSTRING MATCH 
 */

static substr_search (fitem,avs)
    register struct filter_item *fitem;
    register AV_Sequence avs;
{

	for (; avs != NULLAV; avs=avs->avseq_next)
		if (aux_substr_search (fitem,avs,fitem->UNSUB.fi_sub_match) == OK)
			return (OK);
	return (NOTOK);
}



static aux_substr_search (fitem,avs,chrmatch)
    struct filter_item *fitem;
    AV_Sequence avs;
    char chrmatch [];
{
register AV_Sequence loopavs;
register char * compstr;
char * top;
register char * temp;
char * temp2;
int offset;

	compstr = (char *)avs->avseq_av.av_struct;
	top  = compstr;
	if (fitem->UNSUB.fi_sub_initial != NULLAV) {
		temp = (char *)fitem->UNSUB.fi_sub_initial->avseq_av.av_struct;
		do
			if (chrmatch[*compstr++] != *temp++) {
				DLOG (log_dsap,LLOG_DEBUG,("initial failure (%s, %s)",top,(char *)fitem->UNSUB.fi_sub_initial->avseq_av.av_struct));
				return (NOTOK);
			}
		while (*temp != '\0') ;
	}

	for (loopavs=fitem->UNSUB.fi_sub_any; loopavs!=NULLAV; loopavs=loopavs->avseq_next, compstr += offset)
		if ((offset= attr_substr (compstr, &loopavs->avseq_av,chrmatch)) == -1) {
			DLOG (log_dsap,LLOG_DEBUG,("any failure (%s, %s)",top,(char *)loopavs->avseq_av.av_struct));
			return (NOTOK);
		}

	if (fitem->UNSUB.fi_sub_final != NULLAV) {
		temp = (char *)fitem->UNSUB.fi_sub_final->avseq_av.av_struct;
		temp2 = temp;
		while (*++compstr != '\0')
			;  /* NO-OP*/

		while (*temp++ != '\0')
			compstr--;

		if (compstr < top) {
			DLOG (log_dsap,LLOG_DEBUG,("final too long failure (%s,%s)",top,temp2));
			return (NOTOK);
		}

		temp = temp2;
		while (*compstr != '\0')
			if (chrmatch[*compstr++] != *temp++) {
				/* free (top); */
				DLOG (log_dsap,LLOG_DEBUG,("final failure (%s, %s)",top,temp2));
				return (NOTOK);
			}
	}
	return (OK);
}

attr_substr (str1,av,chrmatch)
register char * str1;
AttributeValue av;
char chrmatch[];
{
register char * str2;
register int count;
char * top, *top2;
char found;

    top = str1;
    top2 = str2 = (char *)av->av_struct;

    while (*str1 != '\0') {
	if (chrmatch[*str1++] == *str2) {
		str2++;
		found = 1;
		break;
	}
    }

    if ( found == 0 )
	return (-1);

    for (count = 1; *str2 ; count ++) {
	if (*str1 == '\0')
	    return (-1);

	if (chrmatch[*str1++] != *str2++) {
		/* not found here, but may still be in the string !! */
		str1 -= count;
		str2 = top2;
		while (*str1 != '\0') {
			if (chrmatch[*str1++] == *str2) {
				str2++;
				break;
			}
		}
		count = 0;  /* for loop ++ will make it 1 !!! */
	}
    }
    return (str1 - top);
}


subtask_refer(arg, local, refer, ismanager, di)
    struct ds_search_arg	*arg;
    struct ds_search_task	**local,
			        **refer;
    int	ismanager;
    struct di_block		* di;
{
	/* turn query into a referral */
	struct ds_search_task	* new_task;
		
	new_task = st_alloc();
	new_task->st_save = NULL_ST;
	new_task->st_baseobject = dn_cpy ((*local)->st_baseobject);
	new_task->st_subset = (*local)->st_subset;
	new_task->st_alias = dn_cpy ((*local)->st_alias);
	new_task->st_bind = NULLDN;
	if ((*local)->st_bind != NULLDN) {
		LLOG(log_dsap,LLOG_NOTICE,("Search consistency problem"));
		/* Doing a authenticed search, but need to go outside this DSA */
		/* probably aliases to blame... */
	}

	new_task->st_entryonly = (*local)->st_entryonly;

	if (ismanager) {
	    if ((new_task->st_size = arg->sra_common.ca_servicecontrol.svc_sizelimit) == SVC_NOSIZELIMIT)
		new_task->st_size = big_size;
	}
	else
	    if ((new_task->st_size = MIN(admin_size,arg->sra_common.ca_servicecontrol.svc_sizelimit)) == SVC_NOSIZELIMIT)
		new_task->st_size = admin_size;

	new_task->st_di = di;
	new_task->st_next = *refer;
	*refer = new_task;
}

dsa_search_control (arg,result)
    struct ds_search_arg          *arg;
    struct ds_search_result       *result;
{
extern DN mydsadn;
char buffer [LINESIZE];
Attr_Sequence as;
extern AttributeType at_control;
int i;

	if (big_size == 0)
	    for (i = NBBY * sizeof big_size - 1; i > 0; i--)
		big_size <<= 1, big_size |= 1;

	if ((arg->sra_eis.eis_allattributes) || 
		(arg->sra_eis.eis_infotypes == EIS_ATTRIBUTETYPESONLY))
		return FALSE;

	if (arg->sra_eis.eis_select == NULLATTR)
		return FALSE;

	if (arg->sra_eis.eis_select->attr_link != NULLATTR)
		return FALSE;

	if (AttrT_cmp (at_control,arg->sra_eis.eis_select->attr_type) != 0)
		return FALSE;

	if (result->CSR_entries)
		entryinfo_free (result->CSR_entries,0);

	(void) sprintf (buffer,"%d",big_size-size);

	as=as_comp_alloc();
	as->attr_acl = NULLACL_INFO;
	as->attr_type = at_control;
	as->attr_link = NULLATTR;
        if ((as->attr_value = str2avs (buffer,as->attr_type)) == NULLAV) {
                as_free (as);
		result->CSR_entries = NULLENTRYINFO;
                return FALSE;
	}

	result->CSR_entries = entryinfo_alloc ();
	result->CSR_entries->ent_dn = dn_cpy (mydsadn);
	result->CSR_entries->ent_next = NULLENTRYINFO;
	result->CSR_entries->ent_age = (time_t) 0;
	result->CSR_entries->ent_iscopy = TRUE;
	result->CSR_entries->ent_attr = as;

	return TRUE;
}
