/* ds_list.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_list.c,v 7.4 91/03/09 11:56:42 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_list.c,v 7.4 91/03/09 11:56:42 mrose Exp $
 *
 *
 * $Log:	ds_list.c,v $
 * Revision 7.4  91/03/09  11:56:42  mrose
 * update
 * 
 * Revision 7.3  91/02/22  09:38:50  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:53:44  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:45:43  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:09  mrose
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
#include "quipu/connection.h"
#include "quipu/list.h"
#ifdef TURBO_AVL
#include "quipu/turbo.h"
#endif
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern LLog * log_dsap;
extern Entry database_root;
static int build_result();

do_ds_list (arg, error, result, binddn, target, di_p, dsp)
    register struct ds_list_arg          *arg;
    register struct ds_list_result       *result;
    struct DSError                      *error;
    DN                                  binddn;
    DN                                  target;
    struct di_block			**di_p;
    char				dsp;
{
Entry  entryptr;
int retval;
DN realtarget;

	DLOG (log_dsap,LLOG_TRACE,("ds_list"));

	if (!dsp)
		target = arg->lsa_object;

	switch(find_child_entry(target,&(arg->lsa_common),binddn,NULLDNSEQ,FALSE,&(entryptr),error,di_p))
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
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("do_ds_list() - find_child_entry failed"));
	    return(DS_ERROR_LOCAL);
	}

	/* Strong authentication  */
	if ((retval = check_security_parms((caddr_t) arg,
			_ZListArgumentDataDAS,
			&_ZDAS_mod,
			arg->lsa_common.ca_security,
			arg->lsa_common.ca_sig, &binddn)) != 0)
	{
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = retval;
		return (DS_ERROR_REMOTE);
	}

	realtarget = get_copy_dn(entryptr);

	if (isleaf(entryptr)) {

#ifdef NOTANYMORE

		if ((entryptr->e_master != NULLAV) || (entryptr->e_slave != NULLAV)) {
			int res;
			/* PROBLEM: Might not list everything if user is entitled to more... */
			if (try_cache (arg,result,realtarget) == OK) {
				dn_free (realtarget);
				return (DS_OK);
			}
			res = constructor_dsa_info(realtarget,NULLDNSEQ,FALSE,entryptr,error,di_p);
			dn_free (realtarget);
			return res;
		}
#endif

		dn_free (realtarget);

		result->lsr_subordinates = NULLSUBORD;
		result->lsr_age  =  (time_t) 0 ;
		result->lsr_common.cr_requestor = NULLDN;
		if ( error->dse_type == DSE_NOERROR ) {
			result->lsr_object = NULLDN;
			result->lsr_common.cr_aliasdereferenced = FALSE;
		} else {
			result->lsr_common.cr_aliasdereferenced = TRUE;
			result->lsr_object = get_copy_dn (entryptr->e_parent);
		}
		result->lsr_cr = NULLCONTINUATIONREF;
		result->lsr_limitproblem = LSR_NOLIMITPROBLEM;
		return (DS_OK);
	}

	/* check parent will allow listing */
	if (check_acl (dsp ? NULLDN : binddn,ACL_READ, entryptr->e_acl->ac_child, realtarget) != OK) {
		if (dsp && (check_acl (binddn,ACL_READ, entryptr->e_acl->ac_child, realtarget) == OK)) {
			error->dse_type = DSE_SECURITYERROR;
			error->ERR_SECURITY.DSE_sc_problem = DSE_SC_AUTHENTICATION;
			dn_free (realtarget);
			return (DS_ERROR_REMOTE);
		}
		error->dse_type = DSE_SECURITYERROR;
		error->ERR_SECURITY.DSE_sc_problem = DSE_SC_ACCESSRIGHTS;
		dn_free (realtarget);
		return (DS_ERROR_REMOTE);
	}

#ifdef TURBO_AVL
	if (entryptr->e_children == NULLAVL) {
#else
	if (entryptr->e_child == NULLENTRY) {
#endif
		int res;
		if (try_cache (arg,result,realtarget) == OK) {
			dn_free (realtarget);
			return (DS_OK);
		}
		res = constructor_dsa_info(realtarget,NULLDNSEQ,FALSE,entryptr,error,di_p);
		dn_free (realtarget);
		return res;		
	}

	dn_free (realtarget);

#ifdef TURBO_AVL
	build_result (arg,entryptr,result,error,dsp ? NULLDN : binddn, dsp);
#else
	build_result (arg,entryptr->e_child,result,error,dsp ? NULLDN : binddn, dsp);
#endif
	return (DS_OK);
}

#ifdef TURBO_AVL
/*
 * these are globals changed by build_result and build_list.  they have
 * to be globals because build_list is called by avl_apply, and it only
 * allows one additional argument to be passed.  pretty gross, but that's
 * the way it is...
 */

static struct subordinate       *g_sub;
static struct subordinate       *g_trail = NULLSUBORD;
static int                      g_count;
static int			g_size;
static DN                       g_dn;
static DN                       g_dnend;

static int build_list(e, dn)
Entry   e;
DN      dn;
{
        struct subordinate      *sub;

        if (g_size != SVC_NOSIZELIMIT && g_count >= g_size)
                return(NOTOK);

        g_dnend->dn_rdn = e->e_name;
        if (check_acl(dn, ACL_READ, e->e_acl->ac_entry, g_dn) != OK)
                return(0);

        sub = (struct subordinate *) smalloc(sizeof(struct subordinate));
        sub->sub_copy = e->e_data;
        sub->sub_rdn = rdn_cpy(e->e_name);
        sub->sub_aliasentry = (e->e_alias == NULLDN ? FALSE : TRUE);
        sub->sub_next = NULLSUBORD;

        if (g_sub == NULLSUBORD) {
                g_sub = sub;
                g_trail = sub;
        } else {
                g_trail->sub_next = sub;
                g_trail = sub;
        }

        g_count++;
        return(0);
}
#endif /* TURBO_AVL */

static int build_result (arg,ptr,result,error,binddn,dsp)
register Entry ptr;
struct ds_list_arg    *arg;
struct ds_list_result *result;
struct DSError * error;
DN binddn;
char dsp;
{
DN dn;
DN dnend;
int size;
RDN dnrdn;
extern int admin_size;
char adminlimit = FALSE;
#ifdef TURBO_AVL
int rc;
Entry akid;
#else
register struct subordinate *sub = NULLSUBORD;
register struct subordinate *trail = NULLSUBORD;
register int cnt;
#endif

	DLOG (log_dsap,LLOG_DEBUG,("building list results"));

	result->lsr_subordinates = NULLSUBORD;
	if (!dsp && manager (binddn))
	    size = arg->lsa_common.ca_servicecontrol.svc_sizelimit;
	else 
	    if ((size = MIN(admin_size,arg->lsa_common.ca_servicecontrol.svc_sizelimit)) == SVC_NOSIZELIMIT) {
		size = admin_size;
		adminlimit = TRUE;
	    }

	result->lsr_age  =  (time_t) 0 ;
	result->lsr_common.cr_requestor = NULLDN;
	/* if no error and NOT SVC_OPT_DONTDEREFERENCEALIASES then */
	/* the alias will have been derefeferenced -signified by   */
	/* NO_ERROR !!! */
	if ( error->dse_type == DSE_NOERROR ) {
		result->lsr_object = NULLDN;
		result->lsr_common.cr_aliasdereferenced = FALSE;
	} else {
		result->lsr_common.cr_aliasdereferenced = TRUE;
#ifdef TURBO_AVL
		result->lsr_object = get_copy_dn (ptr);
#else
		result->lsr_object = get_copy_dn (ptr->e_parent);
#endif
	}
	result->lsr_cr = NULLCONTINUATIONREF;

#ifdef TURBO_AVL
	/* we already checked for null kids ... */
	akid = (Entry) avl_getone(ptr->e_children);
	dn = get_copy_dn(akid);
#else
	dn = get_copy_dn (ptr);
#endif
	for (dnend = dn; dnend->dn_parent != NULLDN; dnend=dnend->dn_parent)
		;  /* NO-OP */
	dnrdn = dnend->dn_rdn;

#ifdef TURBO_AVL
	g_dn = dn;
	g_dnend = dnend;
	g_sub = NULLSUBORD;
	g_size = size;
	g_count = 0;

        /*
         * preorder would be a little faster in case of small size limit,
         * but inorder is more user-predictable, which is nice, though not
         * required...
         */

	rc = avl_apply(ptr->e_children, build_list, (caddr_t) binddn, NOTOK,
	    AVL_INORDER);

        /*
         * build_list has updated g_count and g_sub to contain a count of
         * the number of entries in the list and the list itself,
         * respectively.  if avl_apply was not cut short because the size
         * limit was reached (i.e. instead it ran out of nodes), rc will
         * be NOTOK.
         */

	size = g_size;
	result->lsr_subordinates = g_sub;
#else
	for (cnt =0; (ptr!=NULLENTRY) && (size == SVC_NOSIZELIMIT || cnt < size) ; ptr=ptr->e_sibling) {
		dnend->dn_rdn = ptr->e_name;
		if (check_acl (binddn,ACL_READ,ptr->e_acl->ac_entry,dn) == OK) {
			sub = (struct subordinate *) smalloc (sizeof(struct subordinate));
			sub->sub_copy = ptr->e_data;
			sub->sub_rdn = rdn_cpy(ptr->e_name);
			sub->sub_aliasentry = (ptr->e_alias == NULLDN ? FALSE : TRUE);
			if (trail != NULLSUBORD)
				trail->sub_next = sub;
			else
				result->lsr_subordinates = sub;
			trail = sub;
			cnt++;
		}
	}
	if (sub)
		sub->sub_next = NULLSUBORD;
#endif /* TURBO_AVL */


#ifdef TURBO_AVL
	if ( rc != AVL_NOMORE )
#else
	if ( (size != SVC_NOSIZELIMIT && cnt >= size) && (ptr!=NULLENTRY) )
#endif
		/* stopped look up due to size limit */
		/* need to send continuation reference */
		result->lsr_limitproblem = adminlimit ? 
			LSR_ADMINSIZEEXCEEDED : LSR_SIZELIMITEXCEEDED;
	else
		result->lsr_limitproblem = LSR_NOLIMITPROBLEM;

	dnend->dn_rdn = NULLRDN;
	dn_free (dn);
	rdn_free (dnrdn);
}


try_cache (arg,result,target)
    register struct ds_list_arg          *arg;
    register struct ds_list_result       *result;
    DN 					 target;
{
struct list_cache *ptr;
struct subordinate * subord_cpy();

	if ((arg->lsa_common.ca_servicecontrol.svc_options & SVC_OPT_DONTUSECOPY) == 0) {
		if ((ptr = find_list_cache (target,arg->lsa_common.ca_servicecontrol.svc_sizelimit)) != NULLCACHE) {
			DLOG (log_dsap,LLOG_DEBUG,("building list results using cache"));
			result->lsr_subordinates = subord_cpy(ptr->list_subs);
			result->lsr_age  =  (time_t) 0 ;
			result->lsr_common.cr_aliasdereferenced = FALSE;
			result->lsr_common.cr_requestor = NULLDN;
			result->lsr_object = NULLDN;
			result->lsr_cr = NULLCONTINUATIONREF;
			result->lsr_limitproblem = ptr->list_problem;
			return (OK);
		}
	}

	return (NOTOK);
}

