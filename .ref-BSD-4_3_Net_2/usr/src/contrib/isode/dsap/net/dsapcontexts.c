/* dsapcontexts.c - Directory context checking routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapcontexts.c,v 7.2 91/02/22 09:21:17 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapcontexts.c,v 7.2 91/02/22 09:21:17 mrose Interim $
 *
 *
 * $Log:	dsapcontexts.c,v $
 * Revision 7.2  91/02/22  09:21:17  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/20  15:29:42  mrose
 * cjr
 * 
 * Revision 7.0  90/07/26  14:45:50  mrose
 * *** empty log message ***
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


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/dsap.h"
#include "tsap.h"

extern LLog * log_dsap;
#ifndef NO_STATS
extern LLog * log_stat;
#endif


OID	acse_pci;
OID	x500_da_as;
OID	x500_ds_as;
OID	quipu_ds_as;
OID	x500_da_ac;
OID	x500_ds_ac;
OID	quipu_ds_ac;
struct PSAPctxlist         x500_da_pcdl_s;
struct PSAPctxlist	* x500_da_pcdl = &x500_da_pcdl_s;
struct PSAPctxlist         x500_ds_pcdl_s;
struct PSAPctxlist	* x500_ds_pcdl = &x500_ds_pcdl_s;
struct PSAPctxlist         quipu_ds_pcdl_s;
struct PSAPctxlist	* quipu_ds_pcdl = &quipu_ds_pcdl_s;

int	  select_context (app_ctx)
OID			  app_ctx;
{
	if(oid_cmp(app_ctx, x500_da_ac) == 0)
	{
		return(DS_CTX_X500_DAP);
	}

	if(oid_cmp(app_ctx, x500_ds_ac) == 0)
	{
		return(DS_CTX_X500_DSP);
	}

	if(oid_cmp(app_ctx, quipu_ds_ac) == 0)
	{
		return(DS_CTX_QUIPU_DSP);
	}

	return (NOTOK);
}

/* ARGSUSED */
int	  judge_ctxlist(req_ctxlist, ok_ctxlist)
struct PSAPctxlist      * req_ctxlist;
struct PSAPctxlist      * ok_ctxlist;
{
    int			  ctxlist_notok = OK;
    int                   i;
    int                   j;
    OID                   ok_asn;
    OID                   req_asn;

    DLOG (log_dsap, LLOG_TRACE, ("judge_ctxlist"));

    for(i=0; i<req_ctxlist->pc_nctx; i++)
    {
	DLOG (log_dsap, LLOG_DEBUG, ("Context (%d): id=%d, %s",
		i,
		req_ctxlist->pc_ctx[i].pc_id,
		oid2ode (req_ctxlist->pc_ctx[i].pc_asn)));

	if(req_ctxlist->pc_ctx[i].pc_result == PC_ACCEPT)
	    req_ctxlist->pc_ctx[i].pc_result = PC_REJECTED;
    }

    for(j=0; j<ok_ctxlist->pc_nctx; j++)
    {
	ok_asn = ok_ctxlist->pc_ctx[j].pc_asn;
	for(i=0; i<req_ctxlist->pc_nctx; i++)
	{
	    if((req_asn = req_ctxlist->pc_ctx[i].pc_asn) == NULLOID) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,( "Reject: asn is NULLOID"));
		continue;
	    }

	    if((oid_cmp(req_asn, ok_asn) == 0))
		break;
	}
	if(i < req_ctxlist->pc_nctx) {
	    req_ctxlist->pc_ctx[i].pc_result = PC_ACCEPT;
	} else {
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("Missing Context: %s", oid2ode (ok_asn)));
	    ctxlist_notok = NOTOK;
	}
    }

#ifdef	DEBUG
    for(i=0; i<req_ctxlist->pc_nctx; i++)
    {
	DLOG(log_dsap, LLOG_DEBUG, ("ctx[%d] id = %d, res = %d.", i,
		req_ctxlist->pc_ctx[i].pc_id,
		req_ctxlist->pc_ctx[i].pc_result));

	if(req_ctxlist->pc_ctx[i].pc_result == PC_REJECTED)
		DLOG (log_dsap, LLOG_DEBUG, ("Context Rejected: id=%d, %s",
			req_ctxlist->pc_ctx[i].pc_id,
			oid2ode (req_ctxlist->pc_ctx[i].pc_asn)));
	    
    }
#endif

    return(ctxlist_notok);
}

int	  find_ctx_id(pcdl, ctx_oid)
struct PSAPctxlist	* pcdl;
OID			  ctx_oid;
{
    int	  i;

    DLOG (log_dsap, LLOG_TRACE, ("find_ctx_id"));

    for(i=0; i<pcdl->pc_nctx; i++)
    {
	if(oid_cmp(ctx_oid, pcdl->pc_ctx[i].pc_asn) == 0)
	    break;
    }

    if(i < pcdl->pc_nctx)
	return(pcdl->pc_ctx[i].pc_id);

    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Couldn't find context identifier %s", sprintoid(ctx_oid)));

    return(NOTOK);
}

int	  check_dap_ctxlist (ctxlist)
struct PSAPctxlist	* ctxlist;
{
	if (judge_ctxlist (ctxlist, x500_da_pcdl) != OK)
		return (NOTOK);

	return (find_ctx_id (ctxlist, x500_da_as));
}

int	  check_dsp_ctxlist (ctxlist)
struct PSAPctxlist	* ctxlist;
{
	if (judge_ctxlist (ctxlist, x500_ds_pcdl) != OK)
		return (NOTOK);

	return (find_ctx_id (ctxlist, x500_ds_as));
}

int	  check_qsp_ctxlist (ctxlist)
struct PSAPctxlist	* ctxlist;
{
	if (judge_ctxlist (ctxlist, quipu_ds_pcdl) != OK)
		return (NOTOK);

	return (find_ctx_id (ctxlist, quipu_ds_as));
}

