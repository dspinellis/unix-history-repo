/* dapwait.c - DAP: Deal with incoming activity */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dapwait.c,v 7.1 91/02/22 09:21:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dapwait.c,v 7.1 91/02/22 09:21:09 mrose Interim $
 *
 *
 * $Log:	dapwait.c,v $
 * Revision 7.1  91/02/22  09:21:09  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:31  mrose
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


/* LINTLIBRARY */

#include "logger.h"
#include "quipu/util.h"
#include "quipu/dap2.h"
#include "../x500as/DAS-types.h"


extern LLog	* log_dsap;

#ifdef PDU_DUMP
#define DUMP_ARG 	"arg"
#define DUMP_RES 	"res"
#define DUMP_ERR 	"err"
#endif

/*
* Wait routine for a DAP initiator.
*/

/* ARGSUSED */

int	  DapInitWaitRequest (sd, secs, di)
int			  sd;
int			  secs;
struct DAPindication	* di;
{
    int	  result;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);

    DLOG (log_dsap,LLOG_TRACE,( "DapInitWaitRequest()"));

    result = RoWaitRequest(sd, secs, roi);

    if (result == NOTOK)
    {
	return (ros2daplose (di, "RoBindWaitRequest", &(roi->roi_preject)));
    }

    switch(roi->roi_type)
    {
	case ROI_INVOKE:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInitWaitRequest: Invocation received"));
	    DRejectRequest (sd, ROS_IP_UNRECOG, roi->roi_invoke.rox_id);
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept invokes"));

	case ROI_RESULT:
	    return (DapDecodeResult (sd, &(roi->roi_result), di));

	case ROI_ERROR:
	    return (DapDecodeError (sd, &(roi->roi_error), di));

	case ROI_UREJECT:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInitWaitRequest: Operation (%d) user rejected (%d)", roi->roi_ureject.rou_id, roi->roi_ureject.rou_reason));
		return (ros2dapreject(di, "ROI_UREJECT", &(roi->roi_ureject)));

	case ROI_PREJECT:
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInitWaitRequest: Operation (%d) provider rejected", roi->roi_preject.rop_id));
		return (ros2daplose (di, "ROI_PREJECT", &(roi->roi_preject)));

	case ROI_FINISH:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInitWaitRequest: Unbind request received"));
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept unbind requests"));

	default:
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unknown indication type : %d", roi->roi_type));
	    return (daplose (di, DP_ROS, NULLCP, "DapInitWaitRequest(): RoWaitRequest error"));
        }
	/* NOT REACHED */
}

int	  DapDecodeResult (sd, ror, di)
int			  sd;
struct RoSAPresult	* ror;
struct DAPindication	* di;
{
    int			  success = NOTOK;
    PE			  pe = ror->ror_result;
    struct DSResult	* res = &(di->di_result.dr_res);

    di->di_type = DI_RESULT;
    di->di_result.dr_id = ror->ror_id;

#ifdef PDU_DUMP
	    pdu_dump (pe,DUMP_RES,ror->ror_op);
#endif

#ifdef	HEAVY_DEBUG
	    pdu_res_log (pe, ror->ror_op);
#endif

    switch(res->result_type = ror->ror_op)
    {
    case    OP_READ :
    {
	struct ds_read_result * rr;
	success = decode_DAS_ReadResult(pe,1,NULLIP,NULLVP,&rr);
	res->res_rd = *rr;	/* struct copy */
	free ( (char *) rr);
    }
	break;
    case    OP_COMPARE :
    {
	struct ds_compare_result * cr;
	success = decode_DAS_CompareResult(pe,1,NULLIP,NULLVP,&cr);
	res->res_cm = *cr;	/* struct copy */
	free ( (char *) cr);
    }
	break;
    case    OP_LIST :
    {
	struct ds_list_result * lr;
	success = decode_DAS_ListResult(pe,1,NULLIP,NULLVP,&lr);
	res->res_ls = *lr;	/* struct copy */
	free ( (char *) lr);
    }
	break;
    case    OP_SEARCH :
    {
	struct ds_search_result * sr;
	success = decode_DAS_SearchResult(pe,1,NULLIP,NULLVP,&sr);
	res->res_sr = *sr;	/* struct copy */
	free ( (char *) sr);
    }
	break;

    case    OP_ADDENTRY :
    case    OP_REMOVEENTRY :
    case    OP_MODIFYENTRY :
    case    OP_MODIFYRDN :
    case    OP_ABANDON :

	if ( pe && 
	    (pe -> pe_form == PE_FORM_PRIM) &&
	    ( PE_ID (pe -> pe_class, pe -> pe_id) == 
	      PE_ID (PE_CLASS_UNIV, PE_PRIM_NULL)
	    ))
		success = OK;
	break;

    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DapDecodeResult: op id %d unknown!", ror->ror_op));
	DRejectRequest (sd, ROS_RRP_UNRECOG, ror->ror_id);
	return (daplose (di, DP_RESULT, NULLCP, "Unknown operation identifier"));
    }

    if (success == NOTOK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapDecodeResult: Unable to parse argument"));
	DRejectRequest (sd, ROS_RRP_MISTYPED, ror->ror_id);
	return (daplose (di, DP_RESULT, NULLCP, "Undecodable argument"));
    }

    return(success);
}

int	  DapDecodeError (sd, roe, di)
int			  sd;
struct RoSAPerror	* roe;
struct DAPindication	* di;
{
    int			  success;
    PE			  pe = roe->roe_param;
    struct DSError	* err = &(di->di_error.de_err);

#ifdef PDU_DUMP
	    pdu_dump (pe,DUMP_ERR,roe->roe_id);
#endif

    di->di_type = DI_ERROR;
    di->di_error.de_id = roe->roe_id;

    switch(err->dse_type = roe->roe_error)
    {
    case    DSE_ABANDON_FAILED :
    {
	struct DSE_abandon_fail * af;
	success = decode_DAS_AbandonFailedParm(pe,1,NULLIP,NULLVP,&af);
	err->dse_un.dse_un_abandon_fail = *af; /* struct copy */
	free ((char *)af);
    }
	break;
    case    DSE_ATTRIBUTEERROR :
    {
	struct DSE_attribute * at;
	success = decode_DAS_AttributeErrorParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_attribute = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_NAMEERROR : 
    {
	struct DSE_name * at;
	success = decode_DAS_NameErrorParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_name = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_REFERRAL :
    {
	struct DSE_referral * at;
	success = decode_DAS_ReferralParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_referral = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_SECURITYERROR :
    {
	struct DSE_security * at;
	success = decode_DAS_SecurityErrorParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_security = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_SERVICEERROR :
    {
	struct DSE_service * at;
	success = decode_DAS_ServiceErrorParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_service = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_UPDATEERROR :
    {
	struct DSE_update * at;
	success = decode_DAS_UpdateErrorParm(pe,1,NULLIP,NULLVP,&at);
	err->dse_un.dse_un_update = *at; /* struct copy */
	free ((char *) at);
    }
	break;
    case    DSE_ABANDONED :
	success = ((pe == NULLPE) ? OK : NOTOK);
	break;
    case	DSE_DSAREFERRAL :
    {
	struct DSE_referral * at;
	success = decode_DO_DSAReferralParm(pe, 1, NULLIP, NULLVP, &at);
	err->dse_un.dse_un_referral = *at; /* struct copy */
	free ((char *) at);
    }
	break;

    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DapDecodeError: op id %d unknown!", roe->roe_error));
	DRejectRequest (sd, ROS_REP_UNRECOG, roe->roe_id);
	return (daplose (di, DP_ERROR, NULLCP, "Unknown operation identifier"));
    }

    if (success == NOTOK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapDecodeError: Unable to parse argument"));
	DRejectRequest (sd, ROS_RRP_MISTYPED, roe->roe_id);
	return (daplose (di, DP_ERROR, NULLCP, "Undecodable argument"));
    }

    return(success);
}

