/* dapinvoke.c - DAP : Invoke DAP operations */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dapinvoke.c,v 7.2 91/02/22 09:20:57 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dapinvoke.c,v 7.2 91/02/22 09:20:57 mrose Interim $
 *
 *
 * $Log:	dapinvoke.c,v $
 * Revision 7.2  91/02/22  09:20:57  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:43:18  mrose
 * sync
 * 
 * Revision 7.0  90/07/26  14:45:21  mrose
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

#include <stdio.h>
#include "logger.h"
#include "quipu/util.h"
#include "quipu/dap2.h"
#include "../x500as/DAS-types.h"
#include "../x500as/Quipu-types.h"

extern  LLog    * log_dsap;
extern	void	  ros_log();

#ifdef PDU_DUMP
#define DUMP_ARG 	"arg"
#define DUMP_RES 	"res"
#define DUMP_ERR 	"err"
#endif

int	  DapInvokeReqAux (sd, id, op, pe, di, asyn)
int			  sd;
int			  id;
int			  op;
PE			  pe;
struct DAPindication	* di;
int			  asyn;
{

#ifdef PDU_DUMP
    pdu_dump (pe,DUMP_ARG,op);
#endif

#ifdef HEAVY_DEBUG
    pdu_arg_log (pe, op);
#endif

	switch (asyn)
	{
	case ROS_SYNC:
		return (DapSyncInvokeRequest (sd, id, op, pe, di));

	case ROS_INTR:
		return (DapIntrInvokeRequest (sd, id, op, pe, di));

	case ROS_ASYNC:
		return (DapAsynInvokeRequest (sd, id, op, pe, di));

	default:
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInvokeReqAux(): asyn has unknown value: %d", asyn));
		return (daplose (di, DP_INVOKE, NULLCP, "Unknown synchronicity"));
	}
}

int	  DapSyncInvokeRequest (sd, id, op, pe, di)
int			  sd;
int			  id;
int			  op;
PE			  pe;
struct DAPindication	* di;
{
    int				  result;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    DLOG (log_dsap,LLOG_TRACE,( "DapSyncInvokeRequest()"));

    result = RoInvokeRequest (sd, op, ROS_SYNC, pe,
				id, NULLIP, ROS_NOPRIO, roi);

    if (result != OK)
    {
	if (roi->roi_type != ROI_PREJECT)
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest(): Failed without rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest inconsistent result"));
	}

	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest(): Fatal rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest(): Non-Fatal rejection"));
	    return (dapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    switch(roi->roi_type)
    {
	case ROI_INVOKE:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest: Invocation received"));
	    DRejectRequest (sd, ROS_IP_UNRECOG, roi->roi_invoke.rox_id);
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept invokes"));

	case ROI_RESULT:
	    return (DapDecodeResult (sd, &(roi->roi_result), di));

	case ROI_ERROR:
	    return (DapDecodeError (sd, &(roi->roi_error), di));

	case ROI_UREJECT:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest: Operation (%d) user rejected (%d)", roi->roi_ureject.rou_id, roi->roi_ureject.rou_reason));
		return (ros2dapreject (di, "ROI_UREJECT", &(roi->roi_ureject)));

	case ROI_PREJECT:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest: Operation (%d) provider rejected", roi->roi_preject.rop_id));
	    return (ros2daplose (di, "ROI_PREJECT", &(roi->roi_preject)));

	case ROI_FINISH:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapSyncInvokeRequest: Unbind request received"));
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept unbind requests"));

	default:
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unknown indication type : %d", roi->roi_type));
	    break;
    }

    return (OK);
}

int	  DapIntrInvokeRequest (sd, id, op, pe, di)
int			  sd;
int			  id;
int			  op;
PE			  pe;
struct DAPindication	* di;
{
    int				  result;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    DLOG (log_dsap,LLOG_TRACE,( "DapIntrInvokeRequest()"));

    result = RoIntrRequest (sd, op, pe, id, NULLIP, ROS_NOPRIO, roi);

    if (result != OK)
    {
	if (roi->roi_type != ROI_PREJECT)
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest(): Failed without rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest inconsistent result"));
	}

	if (rop->rop_reason == ROS_INTERRUPTED)
	{
	     return (DapInterrupt(sd, id, op, di));
	}

	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest(): Fatal rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest(): Non-Fatal rejection"));
	    return (dapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    switch(roi->roi_type)
    {
	case ROI_INVOKE:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest: Invocation received"));
	    DRejectRequest (sd, ROS_IP_UNRECOG, roi->roi_invoke.rox_id);
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept invokes"));

	case ROI_RESULT:
	    return (DapDecodeResult (sd, &(roi->roi_result), di));

	case ROI_ERROR:
	    return (DapDecodeError (sd, &(roi->roi_error), di));

	case ROI_UREJECT:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest: Operation (%d) user rejected (%d)", roi->roi_ureject.rou_id, roi->roi_ureject.rou_reason));
		return (ros2dapreject (di, "ROI_UREJECT", &(roi->roi_ureject)));

	case ROI_PREJECT:
		LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest: Operation (%d) provider rejected", roi->roi_preject.rop_id));
		return (ros2daplose (di, "ROI_PREJECT", &(roi->roi_preject)));

	case ROI_FINISH:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapIntrInvokeRequest: Unbind request received"));
	    return (daplose (di, DP_ROS, NULLCP, "DAP initiator cannot accept unbind requests"));

	default:
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unknown indication type : %d", roi->roi_type));
	    break;
    }

    return (OK);
}

int	  DapAsynInvokeRequest (sd, id, op, pe, di)
int			  sd;
int			  id;
int			  op;
PE			  pe;
struct DAPindication	* di;
{
    int				  result;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    result = RoInvokeRequest (sd, op, ROS_ASYNC, pe,
				id, NULLIP, ROS_NOPRIO, roi);

    if (result != OK)
    {
	if (roi->roi_type != ROI_PREJECT)
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapAsynInvokeRequest(): Failed without rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest inconsistent result"));
	}

	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapAsynInvokeRequest(): Fatal rejection"));
	    return (daplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapAsynInvokeRequest(): Non-Fatal rejection"));
	    return (dapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    return (OK);
}

int	  DapInterrupt(sd, id, op, di)
int			  sd;
int			  id;
int			  op;
struct DAPindication	* di;
{
    /*
    * Abandoning. Trickier than it looks!
    * Need to RoInvoke an abandon op, which will receive
    * One of the following:
    *    Result/Error for op being abandoned sent before
    *	this abandon arrived at the DSA;
    *    Abandoned error for op being abandoned;
    *    Result for abandon op which has overtaken the
    *	abandoned error for previous op between DSA and DUA
    *    Error for abandon because DSA has screwed up.
    *
    * Unless something goes wrong there should be 2 Ro events to
    * collect before returning. 
    */

    /* abandon operation */
    struct ds_abandon_arg	  ab_arg;
    struct DSError		  ab_err;
    PE				  ab_req_pe;
    int				  old_id;
    int				  new_id;
    int				  ret1;
    int				  ret2;
    struct RoSAPindication	  roi1_s;
    struct RoSAPindication	* roi1 = &(roi1_s);
    struct RoSAPpreject		* rop1 = &(roi1->roi_preject);
    struct RoSAPindication	  roi2_s;
    struct RoSAPindication	* roi2 = &(roi2_s);
    struct RoSAPpreject		* rop2 = &(roi2->roi_preject);
    struct RoSAPindication	* result_roi;

    ab_arg.aba_invokeid = old_id = id;
    new_id = ++id;

    if(encode_DAS_AbandonArgument(&ab_req_pe,1,0,NULLCP,&ab_arg) != OK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Failed to encode an abandon operation"));
	/* Go on listening for result or dump out ?? */
	return(dapreject (di, DP_INVOKE, old_id, NULLCP, "DapInterrupt: Abandon argument encoding failed"));
    }
    else
    {
	DLOG(log_dsap, LLOG_DEBUG, ("Abandon invoke request"));

    	ret1 = RoInvokeRequest(sd,OP_ABANDON,ROS_SYNC,ab_req_pe,new_id,NULLIP,ROS_NOPRIO,roi1);

	DLOG(log_dsap, LLOG_DEBUG, ("Abandon RoInvoke returns: %d", ret1));

	if (ab_req_pe != NULLPE)
	    pe_free(ab_req_pe);

	switch(ret1)
	{
	    case OK:
		/* What have we got? */
		switch(roi1->roi_type)
		{
		    case ROI_RESULT:
		        if(roi1->roi_result.ror_id == old_id)
		        {
			    /* Ferret result away for later */
			    result_roi = roi1;
		        }
		        else if(roi1->roi_result.ror_id == (old_id + 1))
		        {
			    RORFREE (&(roi1->roi_result));
		        }
		        else
		        {
			    LLOG(log_dsap, LLOG_EXCEPTIONS, ("ARRGH! Abandon sent: event for neither op nor abandon op returned!!"));
			    return(dapreject (di, DP_INVOKE, roi1->roi_result.ror_id, NULLCP, "Unexpected operation identifier"));
		        }
		        break;

		    case ROI_ERROR:
#ifdef PDU_DUMP
	    pdu_dump (roi1->roi_error.roe_param,DUMP_ERR,op);
#endif

		        if(roi1->roi_error.roe_id == old_id)
		        {
			    /* Ferret error away for later */
			    result_roi = roi1;
		        }
		        else if(roi1->roi_error.roe_id == (old_id + 1))
		        {
			    if (roi1->roi_error.roe_error != DSE_ABANDON_FAILED)
			    {
				LLOG(log_dsap, LLOG_EXCEPTIONS, ("Failed to abandon correctly"));
				return(dapreject (di, DP_INVOKE, roi1->roi_error.roe_id, NULLCP, "Error mistyped for abandon"));
			    }
			    else
			    {
				struct DSE_abandon_fail * af;
			        if(decode_DAS_AbandonFailedParm (roi1->roi_error.roe_param, 1, NULLIP, NULLVP, &af) != OK)
				{
				    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Failed to decode abandonFailed"));
				    return(dapreject (di, DP_INVOKE, old_id, NULLCP, "Abandon error decoding failed"));
				}
				else
				{
				    LLOG(log_dsap, LLOG_NOTICE, ("Abandon failed error!!!"));
				    ab_err.dse_un.dse_un_abandon_fail = *af; /* struct copy */
				    free ((char *)af);
				    ab_err.dse_type = DSE_ABANDON_FAILED;
				    log_ds_error(&ab_err);
				    ds_error_free(&ab_err);
				}
			    }
		        }
		        else
		        {
			    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Abandon sent : event for neither op nor abandon op returned!!"));
			    return(dapreject (di, DP_INVOKE, roi1->roi_error.roe_id, NULLCP, "Unrecognised op id"));
		        }
		        break;

		    default:
		        LLOG(log_dsap, LLOG_FATAL, ("Unexpected roi_type : %d", roi1->roi_type));
		        return(dapreject (di, DP_INVOKE, -1, NULLCP, "Unrecognised event"));
		}
	        break;

	    case NOTOK:
		ros_log(rop1, "RO-INVOKE.REQUEST");
		if (ROS_FATAL (rop2->rop_reason))
		{
		    return (ros2daplose (di, "DapInterrupt", rop2));
		}
		else
		{
		    return (dapreject (di, DP_ROS, -1, NULLCP, "DapInterrupt: Non-fatal reject"));
		}

	    case DONE:
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Responder has sent Finish!"));
		return(daplose (di, DP_ROS, NULLCP, "Received Finish"));

            default:
		LLOG(log_dsap, LLOG_FATAL, ("Unknown return from RoInvokeRequest : %d", ret1));
		return(daplose (di, DP_ROS, NULLCP, "RoInvokeRequest error"));
        }

	ret2 = RoWaitRequest(sd, NOTOK, roi2);

	DLOG(log_dsap, LLOG_DEBUG, ("Abandon RoInvoke returns: %d", ret1));

	switch(ret2)
	{
	    case OK:
	    /* What have we got? */
		switch(roi2->roi_type)
		{
		    case ROI_RESULT:
		        if(roi2->roi_result.ror_id == old_id)
		        {
			    /* Ferret result away for later */
			    result_roi = roi2;
		        }
		        else if(roi2->roi_result.ror_id == (old_id + 1))
		        {
			    RORFREE (&(roi2->roi_result));
		        }
		        else
		        {
			    LLOG(log_dsap, LLOG_EXCEPTIONS, ("ARRGH! Abandon sent and event for neither op nor abandon op returned!!"));
			    return(dapreject (di, DP_INVOKE, roi1->roi_result.ror_id, NULLCP, "Unexpected operation identifier"));
		        }
		        break;

		    case ROI_ERROR:
#ifdef PDU_DUMP
	    pdu_dump (roi1->roi_error.roe_param,DUMP_ERR,op);
#endif

		        if(roi2->roi_error.roe_id == old_id)
		        {
			    /* Ferret error away for later */
			    result_roi = roi2;
		        }
		        else if(roi2->roi_error.roe_id == (old_id + 1))
		        {
			    if (roi2->roi_error.roe_error != DSE_ABANDON_FAILED)
			    {
				LLOG(log_dsap, LLOG_EXCEPTIONS, ("Failed to abandon correctly"));
				return(dapreject (di, DP_INVOKE, roi2->roi_error.roe_id, NULLCP, "Error mistyped for abandon"));
			    }
			    else
			    {
				
				struct DSE_abandon_fail * af;
			        if(decode_DAS_AbandonFailedParm (roi2->roi_error.roe_param, 1, NULLIP, NULLVP, &af) != OK)
				{
				    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Failed to decode abandonFailed"));
				    return(dapreject (di, DP_INVOKE, old_id, NULLCP, "Abandon error decoding failed"));
				}
				else
				{
				    LLOG(log_dsap, LLOG_NOTICE, ("Abandon failed error!!!"));
				    ab_err.dse_un.dse_un_abandon_fail = *af; /*struct copy */
				    free ((char *) af);
				    ab_err.dse_type = DSE_ABANDON_FAILED;
				    log_ds_error(&ab_err);
				    ds_error_free(&ab_err);
				}
			    }
		        }
		        else
		        {
			    LLOG(log_dsap, LLOG_EXCEPTIONS, ("Abandon sent : event for neither op nor abandon op returned!!"));
			    return(dapreject (di, DP_INVOKE, roi1->roi_error.roe_id, NULLCP, "Unrecognised op id"));
		        }
		        break;

		    default:
		        LLOG(log_dsap, LLOG_FATAL, ("Unexpected roi_type : %d", roi2->roi_type));
		        return(dapreject (di, DP_INVOKE, -1, NULLCP, "Unrecognised event"));
		}
		break;

	    case NOTOK:
		ros_log(rop2, "RO-WAIT.REQUEST (Abandon)");
		if (ROS_FATAL (rop1->rop_reason))
		{
		    return (ros2daplose (di, "DapInterrupt", rop2));
		}
		else
		{
		    return (dapreject (di, DP_ROS, -1, NULLCP, "DapInterrupt: Non-fatal reject"));
		}

	    case DONE:
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Responder has sent Finish!"));
		return(daplose (di, DP_ROS, NULLCP, "Received Finish"));

            default:
		LLOG(log_dsap, LLOG_FATAL, ("Unknown return from RoInvokeRequest : %d", ret2));
		return(daplose (di, DP_ROS, NULLCP, "RoInvokeRequest error"));
        }
    }

    switch(result_roi->roi_type)
    {
	case ROI_RESULT:
	    return (DapDecodeResult (sd, &(result_roi->roi_result), di));

	case ROI_ERROR:
	    return (DapDecodeError (sd, &(result_roi->roi_error), di));

	default:
	    return (dapreject (di, DP_ROS, old_id, NULLCP, "DapInterrupt erroneous"));
    }
}

#ifdef PDU_DUMP

static int pdu_count = -1;
static char * pdu_dir = NULLCP;

pdu_dump_init (dir)
char * dir;
{
	pdu_count = 0;
	pdu_dir = strdup (dir);
	LLOG (log_dsap, LLOG_NOTICE, ("PDU Tracing enabled - %s",dir));
	
	(void) mkdir (pdu_dir,0755);
}

pdu_dump (pe,type,op)
PE pe;
char * type;
int op;
{
char filename [BUFSIZE];
char * oper;
PS ps;
FILE * fptr;

	if ( pdu_count == -1)
		return;

	if (strcmp (type,DUMP_ARG) == 0)
		pdu_count++;

	switch (op) {
		case OP_READ:
			oper = "read";
			break;
		case OP_COMPARE:
			oper = "compare";
			break;
		case OP_ABANDON:	/* Humm ... */
			oper = "abandon";
			break;
		case OP_LIST:
			oper = "list";
			break;
		case OP_SEARCH:
			oper = "search";
			break;
		case OP_ADDENTRY:
			oper = "add";
			break;
		case OP_REMOVEENTRY:
			oper = "remove";
			break;
		case OP_MODIFYENTRY:
			oper = "modify";
			break;
		case OP_MODIFYRDN:
			oper = "modifyrdn";
			break;
		case OP_GETEDB:
			oper = "getedb";
			break;
		case 100:	/* special case for bind */
			oper = "bind";
			break;
	}

	if (strcmp (type,DUMP_ERR) == 0)
		oper = "oper";

	(void) sprintf (filename, "%s/%s_%s.%d",pdu_dir,oper,type,pdu_count);
	DLOG (log_dsap,LLOG_DEBUG,("Writing PDU to file %s",filename));

	if ((fptr = fopen (filename,"w")) == (FILE *) NULL) {
		LLOG(log_dsap,LLOG_EXCEPTIONS,("Cant open PDU file %s",filename));
		return;
	}

	ps = ps_alloc (std_open);
	if (std_setup (ps,fptr) != OK) {
		(void) fclose (fptr);
		return;
	}
		
	(void) pe2pl (ps,pe);

	(void) fclose (fptr);
	ps_free (ps);

}
#endif

#ifdef	HEAVY_DEBUG
pdu_arg_log (pe,op)
PE pe;
int op;
{
    /* PDU Level Logging */
    switch (op) {
	case OP_READ:
	    PLOG (log_dsap, print_DAS_ReadArgument, pe, "Read", 0);
	    break;
	case OP_COMPARE:
	    PLOG (log_dsap, print_DAS_CompareArgument, pe, "Compare", 0);
	    break;
	case OP_ABANDON:
	    PLOG (log_dsap, print_DAS_AbandonArgument, pe, "Abandon", 0);
	    break;
	case OP_LIST:
	    PLOG (log_dsap, print_DAS_ListArgument, pe, "List", 0);
	    break;
	case OP_SEARCH:
	    PLOG (log_dsap, print_DAS_SearchArgument, pe, "Search", 0);
	    break;
	case OP_ADDENTRY:
	    PLOG (log_dsap, print_DAS_AddEntryArgument, pe, "AddEntry", 0);
	    break;
	case OP_REMOVEENTRY:
	    PLOG (log_dsap, print_DAS_RemoveEntryArgument, pe, "RemoveEntry", 0);
	    break;
	case OP_MODIFYENTRY:
	    PLOG (log_dsap, print_DAS_ModifyEntryArgument, pe, "ModifyEntry", 0);
	    break;
	case OP_MODIFYRDN:
	    PLOG (log_dsap, print_DAS_ModifyRDNArgument, pe, "ModifyRDN", 0);
	    break;
	case OP_GETEDB:
	    PLOG (log_dsap, print_Quipu_GetEntryDataBlockArgument, pe, "GetEDB", 0);
	    break;
	default:
		LLOG (log_dsap, LLOG_PDUS, ("Unknown operation (%d) - no argument PDU logged",op));
    }
}

pdu_res_log (pe, op)
PE	  pe;
int	  op;
{
    /* PDU Level Logging */
    switch (op)
    {
	case OP_READ:
	    PLOG (log_dsap, print_DAS_ReadResult, pe, "Read", 1);
	    break;

	case OP_COMPARE:
	    PLOG (log_dsap, print_DAS_CompareResult, pe, "Compare", 1);
	    break;
	case OP_ABANDON:
	    PLOG (log_dsap, print_DAS_AbandonResult, pe, "Abandon", 1);
	    break;
	case OP_LIST:
	    PLOG (log_dsap, print_DAS_ListResult, pe, "List", 1);
	    break;
	case OP_SEARCH:
	    PLOG (log_dsap, print_DAS_SearchResult, pe, "Search", 1);
	    break;
	case OP_ADDENTRY:
	    PLOG (log_dsap, print_DAS_AddEntryResult, pe, "AddEntry", 1);
	    break;
	case OP_REMOVEENTRY:
	    PLOG (log_dsap, print_DAS_RemoveEntryResult, pe, "RemoveEntry",1);
	    break;
	case OP_MODIFYENTRY:
	    PLOG (log_dsap, print_DAS_ModifyEntryResult, pe, "ModifyEntry",1);
	    break;
	case OP_MODIFYRDN:
	    PLOG (log_dsap, print_DAS_ModifyRDNResult, pe, "ModifyRDN", 1);
	    break;
	case OP_GETEDB:
	    PLOG (log_dsap, print_Quipu_GetEntryDataBlockResult, pe, "GetEDB", 1);
	    break;
	default:
	    LLOG (log_dsap, LLOG_PDUS, ("Unknown operation (%d) - no result PDU logged",op));
    }
}
#endif
