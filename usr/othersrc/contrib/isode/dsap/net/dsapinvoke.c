/* dsapinvoke.c - DSAP : Invoke DAP operations */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapinvoke.c,v 7.1 91/02/22 09:21:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapinvoke.c,v 7.1 91/02/22 09:21:20 mrose Interim $
 *
 *
 * $Log:	dsapinvoke.c,v $
 * Revision 7.1  91/02/22  09:21:20  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:54  mrose
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
#include "quipu/dsap.h"
#include "../x500as/DAS-types.h"
#include "../x500as/Quipu-types.h"

extern  LLog    * log_dsap;

int	  DapInvokeRequest (sd, id, arg, di)
int			  sd;
int			  id;
struct DSArgument	* arg;
struct DSAPindication	* di;
{
    int				  result;
    PE				  arg_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (DapEncodeInvoke (&(arg_pe), arg) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInvokeRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation argument"));
    }

    result = RoInvokeRequest (sd, arg->arg_type, ROS_ASYNC, arg_pe,
		id, NULLIP, ROS_NOPRIO, roi);

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInvokeRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapInvokeRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    if (arg_pe != NULLPE)
	pe_free (arg_pe);
    return (OK);
}

int	  DapEncodeInvoke (pep, arg)
PE			* pep;
struct DSArgument	* arg;
{
    int		success;

    switch(arg->arg_type)
    {
    case    OP_READ :
	success = encode_DAS_ReadArgument(pep,1,0,NULLCP,&(arg->arg_rd));
	break;
    case    OP_COMPARE :
	success = encode_DAS_CompareArgument(pep,1,0,NULLCP,&(arg->arg_cm));
	break;
    case    OP_ABANDON :
	success = encode_DAS_AbandonArgument(pep,1,0,NULLCP,&(arg->arg_ab));
	break;
    case    OP_LIST :
	success = encode_DAS_ListArgument(pep,1,0,NULLCP,&(arg->arg_ls));
	break;
    case    OP_SEARCH :
	success = encode_DAS_SearchArgument(pep,1,0,NULLCP,&(arg->arg_sr));
	break;
    case    OP_ADDENTRY :
	success = encode_DAS_AddEntryArgument(pep,1,0,NULLCP,&(arg->arg_ad));
	break;
    case    OP_REMOVEENTRY :
	success = encode_DAS_RemoveEntryArgument(pep,1,0,NULLCP,&(arg->arg_rm));
	break;
    case    OP_MODIFYENTRY :
	success = encode_DAS_ModifyEntryArgument(pep,1,0,NULLCP,&(arg->arg_me));
	break;
    case    OP_MODIFYRDN :
	success = encode_DAS_ModifyRDNArgument(pep,1,0,NULLCP,&(arg->arg_mr));
	break;
    default :
	success = NOTOK;
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DapEncodeInvoke(): unknown op type %d", arg->arg_type));
	break;
    }

    return(success);
}

int	  DspInvokeRequest (sd, id, arg, di)
int	  sd;
int	  id;
struct ds_op_arg	* arg;
struct DSAPindication	* di;
{
    int				  result;
    PE				  arg_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (DspEncodeInvoke (&(arg_pe), arg) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspInvokeRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation argument"));
    }

    watch_dog("RoInvokeRequest (DSP)");
    result = RoInvokeRequest (sd, arg->dca_dsarg.arg_type, ROS_ASYNC, arg_pe,
		id, NULLIP, ROS_NOPRIO, roi);
    watch_dog_reset();

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspInvokeRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspInvokeRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    if (arg_pe != NULLPE)
	pe_free (arg_pe);
    return (OK);
}

int	  DspEncodeInvoke (pep, arg)
PE			* pep;
struct ds_op_arg	* arg;
{
    int		success;

    switch(arg->dca_dsarg.arg_type)
    {
    case    OP_READ :
	success = encode_DO_ChainedReadArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_COMPARE :
	success = encode_DO_ChainedCompareArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_ABANDON :
	success = encode_DAS_AbandonArgument(pep,1,0,NULLCP,&(arg->dca_dsarg.arg_ab));
	break;
    case    OP_LIST :
	success = encode_DO_ChainedListArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_SEARCH :
	success = encode_DO_ChainedSearchArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_ADDENTRY :
	success = encode_DO_ChainedAddEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_REMOVEENTRY :
	success = encode_DO_ChainedRemoveEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_MODIFYENTRY :
	success = encode_DO_ChainedModifyEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_MODIFYRDN :
	success = encode_DO_ChainedModifyRDNArgument(pep,1,0,NULLCP,arg);
	break;
    default :
	success = NOTOK;
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DspEncodeInvoke(): unknown op type %d", arg->dca_dsarg.arg_type));
	break;
    }

    return(success);
}

int	  QspInvokeRequest (sd, id, arg, di)
int	  sd;
int	  id;
struct ds_op_arg	* arg;
struct DSAPindication	* di;
{
    int				  result;
    PE				  arg_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (QspEncodeInvoke (&(arg_pe), arg) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspInvokeRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation argument"));
    }

    watch_dog("RoInvokeRequest (QSP)");
    result = RoInvokeRequest (sd, arg->dca_dsarg.arg_type, ROS_ASYNC, arg_pe,
		id, NULLIP, ROS_NOPRIO, roi);
    watch_dog_reset();

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspInvokeRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoInvokeRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspInvokeRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoInvokeRequest failed"));
	}
    }

    if (arg_pe != NULLPE)
	pe_free (arg_pe);
    return (OK);
}

int	  QspEncodeInvoke (pep, arg)
PE			* pep;
struct ds_op_arg	* arg;
{
    int		success;

    switch(arg->dca_dsarg.arg_type)
    {
    case    OP_READ :
	success = encode_DO_ChainedReadArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_COMPARE :
	success = encode_DO_ChainedCompareArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_ABANDON :
	success = encode_DAS_AbandonArgument(pep,1,0,NULLCP,&(arg->dca_dsarg.arg_ab));
	break;
    case    OP_LIST :
	success = encode_DO_ChainedListArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_SEARCH :
	success = encode_DO_ChainedSearchArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_ADDENTRY :
	success = encode_DO_ChainedAddEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_REMOVEENTRY :
	success = encode_DO_ChainedRemoveEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_MODIFYENTRY :
	success = encode_DO_ChainedModifyEntryArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_MODIFYRDN :
	success = encode_DO_ChainedModifyRDNArgument(pep,1,0,NULLCP,arg);
	break;
    case    OP_GETEDB :
        success = encode_Quipu_GetEntryDataBlockArgument(pep,1,0,NULLCP,&(arg->dca_dsarg.arg_ge));
        break;
    default :
	success = NOTOK;
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("QspEncodeInvoke(): unknown op type %d", arg->dca_dsarg.arg_type));
	break;
    }

    return(success);
}

