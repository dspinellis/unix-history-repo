/* dsaperror.c - DSAP : Return operation errors */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsaperror.c,v 7.1 91/02/22 09:21:18 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsaperror.c,v 7.1 91/02/22 09:21:18 mrose Interim $
 *
 *
 * $Log:	dsaperror.c,v $
 * Revision 7.1  91/02/22  09:21:18  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:52  mrose
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

extern  LLog    * log_dsap;

int	  DapErrorRequest (sd, id, err, di)
int			  sd;
int			  id;
struct DSError		* err;
struct DSAPindication	* di;
{
    int				  result;
    PE				  err_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (DEncodeError (&(err_pe), err) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapErrorRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation error"));
    }

    watch_dog ("RoErrorRequest (DAP)");
    result = RoErrorRequest (sd, id, err->dse_type, err_pe, ROS_NOPRIO, roi);
    watch_dog_reset();

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapErrorRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoErrorRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapErrorRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoErrorRequest failed"));
	}
    }

    if (err_pe != NULLPE)
	pe_free (err_pe);

    return (OK);
}

int	  DspErrorRequest (sd, id, err, di)
int			  sd;
int			  id;
struct DSError		* err;
struct DSAPindication	* di;
{
    int				  result;
    PE				  err_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (DEncodeError (&(err_pe), err) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspErrorRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation error"));
    }

    watch_dog ("RoErrorRequest (DSP)");
    result = RoErrorRequest (sd, id, err->dse_type, err_pe, ROS_NOPRIO, roi);
    watch_dog_reset();

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspErrorRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoErrorRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("DspErrorRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoErrorRequest failed"));
	}
    }

    if (err_pe != NULLPE)
	pe_free (err_pe);

    return (OK);
}

int	  QspErrorRequest (sd, id, err, di)
int			  sd;
int			  id;
struct DSError		* err;
struct DSAPindication	* di;
{
    int				  result;
    PE				  err_pe;
    struct RoSAPindication	  roi_s;
    struct RoSAPindication	* roi = &(roi_s);
    struct RoSAPpreject		* rop = &(roi->roi_preject);

    if (DEncodeError (&(err_pe), err) != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspErrorRequest: Encoding failed"));
	return (dsapreject (di, DP_INVOKE, id, NULLCP, "Failed to encode operation error"));
    }

    watch_dog ("RoErrorRequest (QSP)");
    result = RoErrorRequest (sd, id, err->dse_type, err_pe, ROS_NOPRIO, roi);
    watch_dog_reset();

    if (result != OK)
    {
	if (ROS_FATAL (rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspErrorRequest(): Fatal rejection"));
	    return (dsaplose (di, DP_INVOKE, NULLCP, "RoErrorRequest failed"));
	}
	else
	{
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("QspErrorRequest(): Non-Fatal rejection"));
	    return (dsapreject (di, DP_INVOKE, id, NULLCP, "RoErrorRequest failed"));
	}
    }

    if (err_pe != NULLPE)
	pe_free (err_pe);

    return (OK);
}

int             DEncodeError(pep, err)
PE		*pep;
struct DSError  *err;
{
    int		success;

    switch(err->dse_type)
    {
	case    DSE_NOERROR :
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("DEncodeError(): no error"));
	    (*pep) = NULLPE;
	    success = NOTOK;
	    break;
	case    DSE_ABANDON_FAILED :
	    success = encode_DAS_AbandonFailedParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_abandon_fail));
	    break;
	case    DSE_ATTRIBUTEERROR :
	    success = encode_DAS_AttributeErrorParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_attribute));
	    break;
	case    DSE_NAMEERROR :
	    success = encode_DAS_NameErrorParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_name));
	    break;
	case    DSE_REFERRAL :
	    success = encode_DAS_ReferralParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_referral));
	    break;
	case    DSE_SECURITYERROR :
	    success = encode_DAS_SecurityErrorParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_security));
	    break;
	case    DSE_SERVICEERROR :
	    success = encode_DAS_ServiceErrorParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_service));
	    break;
	case    DSE_UPDATEERROR :
	    success = encode_DAS_UpdateErrorParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_update));
	    break;
	case    DSE_ABANDONED :
	    (*pep) = NULLPE;
	    success = OK;
	    break;
	case    DSE_DSAREFERRAL :
	    success = encode_DO_DSAReferralParm(pep,1,0,NULLCP,&(err->dse_un.dse_un_referral));
	    break;
	default :
	    (*pep) = NULLPE;
	    success = NOTOK;
	    LLOG(log_dsap, LLOG_EXCEPTIONS, ("DEncodeError(): unknown error %d", err->dse_type));
	    break;
    }

    return(success);
}

